#!/usr/bin/env python3
"""Verify OCI package publication, repository linking metadata, and tamper refusal."""

import hashlib
import http.server
import importlib.util
import json
import tempfile
import threading
import urllib.parse
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
SPEC = importlib.util.spec_from_file_location(
    "publish_packages", ROOT / "scripts" / "publish-packages.py"
)
MOD = importlib.util.module_from_spec(SPEC)
assert SPEC and SPEC.loader
SPEC.loader.exec_module(MOD)

ALLOWED_REPOS = frozenset(
    {"cmzn-consulting/typify"}
    | {f"cmzn-consulting/typify-{spec['platform']}" for spec in MOD.TARGETS}
)


def resolve_allowed_repo(candidate: str) -> str | None:
    for allowed in ALLOWED_REPOS:
        if candidate == allowed:
            return allowed
    return None


class MockOciState:
    def __init__(self):
        self.blobs: dict[tuple[str, str], bytes] = {}
        self.uploads: dict[str, str] = {}
        self.manifests: dict[tuple[str, str], tuple[str, bytes]] = {}
        self.tokens_issued: list[str] = []
        self.counter = 0


def make_handler(state: MockOciState):
    class Handler(http.server.BaseHTTPRequestHandler):
        def log_message(self, fmt, *args):
            return

        def do_GET(self):
            parsed = urllib.parse.urlparse(self.path)
            if parsed.path == "/token":
                auth = self.headers.get("Authorization", "")
                if not auth.startswith("Basic "):
                    self.send_response(401)
                    self.end_headers()
                    return
                qs = urllib.parse.parse_qs(parsed.query)
                scope = qs.get("scope", [""])[0]
                state.tokens_issued.append(scope)
                body = json.dumps({"token": f"tok-{len(state.tokens_issued)}"}).encode(
                    "utf-8"
                )
                self.send_response(200)
                self.send_header("Content-Type", "application/json")
                self.send_header("Content-Length", str(len(body)))
                self.end_headers()
                self.wfile.write(body)
                return

            if parsed.path.startswith("/v2/") and "/manifests/" in parsed.path:
                raw_repo, ref = parsed.path[len("/v2/") :].split("/manifests/", 1)
                repo = resolve_allowed_repo(raw_repo)
                entry = state.manifests.get((repo, ref)) if repo is not None else None
                if not entry:
                    self.send_response(404)
                    self.end_headers()
                    return
                ctype, data = entry
                self.send_response(200)
                self.send_header("Content-Type", ctype)
                self.send_header("Content-Length", str(len(data)))
                self.end_headers()
                self.wfile.write(data)
                return

            self.send_response(404)
            self.end_headers()

        def do_HEAD(self):
            parsed = urllib.parse.urlparse(self.path)
            if parsed.path.startswith("/v2/") and "/blobs/" in parsed.path:
                raw_repo, digest = parsed.path[len("/v2/") :].split("/blobs/", 1)
                repo = resolve_allowed_repo(raw_repo)
                data = state.blobs.get((repo, digest)) if repo is not None else None
                if data is None:
                    self.send_response(404)
                    self.end_headers()
                    return
                self.send_response(200)
                self.send_header("Content-Length", str(len(data)))
                self.end_headers()
                return
            self.send_response(404)
            self.end_headers()

        def do_POST(self):
            parsed = urllib.parse.urlparse(self.path)
            if parsed.path.startswith("/v2/") and parsed.path.endswith(
                "/blobs/uploads/"
            ):
                raw_repo = parsed.path[len("/v2/") : -len("/blobs/uploads/")]
                repo = resolve_allowed_repo(raw_repo)
                if repo is None:
                    self.send_response(404)
                    self.end_headers()
                    return
                state.counter += 1
                uid = f"u{state.counter}"
                state.uploads[uid] = repo
                self.send_response(202)
                self.send_header("Location", f"/v2/{repo}/blobs/uploads/{uid}")
                self.send_header("Content-Length", "0")
                self.end_headers()
                return
            self.send_response(404)
            self.end_headers()

        def do_PUT(self):
            parsed = urllib.parse.urlparse(self.path)
            length = int(self.headers.get("Content-Length", "0"))
            body = self.rfile.read(length) if length > 0 else b""

            if parsed.path.startswith("/v2/") and "/blobs/uploads/" in parsed.path:
                raw_repo, uid = parsed.path[len("/v2/") :].split("/blobs/uploads/", 1)
                repo = resolve_allowed_repo(raw_repo)
                if repo is None or state.uploads.get(uid) != repo:
                    self.send_response(404)
                    self.end_headers()
                    return
                qs = urllib.parse.parse_qs(parsed.query)
                requested_digest = qs.get("digest", [""])[0]
                actual = "sha256:" + hashlib.sha256(body).hexdigest()
                if requested_digest != actual:
                    self.send_response(400)
                    self.end_headers()
                    return
                state.blobs[(repo, actual)] = body
                self.send_response(201)
                self.send_header("Docker-Content-Digest", actual)
                self.send_header("Content-Length", "0")
                self.end_headers()
                return

            if parsed.path.startswith("/v2/") and "/manifests/" in parsed.path:
                raw_repo, ref = parsed.path[len("/v2/") :].split("/manifests/", 1)
                repo = resolve_allowed_repo(raw_repo)
                if repo is None:
                    self.send_response(404)
                    self.end_headers()
                    return
                ctype = self.headers.get("Content-Type", "")
                digest = "sha256:" + hashlib.sha256(body).hexdigest()
                state.manifests[(repo, ref)] = (ctype, body)
                state.manifests[(repo, digest)] = (ctype, body)
                self.send_response(201)
                self.send_header("Docker-Content-Digest", digest)
                self.send_header("Content-Length", "0")
                self.end_headers()
                return

            self.send_response(404)
            self.end_headers()

    return Handler


def populate_mock_builds(builds_dir: Path) -> None:
    for spec in MOD.TARGETS:
        platform = spec["platform"]
        asset = spec["asset"]
        bdir = builds_dir / f"build-{platform}-a"
        bdir.mkdir(parents=True, exist_ok=True)
        archive_bytes = f"archive-payload-{platform}\n".encode("utf-8")
        manifest_bytes = f"{'a' * 64}  0755  typify/bin/typify\n".encode("utf-8")
        sha_hex = hashlib.sha256(archive_bytes).hexdigest()
        (bdir / asset).write_bytes(archive_bytes)
        (bdir / f"{asset}.sha256").write_text(f"{sha_hex}  {asset}\n", encoding="utf-8")
        (bdir / f"{platform}.MANIFEST.txt").write_bytes(manifest_bytes)


def main() -> None:
    state = MockOciState()
    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), make_handler(state))
    port = server.server_address[1]
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    try:
        with tempfile.TemporaryDirectory(prefix="test-publish-packages-") as tmp_str:
            tmp = Path(tmp_str)
            builds = tmp / "builds"
            populate_mock_builds(builds)
            commit = "a" * 40
            receipt = MOD.publish_all(
                builds_dir=builds,
                repository="CMZN-Consulting/typify",
                tag="v1.0.0",
                commit=commit,
                registry_url=f"http://127.0.0.1:{port}",
                username="actor",
                token="secret-token",
            )
            assert receipt["tags"] == ["v1.0.0", "1.0.0", "latest"]
            assert len(receipt["packages"]) == 4

            for spec in MOD.TARGETS:
                pkg_repo = f"cmzn-consulting/typify-{spec['platform']}"
                for tag in ("v1.0.0", "1.0.0", "latest"):
                    ctype, raw = state.manifests[(pkg_repo, tag)]
                    assert ctype == MOD.OCI_MANIFEST_MEDIA_TYPE
                    doc = json.loads(raw.decode("utf-8"))
                    assert (
                        doc["annotations"]["org.opencontainers.image.source"]
                        == "https://github.com/CMZN-Consulting/typify"
                    )
                    assert (
                        doc["annotations"]["org.opencontainers.image.url"]
                        == "https://typify.science"
                    )
                    titles = [
                        layer["annotations"]["org.opencontainers.image.title"]
                        for layer in doc["layers"]
                    ]
                    assert titles[0] == spec["asset"]
                    assert titles[1] == f"{spec['platform']}.MANIFEST.txt"
                    cfg_raw = state.blobs[(pkg_repo, doc["config"]["digest"])]
                    cfg = json.loads(cfg_raw.decode("utf-8"))
                    assert (
                        cfg["config"]["Labels"]["org.opencontainers.image.source"]
                        == "https://github.com/CMZN-Consulting/typify"
                    )

            idx_type, idx_raw = state.manifests[("cmzn-consulting/typify", "v1.0.0")]
            assert idx_type == MOD.OCI_INDEX_MEDIA_TYPE
            idx_doc = json.loads(idx_raw.decode("utf-8"))
            assert (
                idx_doc["annotations"]["org.opencontainers.image.source"]
                == "https://github.com/CMZN-Consulting/typify"
            )
            assert len(idx_doc["manifests"]) == 4

            # Verify tampered archive byte is refused before publishing.
            tampered_path = (
                builds / "build-linux-x86_64-a" / "typify-linux-x86_64.tar.gz"
            )
            tampered_path.write_bytes(b"corrupted-bytes")
            refused = False
            try:
                MOD.publish_all(
                    builds_dir=builds,
                    repository="CMZN-Consulting/typify",
                    tag="v1.0.0",
                    commit=commit,
                    registry_url=f"http://127.0.0.1:{port}",
                    username="actor",
                    token="secret-token",
                )
            except ValueError:
                refused = True
            assert refused, "Expected tampered archive to be refused"
    finally:
        server.shutdown()
        server.server_close()

    print(
        "pass  publish-packages verified across 4 platform packages, multi-platform index, and tamper refusal"
    )


if __name__ == "__main__":
    main()
