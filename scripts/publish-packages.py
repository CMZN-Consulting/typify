#!/usr/bin/env python3
"""Publish verified Typify platform distributions as OCI packages on GitHub Packages (ghcr.io)."""
import argparse
import base64
import hashlib
import json
import os
import re
import sys
import urllib.error
import urllib.parse
import urllib.request
from pathlib import Path

TARGETS = (
    {
        "platform": "linux-x86_64",
        "asset": "typify-linux-x86_64.tar.gz",
        "mediaType": "application/gzip",
        "os": "linux",
        "architecture": "amd64",
        "title": "Typify Linux x86_64 binary distribution",
    },
    {
        "platform": "linux-aarch64",
        "asset": "typify-linux-aarch64.tar.gz",
        "mediaType": "application/gzip",
        "os": "linux",
        "architecture": "arm64",
        "title": "Typify Linux ARM64 binary distribution",
    },
    {
        "platform": "macos-aarch64",
        "asset": "typify-macos-aarch64.tar.gz",
        "mediaType": "application/gzip",
        "os": "darwin",
        "architecture": "arm64",
        "title": "Typify macOS ARM64 binary distribution",
    },
    {
        "platform": "windows-x86_64",
        "asset": "typify-windows-x86_64.zip",
        "mediaType": "application/zip",
        "os": "windows",
        "architecture": "amd64",
        "title": "Typify Windows x86_64 binary distribution",
    },
)

OCI_CONFIG_MEDIA_TYPE = "application/vnd.oci.image.config.v1+json"
OCI_MANIFEST_MEDIA_TYPE = "application/vnd.oci.image.manifest.v1+json"
OCI_INDEX_MEDIA_TYPE = "application/vnd.oci.image.index.v1+json"


def sha256_hex(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def canonical_json(obj: object) -> bytes:
    return json.dumps(obj, sort_keys=True, separators=(",", ":")).encode("utf-8")


def parse_sha256_file(path: Path, expected_asset: str) -> str:
    text = path.read_text(encoding="utf-8").strip()
    parts = text.split()
    if len(parts) < 2 or not re.fullmatch(r"[0-9a-f]{64}", parts[0]):
        raise ValueError(f"Malformed sha256 file: {path}")
    asset_name = parts[-1].lstrip("*")
    if asset_name != expected_asset:
        raise ValueError(f"Asset mismatch in {path}: got {asset_name}, expected {expected_asset}")
    return parts[0]


class OciRegistryClient:
    def __init__(self, registry_url: str, username: str, token: str):
        self.registry_url = registry_url.rstrip("/")
        self.username = username
        self.token = token
        self._bearer_cache: dict[str, str] = {}

    def _basic_header(self) -> str:
        raw = f"{self.username}:{self.token}".encode("utf-8")
        return "Basic " + base64.b64encode(raw).decode("ascii")

    def _get_bearer(self, repo_name: str) -> str:
        if repo_name in self._bearer_cache:
            return self._bearer_cache[repo_name]
        parsed = urllib.parse.urlparse(self.registry_url)
        service = parsed.netloc
        query = urllib.parse.urlencode(
            {
                "service": service,
                "scope": f"repository:{repo_name}:pull,push",
            }
        )
        token_url = f"{self.registry_url}/token?{query}"
        req = urllib.request.Request(
            token_url,
            headers={
                "Authorization": self._basic_header(),
                "Accept": "application/json",
            },
            method="GET",
        )
        with urllib.request.urlopen(req, timeout=60) as resp:
            doc = json.loads(resp.read().decode("utf-8"))
        tok = doc.get("token") or doc.get("access_token")
        if not tok:
            raise RuntimeError(f"Registry token response missing token for {repo_name}")
        self._bearer_cache[repo_name] = tok
        return tok

    def _request(
        self,
        repo_name: str,
        method: str,
        url_or_path: str,
        *,
        data: bytes | None = None,
        headers: dict[str, str] | None = None,
        expected_statuses: tuple[int, ...] = (200,),
    ) -> tuple[int, dict[str, str], bytes]:
        bearer = self._get_bearer(repo_name)
        url = (
            url_or_path
            if url_or_path.startswith("http://") or url_or_path.startswith("https://")
            else f"{self.registry_url}{url_or_path}"
        )
        hdrs = {"Authorization": f"Bearer {bearer}"}
        if headers:
            hdrs.update(headers)
        req = urllib.request.Request(url, data=data, headers=hdrs, method=method)
        try:
            with urllib.request.urlopen(req, timeout=180) as resp:
                status = resp.status
                resp_headers = {k.lower(): v for k, v in resp.headers.items()}
                body = resp.read()
        except urllib.error.HTTPError as exc:
            status = exc.code
            resp_headers = {k.lower(): v for k, v in exc.headers.items()}
            body = exc.read()
            if status not in expected_statuses:
                detail = body.decode("utf-8", errors="replace")[:500]
                raise RuntimeError(f"OCI {method} {url} failed with HTTP {status}: {detail}") from exc
        if status not in expected_statuses:
            raise RuntimeError(f"OCI {method} {url} unexpected status {status}")
        return status, resp_headers, body

    def blob_exists(self, repo_name: str, digest: str) -> bool:
        status, _, _ = self._request(
            repo_name,
            "HEAD",
            f"/v2/{repo_name}/blobs/{digest}",
            expected_statuses=(200, 404),
        )
        return status == 200

    def upload_blob(self, repo_name: str, content: bytes) -> tuple[str, int]:
        digest = f"sha256:{sha256_hex(content)}"
        size = len(content)
        if self.blob_exists(repo_name, digest):
            return digest, size
        _, hdrs, _ = self._request(
            repo_name,
            "POST",
            f"/v2/{repo_name}/blobs/uploads/",
            data=b"",
            headers={"Content-Length": "0"},
            expected_statuses=(202,),
        )
        location = hdrs.get("location")
        if not location:
            raise RuntimeError(f"Missing Location header starting blob upload for {repo_name}")
        sep = "&" if "?" in location else "?"
        put_target = f"{location}{sep}digest={urllib.parse.quote(digest, safe=':')}"
        self._request(
            repo_name,
            "PUT",
            put_target,
            data=content,
            headers={
                "Content-Type": "application/octet-stream",
                "Content-Length": str(size),
            },
            expected_statuses=(201,),
        )
        if not self.blob_exists(repo_name, digest):
            raise RuntimeError(f"Uploaded blob {digest} not found on HEAD verification in {repo_name}")
        return digest, size

    def put_manifest(self, repo_name: str, reference: str, media_type: str, manifest_bytes: bytes) -> str:
        expected_digest = f"sha256:{sha256_hex(manifest_bytes)}"
        self._request(
            repo_name,
            "PUT",
            f"/v2/{repo_name}/manifests/{reference}",
            data=manifest_bytes,
            headers={
                "Content-Type": media_type,
                "Content-Length": str(len(manifest_bytes)),
            },
            expected_statuses=(201, 200),
        )
        _, _, fetched = self._request(
            repo_name,
            "GET",
            f"/v2/{repo_name}/manifests/{reference}",
            headers={"Accept": media_type},
            expected_statuses=(200,),
        )
        fetched_digest = f"sha256:{sha256_hex(fetched)}"
        if fetched_digest != expected_digest:
            raise RuntimeError(
                f"Manifest digest mismatch on {repo_name}:{reference}: got {fetched_digest}, expected {expected_digest}"
            )
        return expected_digest


def build_tags(tag: str) -> list[str]:
    tags = [tag]
    if tag.startswith("v") and re.fullmatch(r"v[0-9]+\.[0-9]+\.[0-9]+", tag):
        semver = tag[1:]
        tags.append(semver)
        tags.append("latest")
    return tags


def publish_all(
    builds_dir: Path,
    repository: str,
    tag: str,
    commit: str,
    registry_url: str,
    username: str,
    token: str,
    homepage: str = "https://typify.science",
) -> dict:
    if "/" not in repository:
        raise ValueError(f"Expected OWNER/REPO, got {repository}")
    if not re.fullmatch(r"[0-9a-f]{40}", commit):
        raise ValueError(f"Expected 40-hex commit SHA, got {commit}")

    owner, repo_short = repository.split("/", 1)
    namespace = owner.lower()
    base_pkg = f"{namespace}/{repo_short.lower()}"
    source_url = f"https://github.com/{repository}"
    version = tag[1:] if tag.startswith("v") else tag
    tags = build_tags(tag)
    registry_host = urllib.parse.urlparse(registry_url).netloc

    client = OciRegistryClient(registry_url=registry_url, username=username, token=token)

    index_descriptors = []
    published_packages = []

    for spec in TARGETS:
        platform = spec["platform"]
        asset_name = spec["asset"]
        bdir = builds_dir / f"build-{platform}-a"
        archive_path = bdir / asset_name
        sha_path = bdir / f"{asset_name}.sha256"
        manifest_txt_name = f"{platform}.MANIFEST.txt"
        manifest_txt_path = bdir / manifest_txt_name

        if not archive_path.is_file() or not sha_path.is_file() or not manifest_txt_path.is_file():
            raise FileNotFoundError(f"Missing verified build inputs in {bdir}")

        archive_bytes = archive_path.read_bytes()
        expected_sha = parse_sha256_file(sha_path, asset_name)
        actual_sha = sha256_hex(archive_bytes)
        if actual_sha != expected_sha:
            raise ValueError(
                f"Archive digest mismatch for {asset_name}: file {actual_sha} != recorded {expected_sha}"
            )

        sha_bytes = sha_path.read_bytes()
        manifest_txt_bytes = manifest_txt_path.read_bytes()
        manifest_txt_sha = sha256_hex(manifest_txt_bytes)

        labels = {
            "org.opencontainers.image.title": spec["title"],
            "org.opencontainers.image.description": (
                f"{spec['title']} ({asset_name}) and verified file manifest for {tag}."
            ),
            "org.opencontainers.image.source": source_url,
            "org.opencontainers.image.url": homepage,
            "org.opencontainers.image.version": version,
            "org.opencontainers.image.revision": commit,
            "org.opencontainers.image.licenses": "MIT",
        }
        config_obj = {
            "architecture": spec["architecture"],
            "os": spec["os"],
            "config": {"Labels": labels},
            "rootfs": {"type": "layers", "diff_ids": []},
            "history": [],
        }
        config_bytes = canonical_json(config_obj)

        pkg_repo = f"{base_pkg}-{platform}"

        for target_repo in (pkg_repo, base_pkg):
            cfg_digest, cfg_size = client.upload_blob(target_repo, config_bytes)
            arc_digest, arc_size = client.upload_blob(target_repo, archive_bytes)
            sha_digest, sha_size = client.upload_blob(target_repo, sha_bytes)
            man_digest, man_size = client.upload_blob(target_repo, manifest_txt_bytes)

        manifest_obj = {
            "schemaVersion": 2,
            "mediaType": OCI_MANIFEST_MEDIA_TYPE,
            "config": {
                "mediaType": OCI_CONFIG_MEDIA_TYPE,
                "digest": cfg_digest,
                "size": cfg_size,
            },
            "layers": [
                {
                    "mediaType": spec["mediaType"],
                    "digest": arc_digest,
                    "size": arc_size,
                    "annotations": {"org.opencontainers.image.title": asset_name},
                },
                {
                    "mediaType": "text/plain; charset=utf-8",
                    "digest": man_digest,
                    "size": man_size,
                    "annotations": {"org.opencontainers.image.title": manifest_txt_name},
                },
                {
                    "mediaType": "text/plain; charset=utf-8",
                    "digest": sha_digest,
                    "size": sha_size,
                    "annotations": {"org.opencontainers.image.title": f"{asset_name}.sha256"},
                },
            ],
            "annotations": dict(labels),
        }
        manifest_bytes = canonical_json(manifest_obj)

        manifest_digest = ""
        for t in tags:
            manifest_digest = client.put_manifest(pkg_repo, t, OCI_MANIFEST_MEDIA_TYPE, manifest_bytes)

        client.put_manifest(base_pkg, manifest_digest, OCI_MANIFEST_MEDIA_TYPE, manifest_bytes)

        index_descriptors.append(
            {
                "mediaType": OCI_MANIFEST_MEDIA_TYPE,
                "digest": manifest_digest,
                "size": len(manifest_bytes),
                "platform": {
                    "os": spec["os"],
                    "architecture": spec["architecture"],
                },
                "annotations": {
                    "org.opencontainers.image.title": spec["title"],
                    "science.typify.platform": platform,
                    "science.typify.asset": asset_name,
                },
            }
        )

        published_packages.append(
            {
                "package": f"{registry_host}/{pkg_repo}",
                "platform": platform,
                "asset": asset_name,
                "archiveSha256": actual_sha,
                "manifestSha256": manifest_txt_sha,
                "ociManifestDigest": manifest_digest,
                "tags": list(tags),
            }
        )

    index_annotations = {
        "org.opencontainers.image.title": "Typify",
        "org.opencontainers.image.description": (
            "Multi-platform OCI distribution index for Typify standalone native binaries."
        ),
        "org.opencontainers.image.source": source_url,
        "org.opencontainers.image.url": homepage,
        "org.opencontainers.image.version": version,
        "org.opencontainers.image.revision": commit,
        "org.opencontainers.image.licenses": "MIT",
    }
    index_obj = {
        "schemaVersion": 2,
        "mediaType": OCI_INDEX_MEDIA_TYPE,
        "manifests": index_descriptors,
        "annotations": index_annotations,
    }
    index_bytes = canonical_json(index_obj)
    index_digest = ""
    for t in tags:
        index_digest = client.put_manifest(base_pkg, t, OCI_INDEX_MEDIA_TYPE, index_bytes)

    return {
        "schema": "typify-release-packages-v1",
        "repository": repository,
        "tag": tag,
        "commit": commit,
        "indexPackage": f"{registry_host}/{base_pkg}",
        "indexDigest": index_digest,
        "tags": list(tags),
        "packages": published_packages,
    }


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--builds", type=Path, required=True, help="Directory holding build-<platform>-a folders")
    ap.add_argument("--repository", required=True, help="GitHub OWNER/REPO")
    ap.add_argument("--tag", required=True, help="Release tag, e.g. v1.0.0")
    ap.add_argument("--commit", required=True, help="Exact 40-hex commit SHA")
    ap.add_argument("--registry-url", default="https://ghcr.io")
    ap.add_argument("--homepage", default="https://typify.science")
    ap.add_argument("--output", type=Path, required=True)
    args = ap.parse_args()

    username = os.environ.get("GHCR_USERNAME") or os.environ.get("GITHUB_ACTOR") or "x-access-token"
    token = os.environ.get("GHCR_TOKEN") or os.environ.get("GITHUB_TOKEN") or os.environ.get("GH_TOKEN")
    if not token:
        print("Missing GHCR_TOKEN / GITHUB_TOKEN for package publication", file=sys.stderr)
        return 1

    receipt = publish_all(
        builds_dir=args.builds,
        repository=args.repository,
        tag=args.tag,
        commit=args.commit,
        registry_url=args.registry_url,
        username=username,
        token=token,
        homepage=args.homepage,
    )
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(receipt, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
