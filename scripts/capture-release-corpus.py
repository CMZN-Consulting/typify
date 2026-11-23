import argparse
import base64
import contextlib
import hashlib
import json
import platform
import re
import shutil
import subprocess
import sys
import tarfile
import tempfile
import unicodedata
from pathlib import Path, PureWindowsPath


V12_RECEIPT = "140bad4af9ad2261569c9018b94f40f95a1638bc9df333750561767087f2b6fe"
TARGETS = ("linux-x86_64", "linux-aarch64", "macos-aarch64", "windows-x86_64")


def json_document(raw):
    def unique(pairs):
        result = dict(pairs)
        if len(result) != len(pairs):
            raise ValueError("duplicate member in frozen JSON")
        return result

    def invalid_number(value):
        raise ValueError(f"non-JSON number {value}")

    return json.loads(raw.decode("utf-8"), object_pairs_hook=unique, parse_constant=invalid_number)


def digest(data):
    return hashlib.sha256(data).hexdigest()


def write_transcript(output, raw):
    handle = tempfile.NamedTemporaryFile(dir=output.parent, delete=False)
    temporary = Path(handle.name)
    try:
        with handle:
            handle.write(raw)
        temporary.replace(output)
    finally:
        temporary.unlink(missing_ok=True)


def native_target():
    system = {"linux": "linux", "darwin": "macos", "windows": "windows"}.get(platform.system().lower())
    machine = {"x86_64": "x86_64", "amd64": "x86_64", "aarch64": "aarch64", "arm64": "aarch64"}.get(
        platform.machine().lower())
    return f"{system}-{machine}"


def safe_relative(name, *, allow_root=False):
    if name == "." and allow_root:
        return
    if not isinstance(name, str) or not name or PureWindowsPath(name).drive:
        raise ValueError("expected a relative fixture path")
    for part in name.split("/"):
        if (not part or part in {".", ".."} or part.endswith((" ", "."))
                or re.search(r'[\x00-\x1f\\:<>"|?*]', part)
                or re.fullmatch(r"(CON|PRN|AUX|NUL|CONIN\$|CONOUT\$|(?:COM|LPT)[1-9¹²³])(?:\..*)?", part, re.I)):
            raise ValueError("unsafe or nonportable fixture path")


def release_manifest(raw, pin):
    data = json_document(raw)
    if not isinstance(data, dict):
        raise ValueError("invalid extension manifest")
    keys = {"archiveSha256", "ids", "doubleFailureId"}
    if set(data) not in (keys, keys | {"invocationsSha256"}):
        raise ValueError("unexpected extension manifest fields")
    canonical = json.dumps(data, sort_keys=True, ensure_ascii=False, separators=(",", ":"), allow_nan=False)
    if digest(canonical.encode("utf-8")) != pin:
        raise ValueError("extension manifest pin mismatch")
    for key in keys | {"invocationsSha256"}:
        if key.endswith("Sha256") and key in data:
            if not isinstance(data[key], str) or not re.fullmatch("[0-9a-f]{64}", data[key]):
                raise ValueError("invalid immutable input digest")
    ids = data["ids"]
    if (not isinstance(ids, list) or not ids or not all(isinstance(i, str) and i for i in ids)
            or len(set(ids)) != len(ids) or data["doubleFailureId"] not in ids):
        raise ValueError("invalid extension invocation inventory")
    return data


@contextlib.contextmanager
def unpack_extension(archive_path, root, pin):
    with archive_path.open("rb") as archive_file:
        sha = hashlib.sha256()
        for block in iter(lambda: archive_file.read(1024 * 1024), b""):
            sha.update(block)
        if sha.hexdigest() != pin:
            raise ValueError("extension archive pin mismatch")
        archive_file.seek(0)
        with tarfile.open(fileobj=archive_file, mode="r:*") as archive:
            paths = {}
            files = []
            members = set()
            for member in archive:
                name = member.name.removeprefix("./")
                if name in {"", "."} and member.isdir():
                    continue
                safe_relative(name)
                if not (member.isdir() or member.isfile()) or member.issparse():
                    raise ValueError("extension accepts ordinary files and directories only")
                pieces = name.split("/")
                for i in range(1, len(pieces) + 1):
                    part = "/".join(pieces[:i])
                    identity = (part, i < len(pieces) or member.isdir())
                    portable_name = unicodedata.normalize("NFC", part.casefold())
                    if paths.setdefault(portable_name, identity) != identity:
                        raise ValueError("conflicting file or directory in extension")
                if name in members:
                    raise ValueError("duplicate extension member")
                members.add(name)
                if member.isfile():
                    files.append((name, member))
            root.mkdir()
            try:
                for name, directory in paths.values():
                    if directory:
                        (root / name).mkdir(parents=True, exist_ok=True)
                for name, member in files:
                    with archive.extractfile(member) as source, (root / name).open("xb") as target:
                        shutil.copyfileobj(source, target)
                yield root
            finally:
                shutil.rmtree(root)


def extension_runs(index_bytes, manifest):
    index = json_document(index_bytes)
    if (not isinstance(index, dict) or set(index) not in ({"schema", "runs"}, {"schema", "runs", "doubleFailureId"})
            or index["schema"] != "typify-stage4-invocations-v1" or not isinstance(index["runs"], list)):
        raise ValueError("invalid extension invocation index")
    if "doubleFailureId" in index and index["doubleFailureId"] != manifest["doubleFailureId"]:
        raise ValueError("double-failure invocation differs from the freeze")
    for run in index["runs"]:
        if not isinstance(run, dict) or set(run) != {"id", "cwd", "argv", "exit"}:
            raise ValueError("invalid extension invocation entry")
        safe_relative(run["cwd"], allow_root=True)
        args = run["argv"]
        if (not isinstance(args, list) or len(args) < 2 or args[0] != "typify"
                or not all(isinstance(a, str) and "\x00" not in a for a in args)
                or type(run["exit"]) is not int or run["exit"] not in (0, 1, 2)):
            raise ValueError("invalid native invocation or expected exit")
        for arg in args[1:]:
            if (PureWindowsPath(arg).drive or arg.startswith("/") or "\\" in arg
                    or ".." in arg.split("/")):
                raise ValueError("extension operand leaves portable fixture scope")
    if [r["id"] for r in index["runs"]] != manifest["ids"]:
        raise ValueError("extension invocation order or inventory differs from the freeze")
    return index["runs"]


def run_reader(executable, directory, name, argv, expected_exit):
    result = subprocess.run(
        [executable, *argv[1:]], cwd=directory, stdin=subprocess.DEVNULL,
        stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=120,
    )
    if result.returncode != expected_exit:
        raise ValueError(f"{name} returned {result.returncode}, expected {expected_exit}")
    if "--json" in argv:
        json_document(result.stdout)
    return {
        "id": name, "exit": result.returncode,
        "stdout": base64.b64encode(result.stdout).decode("ascii"),
        "stderr": base64.b64encode(result.stderr).decode("ascii"),
    }


def verify_corpus(root):
    raw = (root / "import.json").read_bytes()
    if hashlib.sha256(raw).hexdigest() != V12_RECEIPT:
        raise ValueError("invalid v12 corpus receipt")
    receipt = json.loads(raw)
    expected_files = {row["path"]: row for row in receipt["files"]}
    generated = {"import.json", "hostile.json"}
    directories = set()
    files = set()
    for entry in root.rglob("*"):
        name = entry.relative_to(root).as_posix()
        if entry.is_symlink():
            raise ValueError(f"unexpected link in corpus: {name}")
        if entry.is_dir():
            directories.add(name)
        elif entry.is_file():
            files.add(name)
        else:
            raise ValueError(f"unexpected entry in corpus: {name}")
    if directories != set(receipt["directories"]) or files != set(expected_files) | generated:
        raise ValueError("corpus file or directory inventory differs")
    for name, row in expected_files.items():
        data = (root / name).read_bytes()
        if len(data) != row["bytes"] or hashlib.sha256(data).hexdigest() != row["sha256"]:
            raise ValueError(f"corpus input differs: {name}")
    return receipt["cases"]


def capture(executable, root):
    observations = []
    for case in verify_corpus(root):
        directory = root / case
        spec = json.loads((directory / "expected.json").read_bytes())
        invocations = ([spec] if "invocation" in spec else []) + spec.get("subcases", [])
        for invocation in invocations:
            name = case if invocation is spec else case + "/" + invocation["name"]
            argv = invocation["invocation"]
            if argv[0] != "typify":
                raise ValueError(f"unexpected executable in {name}")
            observations.append(run_reader(executable, directory, name, argv, invocation["exit_code"]))
    return observations


def capture_extension(executable, archive, root, manifest, index_path, baseline_ids):
    if set(baseline_ids).intersection(manifest["ids"]):
        raise ValueError("extension reuses a baseline invocation ID")
    if (index_path is not None) != ("invocationsSha256" in manifest):
        raise ValueError("a sidecar invocation index requires its own manifest digest")
    with unpack_extension(archive, root, manifest["archiveSha256"]):
        raw = (index_path or root / "capture.json").read_bytes()
        if index_path and digest(raw) != manifest["invocationsSha256"]:
            raise ValueError("extension invocation sidecar pin mismatch")
        runs = extension_runs(raw, manifest)
        for run in runs:
            if not (root / run["cwd"]).is_dir():
                raise ValueError("extension invocation directory is absent")
        return [run_reader(executable, root / r["cwd"], r["id"], r["argv"], r["exit"]) for r in runs]


def main():
    parser = argparse.ArgumentParser(
        description="Capture exact stdout and stderr for each released corpus invocation."
    )
    parser.add_argument("executable", type=Path)
    parser.add_argument("corpus", type=Path)
    parser.add_argument("output", type=Path)
    parser.add_argument("--extension-archive", type=Path)
    parser.add_argument("--extension-root", type=Path)
    parser.add_argument("--extension-manifest", type=Path)
    parser.add_argument("--extension-manifest-sha256")
    parser.add_argument("--extension-invocations", type=Path)
    parser.add_argument("--target", choices=TARGETS)
    args = parser.parse_args()
    extension = (args.extension_archive, args.extension_root, args.extension_manifest, args.extension_manifest_sha256)
    if (any(extension) or args.extension_invocations or args.target) and not all(extension):
        parser.error("native extension capture requires archive, extraction root, manifest and manifest pin")
    if args.target and native_target() != args.target:
        parser.error("the requested target differs from this host's platform and architecture")
    try:
        root = args.corpus.resolve(strict=True)
        output = args.output.resolve()
        executable = args.executable.resolve(strict=True)
        if output == root or root in output.parents or output == executable:
            parser.error("choose an output file outside the corpus and executable")
        if all(extension):
            extension_root = args.extension_root.absolute()
            if (extension_root.exists() or extension_root.is_symlink() or root in extension_root.resolve().parents
                    or output == extension_root.resolve() or extension_root.resolve() in output.parents):
                parser.error("choose a fresh extension root disjoint from the corpus and output")
            inputs = [args.extension_archive, args.extension_manifest, args.extension_invocations]
            if output in {p.resolve() for p in inputs if p is not None}:
                parser.error("the transcript output must differ from all frozen inputs")
            manifest = release_manifest(args.extension_manifest.read_bytes(), args.extension_manifest_sha256)
        rows = capture(executable, root)
        baseline_count = len(rows)
        if all(extension):
            rows.extend(capture_extension(executable, args.extension_archive, extension_root,
                        manifest, args.extension_invocations, [row["id"] for row in rows]))
        raw = (json.dumps(rows, separators=(",", ":"), ensure_ascii=True) + "\n").encode("ascii")
        write_transcript(output, raw)
    except (OSError, EOFError, ValueError, tarfile.TarError, subprocess.TimeoutExpired) as error:
        print(error, file=sys.stderr)
        return 1
    if args.target:
        print(json.dumps({"target": args.target, "v12Invocations": baseline_count,
                          "extensionInvocations": len(rows) - baseline_count, "transcriptSha256": digest(raw)}))
    else:
        print(f"{len(rows)} invocations, sha256 {digest(raw)}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
