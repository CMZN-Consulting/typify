"""Build a disposable, native Typify reader and publish its measurements to the job summary."""

import hashlib
import json
import os
import platform
import shutil
import struct
import subprocess
import sys
import tempfile
from pathlib import Path


SOURCE_TREE = "dc8d7113c0e75c1517f59f42507191dffd0e0c85"
READER_VERSION = (
    "1.0.1 sha256:a2882c2c2c436752117e33e1708ce6677f3ef5441e2601d4aa8f9d8109fc3436"
)
TARGETS = {
    "ubuntu-22.04": (
        "Linux",
        "x86_64",
        "racket-9.3-x86_64-linux-buster-cs.sh",
        272724156,
        "30dc59d9b5af083eacf793253d35782c5231e8bfb7a7331fe32ccd9a41fd792f",
    ),
    "ubuntu-22.04-arm": (
        "Linux",
        "aarch64",
        "racket-9.3-aarch64-linux-buster-cs.sh",
        264306763,
        "26e4d1c3bf8954c4413fb6737af5d211d44c1e158e34e4c90e0af1cde6353c9a",
    ),
    "macos-15": (
        "Darwin",
        "aarch64",
        "racket-9.3-aarch64-macosx-cs.dmg",
        278994444,
        "21f4937ca0bf37c31e60c1d77f5fd20f175e3ad51f6197f27149055bc222e559",
    ),
    "windows-2022": (
        "Windows",
        "x86_64",
        "racket-9.3-x86_64-win32-cs.exe",
        194479288,
        "5bea9560f8bc6c9d31d55b52a2f83fb5947e57cf625e1865e6091a428c13fe60",
    ),
}

DECLARATION = """#lang typify
declare : Probe
cell : entries = shadow
primitive : put : canonical-path
writes : put : entries = put 1 "stored"
ensure : put : jointly = (and (bound entries /left) (bound entries /right))
form : put : <canonical-path>
does : put <canonical-path>
end!
"""


def require(condition, message):
    if not condition:
        raise RuntimeError(message)


def sha256(path):
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def output(command, *, cwd=None, expected=0):
    result = subprocess.run(
        command, cwd=cwd, capture_output=True, text=True, encoding="utf-8"
    )
    require(
        result.returncode == expected,
        f"{command!r} exited {result.returncode}, expected {expected}\n{result.stderr}\n{result.stdout}",
    )
    return result.stdout.strip()


def install(root, package, expected_size, expected_sha, receipt):
    source = root / package
    subprocess.run(
        [
            "curl.exe" if os.name == "nt" else "curl",
            "--fail",
            "--silent",
            "--show-error",
            "--location",
            "--retry",
            "3",
            "--proto",
            "=https",
            "--proto-redir",
            "=https",
            "-o",
            source,
            "https://mirror.racket-lang.org/installers/9.3/" + package,
        ],
        check=True,
    )
    receipt["installer_bytes"] = source.stat().st_size
    receipt["installer_sha256"] = sha256(source)
    require(
        (receipt["installer_bytes"], receipt["installer_sha256"])
        == (expected_size, expected_sha),
        f"Installer verification failed for {package}",
    )
    dest = root / "racket"
    if sys.platform == "win32":
        env = {**os.environ, "PROBE_INSTALLER": str(source), "PROBE_DEST": str(dest)}
        subprocess.run(
            [
                "powershell",
                "-NoProfile",
                "-NonInteractive",
                "-Command",
                "$p = Start-Process -FilePath $env:PROBE_INSTALLER -ArgumentList "
                '("/S /D=" + $env:PROBE_DEST) -Wait -PassThru -ErrorAction Stop; exit $p.ExitCode',
            ],
            env=env,
            check=True,
        )
        return dest / "Racket.exe"
    if sys.platform == "darwin":
        mount = root / "dmg"
        mount.mkdir()
        subprocess.run(
            [
                "hdiutil",
                "attach",
                "-readonly",
                "-nobrowse",
                "-mountpoint",
                mount,
                source,
            ],
            check=True,
        )
        try:
            distributions = [
                p
                for p in (mount, *mount.iterdir())
                if not p.is_symlink()
                and (p / "bin/racket").is_file()
                and (p / "bin/raco").is_file()
            ]
            require(
                len(distributions) == 1,
                f"Expected one Racket distribution on disk image: {distributions}",
            )
            subprocess.run(["ditto", distributions[0], dest], check=True)
        finally:
            subprocess.run(["hdiutil", "detach", mount], check=True)
    else:
        subprocess.run(
            ["sh", source, "--create-dir", "--in-place", "--dest", dest], check=True
        )
    return dest / "bin/racket"


def executable_machine(binary, os_name):
    if os_name == "Darwin":
        return output(["lipo", "-archs", binary]).split()
    with binary.open("rb") as stream:
        header = stream.read(64)
        if os_name == "Linux":
            require(
                header[:6] == b"\x7fELF\x02\x01",
                "Expected a little-endian ELF64 executable",
            )
            return [
                {62: "x86_64", 183: "aarch64"}[struct.unpack_from("<H", header, 18)[0]]
            ]
        require(header[:2] == b"MZ", "Missing DOS header")
        stream.seek(struct.unpack_from("<I", header, 60)[0])
        require(stream.read(4) == b"PE\0\0", "Missing PE signature")
        machine = struct.unpack("<H", stream.read(2))[0]
        return [{0x8664: "x86_64", 0xAA64: "aarch64"}[machine]]


def smoke(binary, root):
    fixtures = root / "consumer"
    fixtures.mkdir()
    (fixtures / "decl.typify").write_bytes(DECLARATION.encode())
    version = output([binary, "--version"], cwd=fixtures)
    require(version == READER_VERSION, f"Unexpected reader version/build: {version}")
    ask = json.loads(
        output([binary, "ask", "decl.typify", "Probe", "put", "--json"], cwd=fixtures)
    )
    require(
        ask["status"] == "declared"
        and ask["word"] == "put"
        and "? : form : put : <canonical-path>" in ask["lines"],
        f"Bad lookup: {ask}",
    )
    measured = []
    for name, paths, expected_key in [
        ("first", ["/other"], "/left"),
        ("second", ["/left"], "/right"),
        ("both", ["/left", "/right"], None),
    ]:
        (fixtures / "program.typify").write_bytes(
            "".join(f"put : {path}\n" for path in paths).encode()
        )
        args = ["decl.typify", "Probe", "program.typify", "--json"]
        checked = json.loads(output([binary, "check", *args], cwd=fixtures))
        require(
            checked["ok"] and checked["terms"] == [["put", path] for path in paths],
            f"Bad check: {checked}",
        )
        status = 1 if expected_key else 0
        plan = json.loads(
            output([binary, "plan", *args], cwd=fixtures, expected=status)
        )
        diagnostics = [(d["code"], d["data"]["key"]) for d in plan["diagnostics"]]
        require(
            diagnostics == ([("E_ENSURE", [expected_key])] if expected_key else []),
            f"{name} diagnostics: {diagnostics}",
        )
        require(
            [row["ok"] for row in plan["ensure"]]
            == [expected_key is None] * len(paths),
            f"{name} invariant outcomes: {plan['ensure']}",
        )
        measured.append({"case": name, "plan_exit": status, "diagnostics": diagnostics})
    return {"version": version, "ask": ask["status"], "ensures": measured}


def build_and_run(root, racket, source, os_name, machine, receipt):
    vm = output(
        [racket, "-e", "(write (list (version) (system-type 'vm) (system-type 'arch)))"]
    )
    receipt["racket"] = vm
    require(
        vm == f'("9.3" chez-scheme {machine})', f"Wrong native Racket runtime: {vm}"
    )
    stage = root / "source"
    shutil.copytree(source, stage, ignore=shutil.ignore_patterns("compiled"))
    name = "typify.exe" if os_name == "Windows" else "typify"
    raw = root / name
    driver = [racket, "-l", "raco", "--"]
    subprocess.run([*driver, "exe", "-o", raw, stage / "main.rkt"], check=True)
    packed = root / "dist"
    subprocess.run([*driver, "distribute", packed, raw], check=True)
    shutil.rmtree(stage)
    raw.unlink()
    deployed = root / "relocated"
    packed.rename(deployed)
    binaries = [p for p in deployed.rglob(name) if p.is_file()]
    require(len(binaries) == 1, f"Expected one installed reader, found {binaries}")
    binary = binaries[0]
    arches = executable_machine(binary, os_name)
    receipt.update(
        binary_arch=arches,
        executable_sha256=sha256(binary),
        executable=binary.relative_to(deployed).as_posix(),
    )
    require(
        ("arm64" if os_name == "Darwin" else machine) in arches,
        f"Wrong reader architecture: {arches}",
    )
    receipt["smoke"] = smoke(binary, root)


def main():
    host = sys.argv[1]
    os_name, machine, package, size, digest = TARGETS[host]
    root = Path(__file__).resolve().parents[2]
    receipt = {
        "host": host,
        "os": platform.platform(),
        "machine": platform.machine(),
        "installer": package,
        "checkout": os.environ.get("GITHUB_SHA"),
        "status": "failed",
    }
    try:
        require(
            platform.system() == os_name,
            f"Unexpected operating system: {platform.system()}",
        )
        actual_machine = {"amd64": "x86_64", "arm64": "aarch64"}.get(
            platform.machine().lower(), platform.machine().lower()
        )
        require(
            actual_machine == machine, f"Unexpected runner machine: {actual_machine}"
        )
        receipt["source_tree"] = output(["git", "rev-parse", "HEAD:src"], cwd=root)
        require(
            receipt["source_tree"] == SOURCE_TREE,
            "Probe's source tree differs from the reviewed reader",
        )
        with tempfile.TemporaryDirectory(dir=os.environ["RUNNER_TEMP"]) as directory:
            temp = Path(directory)
            racket = install(temp, package, size, digest, receipt)
            build_and_run(temp, racket, root / "src", os_name, machine, receipt)
        receipt["status"] = "passed"
    finally:
        evidence = json.dumps(receipt, indent=2)
        print(evidence, flush=True)
        with open(os.environ["GITHUB_STEP_SUMMARY"], "a", encoding="utf-8") as summary:
            summary.write(
                f"## Native reader probe on {host}\n\n```json\n{evidence}\n```\n"
            )


if __name__ == "__main__":
    main()
