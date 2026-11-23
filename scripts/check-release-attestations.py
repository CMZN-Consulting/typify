"""List and verify the exact subjects of one hosted release build."""

import argparse
import hashlib
import json
import subprocess
from pathlib import Path


PLATFORMS = ("linux-x86_64", "linux-aarch64", "macos-aarch64", "windows-x86_64")
CONTROLS = ("flipped-byte", "extra-file", "missing-file", "rewritten-manifest")


def files_exactly(directory, names):
    entries = {p.name: p for p in directory.iterdir()}
    if set(entries) != set(names):
        raise ValueError(f"{directory}: missing {sorted(set(names) - entries.keys())}, "
                         f"unexpected {sorted(entries.keys() - set(names))}")
    for path in entries.values():
        if path.is_symlink() or not path.is_file() or not path.stat().st_size:
            raise ValueError(f"expected a nonempty regular file at {path}")
    return entries


def file_hash(path):
    h = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            h.update(block)
    return h.hexdigest()


def inventory(builds, verified=None):
    subjects = {}
    negatives = []
    for platform in PLATFORMS:
        ext = ".zip" if platform.startswith("windows-") else ".tar.gz"
        asset = "typify-" + platform + ext
        manifest = platform + ".MANIFEST.txt"
        rows = files_exactly(builds / ("build-" + platform + "-a"),
                             [asset, asset + ".sha256", manifest, platform + ".build.json"])
        subjects[asset] = rows[asset]
        subjects[asset + "/typify/MANIFEST.txt"] = rows[manifest]
        if verified is not None:
            names = [name + "-" + asset for name in CONTROLS]
            bad = files_exactly(verified / ("verify-" + platform) / "canaries", names)
            negatives.extend(bad[name] for name in names)
    return subjects, negatives


def check(builds, verified, bundle, repo, ref, commit):
    subjects, negatives = inventory(builds, verified)
    digests = {name: file_hash(path) for name, path in subjects.items()}
    if any(file_hash(path) in digests.values() for path in negatives):
        raise ValueError("a negative control has the digest of a genuine subject")
    policy = ["--repo", repo, "--signer-workflow", repo + "/.github/workflows/release.yml",
              "--source-ref", ref, "--source-digest", commit, "--deny-self-hosted-runners"]
    for path in subjects.values():
        output = subprocess.check_output(["gh", "attestation", "verify", path, *policy, "--format", "json"])
        verified_results = json.loads(output)
        if not any(subjects_match(row["verificationResult"]["statement"]["subject"], digests)
                   for row in verified_results):
            raise ValueError(f"{path.name}: no verified statement names all eight exact subjects")
    for path in negatives:
        result = subprocess.run(["gh", "attestation", "verify", path, *policy, "--bundle", bundle],
                                stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
        if result.returncode != 1:
            raise ValueError(f"{path.name}: expected digest refusal from the signed bundle, got {result.returncode}")
    # A genuine member verifies with this same local bundle and policy.
    subprocess.run(["gh", "attestation", "verify", next(iter(subjects.values())), *policy, "--bundle", bundle],
                   check=True)
    print(f"Verified {len(subjects)} signed subjects and rejected {len(negatives)} altered archives")


def subjects_match(rows, wanted):
    return len(rows) == len(wanted) and all(
        type(row) is dict and row.get("digest") == {"sha256": wanted.get(row.get("name"))}
        for row in rows
    ) and {row["name"] for row in rows} == set(wanted)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="operation", required=True)
    subjects = commands.add_parser("subjects")
    subjects.add_argument("builds", type=Path)
    verify = commands.add_parser("verify")
    for name in ("builds", "verified", "bundle"):
        verify.add_argument(name, type=Path)
    for name in ("repo", "ref", "commit"):
        verify.add_argument(name)
    args = parser.parse_args()
    if args.operation == "subjects":
        for name, path in inventory(args.builds)[0].items():
            print(file_hash(path) + "  " + name)
    else:
        check(args.builds, args.verified, args.bundle, args.repo, args.ref, args.commit)
