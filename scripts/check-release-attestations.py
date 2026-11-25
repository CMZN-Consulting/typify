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
        raise ValueError(
            f"{directory}: missing {sorted(set(names) - entries.keys())}, "
            f"unexpected {sorted(entries.keys() - set(names))}"
        )
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
        rows = files_exactly(
            builds / ("build-" + platform + "-a"),
            [asset, asset + ".sha256", manifest, platform + ".build.json"],
        )
        subjects[asset] = rows[asset]
        subjects[asset + "/typify/MANIFEST.txt"] = rows[manifest]
        if verified is not None:
            names = [name + "-" + asset for name in CONTROLS]
            bad = files_exactly(verified / ("verify-" + platform) / "canaries", names)
            negatives.extend((asset, bad[name]) for name in names)
    return subjects, negatives


def verified_subjects(name, path, policy, expected):
    output = subprocess.check_output(
        ["gh", "attestation", "verify", path, *policy, "--format", "json"]
    )
    for result in json.loads(output):
        rows = result["verificationResult"]["statement"]["subject"]
        if subjects_match(rows, expected):
            signed = {row["name"]: row["digest"]["sha256"] for row in rows}
            if signed[name] == file_hash(path):
                return signed
    raise ValueError(
        f"{name}: no verified statement binds this file and all eight exact subjects"
    )


def verify_control(path, signed, policy):
    if file_hash(path) in signed.values():
        raise ValueError(f"{path.name}: control has a genuine signed digest")
    result = subprocess.run(
        ["gh", "attestation", "verify", path, *policy], capture_output=True, text=True
    )
    # gh discards the underlying Sigstore error. Its issuer-stage refusal corroborates
    # the digest mismatch established above using the authenticated local statement.
    # Unknown CLI diagnostics must stop the check, even if the exit status is 1.
    issuer_errors = {
        'Error: verifying with issuer "sigstore.dev"',
        'Error: verifying with issuer "GitHub, Inc."',
    }
    if (
        result.returncode != 1
        or result.stdout.strip()
        or result.stderr.strip() not in issuer_errors
    ):
        raise ValueError(
            f"{path.name}: unexpected verification outcome (exit {result.returncode}): "
            f"{result.stderr.strip()}"
        )


def check(builds, verified, bundle, repo, ref, commit):
    subjects, negatives = inventory(builds, verified)
    digests = {name: file_hash(path) for name, path in subjects.items()}
    policy = [
        "--repo",
        repo,
        "--signer-workflow",
        repo + "/.github/workflows/release.yml",
        "--source-ref",
        ref,
        "--source-digest",
        commit,
        "--deny-self-hosted-runners",
    ]
    for name, path in subjects.items():
        verified_subjects(name, path, policy, digests)
    local_policy = [*policy, "--bundle", bundle]
    signed_by_archive = {}
    for asset, control in negatives:
        if asset not in signed_by_archive:
            signed_by_archive[asset] = verified_subjects(
                asset, subjects[asset], local_policy, digests
            )
        verify_control(control, signed_by_archive[asset], local_policy)
    print(
        f"Verified {len(subjects)} signed subjects and rejected {len(negatives)} altered archives"
    )


def subjects_match(rows, wanted):
    return (
        len(rows) == len(wanted)
        and all(
            type(row) is dict
            and row.get("digest") == {"sha256": wanted.get(row.get("name"))}
            for row in rows
        )
        and {row["name"] for row in rows} == set(wanted)
    )


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
