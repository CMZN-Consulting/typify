"""Check the eight native build receipts against the captured bytes at one commit."""

import hashlib
import json
import re
import sys
from pathlib import Path


TARGETS = ("linux-x86_64", "linux-aarch64", "macos-aarch64", "windows-x86_64")


def require(condition, description):
    if not condition:
        raise ValueError(description)


def read_json(path):
    return json.loads(path.read_bytes())


def check_extension(extension, version, count, frozen):
    require(
        extension["schema"] == "typify-stage4-extension-result-v1"
        and extension["pass"] is True
        and extension["readerBuild"] == version.split(" ", 1)[1]
        and extension["archiveSha256"] == frozen["archiveSha256"]
        and all(
            type(extension[key]) is int
            for key in ("invocations", "failures", "differences")
        )
        and extension["invocations"] == count == len(frozen["ids"])
        and extension["failures"] == extension["differences"] == 0
        and re.fullmatch("[0-9a-f]{64}", extension["reportSha256"]),
        "the independent extension comparison did not pass for this reader and frozen corpus",
    )


def check_foreign_headers(report, target, foreign_build):
    expected = foreign_build["architecture"]["executables"]
    actual = report.get("executables")
    require(
        foreign_build["platform"] != target
        and foreign_build["architecture"]["pass"] is True
        and report.get("platform") == target
        and report.get("pass") is False
        and isinstance(actual, list)
        and expected
        and all(row["native"] is True for row in expected)
        and actual == [dict(row, native=False) for row in expected],
        f"{target}: wrong-platform control did not identify the foreign executables",
    )


def summarize(directory, commit, frozen):
    results = []
    versions = set()
    common_transcripts = set()
    for target in TARGETS:
        source = directory / ("verify-" + target)
        for foreign in TARGETS:
            if foreign != target:
                build = read_json(directory / ("verify-" + foreign) / "build-a.json")
                require(
                    build["platform"] == foreign
                    and build["source"]["commit"] == commit,
                    "foreign architecture reference belongs to a different platform or build",
                )
                check_foreign_headers(
                    read_json(source / f"foreign-{foreign}.json"), target, build
                )
        for replica in ("a", "b"):

            def record(kind):
                return read_json(source / f"{kind}-{replica}.json")

            build = record("build")
            capture = record("target")
            semantic = record("semantic")
            verification = record("verify")
            extension = record("extension")
            native_inspect = record("native-inspect")
            native_license = record("native-license")
            version = (source / f"version-{replica}.txt").read_text().strip()
            transcript = (source / f"transcript-{replica}.json").read_bytes()
            digest = hashlib.sha256(transcript).hexdigest()
            counts = (capture["v12Invocations"], capture["extensionInvocations"])
            require(
                re.fullmatch(r"1\.0\.0 sha256:[0-9a-f]{64}", version),
                f"{target}: reader build syntax",
            )
            require(
                commit == build["source"]["commit"]
                and target == build["platform"] == capture["target"],
                f"{target}: checkout or native target differs",
            )
            rows = json.loads(transcript)
            require(
                digest == capture["transcriptSha256"]
                and all(type(n) is int and n > 0 for n in counts)
                and len(rows) == sum(counts),
                f"{target}: transcript or counts differ",
            )
            require(
                [row["id"] for row in rows[counts[0] :]] == frozen["ids"],
                f"{target}: extension transcript differs from the frozen invocation order",
            )
            check_extension(extension, version, counts[1], frozen)
            require(
                semantic["complete"]
                and semantic["independent"]
                and semantic["total"] == counts[0]
                and semantic["failures"] == semantic["infrastructure-failures"] == 0,
                f"{target}: v12 replay",
            )
            require(
                all(
                    verification[k] == "ok" for k in ("archive", "tree", "architecture")
                )
                and verification["archive_sha256"] == build["archive_sha256"]
                and verification["manifest_sha256"] == build["manifest_sha256"]
                and verification["executables"] == build["architecture"]["executables"]
                and build["architecture"]["pass"]
                and record("license")["pass"],
                f"{target}: archive eligibility",
            )
            require(
                native_inspect["asset"]["name"] == build["asset"]
                and native_inspect["asset"]["sha256"] == build["archive_sha256"]
                and native_inspect["runtimePaths"] == {}
                and native_license["pass"] is True,
                f"{target}: native archive content or license inventory mismatch",
            )
            versions.add(version)
            common_transcripts.add(digest)
            results.append(
                {
                    "target": target,
                    "replica": replica,
                    "readerVersion": version,
                    "capture": capture,
                    "asset": build["asset"],
                    "archiveSha256": build["archive_sha256"],
                    "manifestSha256": build["manifest_sha256"],
                    "source": build["source"],
                    "installer": build["racket"],
                    "executables": build["architecture"]["executables"],
                    "nativeContent": {
                        "shipped": native_inspect["shipped"],
                        "embedded": native_inspect["embedded"],
                        "linked": native_inspect["linked"],
                        "runtimePaths": native_inspect["runtimePaths"],
                    },
                    "v12Passed": semantic["total"],
                    "extension": extension,
                }
            )
    require(
        len(versions) == len(common_transcripts) == 1,
        "the native reader builds or transcripts disagree",
    )
    return {
        "commit": commit,
        "readerVersion": versions.pop(),
        "transcriptSha256": common_transcripts.pop(),
        "native": results,
    }


if __name__ == "__main__":
    inputs, head, output = sys.argv[1:]
    fixtures = Path(__file__).resolve().parents[1] / "test" / "release-fixtures"
    raw = (fixtures / "stage4-manifest.json").read_bytes()
    expected = (fixtures / "stage4-manifest.sha256").read_text(encoding="ascii").strip()
    require(
        hashlib.sha256(raw).hexdigest() == expected, "the frozen manifest pin differs"
    )
    Path(output).write_text(
        json.dumps(
            summarize(Path(inputs), head, json.loads(raw)), indent=2, sort_keys=True
        )
        + "\n",
        encoding="utf-8",
    )
