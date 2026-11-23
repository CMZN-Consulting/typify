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


def summarize(directory, commit):
    results = []
    versions = set()
    common_transcripts = set()
    for target in TARGETS:
        source = directory / ("verify-" + target)
        for replica in ("a", "b"):
            def record(kind):
                return read_json(source / f"{kind}-{replica}.json")

            build = record("build")
            capture = record("target")
            semantic = record("semantic")
            verification = record("verify")
            version = (source / f"version-{replica}.txt").read_text().strip()
            transcript = (source / f"transcript-{replica}.json").read_bytes()
            digest = hashlib.sha256(transcript).hexdigest()
            counts = (capture["v12Invocations"], capture["extensionInvocations"])
            require(re.fullmatch(r"1\.0\.0 sha256:[0-9a-f]{64}", version), f"{target}: reader build syntax")
            require(commit == build["source"]["commit"] and target == build["platform"] == capture["target"],
                    f"{target}: checkout or native target differs")
            require(digest == capture["transcriptSha256"] and all(type(n) is int and n > 0 for n in counts)
                    and len(json.loads(transcript)) == sum(counts), f"{target}: transcript or counts differ")
            require(semantic["complete"] and semantic["independent"] and semantic["total"] == counts[0]
                    and semantic["failures"] == semantic["infrastructure-failures"] == 0, f"{target}: v12 replay")
            require(all(verification[k] == "ok" for k in ("archive", "tree", "architecture"))
                    and verification["archive_sha256"] == build["archive_sha256"]
                    and verification["manifest_sha256"] == build["manifest_sha256"]
                    and verification["executables"] == build["architecture"]["executables"] and build["architecture"]["pass"]
                    and record("license")["pass"], f"{target}: archive eligibility")
            versions.add(version)
            common_transcripts.add(digest)
            results.append({"target": target, "replica": replica, "readerVersion": version, "capture": capture,
                            "asset": build["asset"], "archiveSha256": build["archive_sha256"],
                            "manifestSha256": build["manifest_sha256"], "source": build["source"],
                            "installer": build["racket"], "executables": build["architecture"]["executables"],
                            "v12Passed": semantic["total"], "extension": record("extension")})
    require(len(versions) == len(common_transcripts) == 1, "the native reader builds or transcripts disagree")
    return {"commit": commit, "readerVersion": versions.pop(), "transcriptSha256": common_transcripts.pop(),
            "native": results}


if __name__ == "__main__":
    inputs, head, output = sys.argv[1:]
    Path(output).write_text(json.dumps(summarize(Path(inputs), head), indent=2, sort_keys=True) + "\n",
                            encoding="utf-8")
