"""Check independently frozen Stage 4 expectations against native reader output."""

import argparse
import base64
import hashlib
import json
import os
import runpy
import subprocess
import sys
import tarfile
import tempfile
import urllib.parse
from pathlib import Path

CAPTURE = runpy.run_path(str(Path(__file__).with_name("capture-release-corpus.py")))
READER_REVISION = "e7adaaf04b490ded03a6aab4aae5d3aafbb6d52b"
READER_BUILD = "sha256:1c084bfd9da45bf19aa474d953f01f2182580d4d186fd4382f7159298a13b04a"
ARCHIVE_SHA256 = "ecbbfa7fc5a53be40c0b82a15f7fd23fb83be7bd420724fd5c9b2e44c2d98e76"
MANIFEST_SHA256 = "816b1a48a59f6700503488ac8ae8eac193dc97b395db3ab23cde6f690b3a934c"
TREE_SHA256 = "e72fcb35971504350c1d5180faa3b4be055712083b2e8d838d317988254ce57f"
MISSING = object()


def child_path(parent, key):
    return parent + "/" + str(key).replace("~", "~0").replace("/", "~1")


def differences(expected, observed, path="", *, object_subset=False):
    if type(expected) is dict and type(observed) is dict:
        keys = set(expected) if object_subset else set(expected).union(observed)
        for key in sorted(keys):
            yield from differences(
                expected.get(key, MISSING),
                observed.get(key, MISSING),
                child_path(path, key),
                object_subset=object_subset,
            )
    elif type(expected) is list and type(observed) is list:
        for i in range(max(len(expected), len(observed))):
            left = expected[i] if i < len(expected) else MISSING
            right = observed[i] if i < len(observed) else MISSING
            yield from differences(
                left, right, child_path(path, i), object_subset=object_subset
            )
    elif type(expected) is not type(observed) or expected != observed:
        delta = {"path": path}
        if expected is not MISSING:
            delta["expected"] = expected
        if observed is not MISSING:
            delta["observed"] = observed
        yield delta


def native_run(executable, argv, directory):
    result = subprocess.run(
        [executable, *argv],
        cwd=directory,
        stdin=subprocess.DEVNULL,
        capture_output=True,
        check=False,
        timeout=120,
    )
    return {
        "exit": result.returncode,
        "stdout": base64.b64encode(result.stdout).decode("ascii"),
        "stderr": base64.b64encode(result.stderr).decode("ascii"),
    }


def reader_identity(executable, directory):
    result = native_run(executable, ["--version", "--json"], directory)
    actual = CAPTURE["json_document"](base64.b64decode(result["stdout"], validate=True))
    if result["exit"] != 0 or actual != {
        "version": "1.0.0",
        "build": READER_BUILD,
        "diagnostics": [],
    }:
        raise ValueError(
            "reader version or source build differs from the approved candidate"
        )
    return actual


def save_report(destination, reader, observations, archive_digest, tree_digest):
    total = len(observations)
    failed = sum(bool(row["differences"]) for row in observations)
    count = sum(len(row["differences"]) for row in observations)
    summary = {
        "schema": "typify-stage4-extension-result-v1",
        "acceptanceReaderRevision": READER_REVISION,
        "readerBuild": reader["build"],
        "archiveSha256": archive_digest,
        "treeSha256": tree_digest,
        "invocations": total,
        "failures": failed,
        "differences": count,
        "pass": total > 0 and failed == 0,
    }
    primary = [
        sum(not d["path"].startswith("/projections/") for d in r["differences"])
        for r in observations
    ]
    summary["primary"] = {
        "invocations": total,
        "failedInvocations": sum(n > 0 for n in primary),
        "differences": sum(primary),
    }
    summary["projections"] = {}
    for fmt in ("lsp", "sarif"):
        per_run = [
            sum(
                d["path"].startswith("/projections/" + fmt + "/")
                for d in r["differences"]
            )
            for r in observations
            if fmt in r.get("projections", {})
        ]
        summary["projections"][fmt] = {
            "invocations": len(per_run),
            "failedInvocations": sum(n > 0 for n in per_run),
            "differences": sum(per_run),
        }
    raw = (
        json.dumps(
            dict(summary, results=observations),
            ensure_ascii=True,
            sort_keys=True,
            separators=(",", ":"),
        )
        + "\n"
    ).encode("ascii")
    CAPTURE["write_transcript"](destination, raw)
    return dict(summary, reportSha256=hashlib.sha256(raw).hexdigest())


def require(ok, reason):
    if not ok:
        raise ValueError(reason)


def read_json(path):
    return CAPTURE["json_document"](path.read_bytes())


def compact(obj):
    return json.dumps(
        obj, sort_keys=True, separators=(",", ":"), ensure_ascii=False, allow_nan=False
    ).encode("utf-8")


def inventory(root):
    files = sorted(
        (p.relative_to(root).as_posix(), CAPTURE["digest"](p.read_bytes()))
        for p in root.rglob("*")
        if p.is_file()
    )
    require(
        len(files) == 75
        and CAPTURE["digest"](b"".join(compact(pair) + b"\n" for pair in files))
        == TREE_SHA256,
        "independent extension tree digest or file inventory differs",
    )
    index = read_json(root / "capture.json")
    require(
        isinstance(index, dict) and set(index) == {"schema", "doubleFailureId", "runs"},
        "unexpected index shape",
    )
    ids = [r["id"] for r in index["runs"]]
    manifest = {
        "archiveSha256": ARCHIVE_SHA256,
        "ids": ids,
        "doubleFailureId": index["doubleFailureId"],
    }
    CAPTURE["release_manifest"](compact(manifest), MANIFEST_SHA256)
    # The index is a self-contained archive member. The capture manifest binds its distinguished ID separately.
    runs = CAPTURE["extension_runs"](compact(index), manifest)
    expect = {}
    case_count = 0
    for directory in sorted(p for p in root.iterdir() if p.is_dir()):
        case = read_json(directory / "expected.json")
        require(
            case["case_id"] == directory.name, "expected file names a different case"
        )
        case_count += 1
        specs = ([case] if "invocation" in case else []) + case.get("subcases", [])
        require(bool(specs), "case has no invocations")
        for spec in specs:
            ident = (
                case["case_id"]
                if spec is case
                else case["case_id"] + "/" + spec["name"]
            )
            require(ident not in expect, "duplicate expected invocation")
            require(
                set(spec)
                <= {
                    "case_id",
                    "title",
                    "sections_covered",
                    "provenance",
                    "command",
                    "name",
                    "invocation",
                    "exit_code",
                    "verdict",
                    "stdout_json",
                    "diagnostics",
                    "sarif_projection",
                    "sarif_projections",
                    "lsp_range_utf16_zero_based",
                    "lsp_ranges_utf16_zero_based",
                },
                "unknown frozen expectation or metadata member",
            )
            require(
                spec["verdict"]
                == {0: "accepted", 1: "refused", 2: "usage_or_io_error"}[
                    spec["exit_code"]
                ],
                "frozen verdict and exit conflict",
            )
            require(
                spec["diagnostics"] == spec["stdout_json"]["diagnostics"],
                "two frozen native diagnostic claims disagree",
            )
            expect[ident] = spec
    require(
        case_count == 18 and len(runs) == 33 and set(ids) == set(expect),
        "registered case and expectation inventories differ",
    )
    for r in runs:
        s = expect[r["id"]]
        require(
            r["cwd"] == r["id"].split("/")[0]
            and r["argv"] == s["invocation"]
            and r["exit"] == s["exit_code"],
            "frozen invocation index and expected command, directory, or exit differ",
        )
    return runs, expect


def read_transcript(path, runs):
    rows = read_json(path)
    require(
        isinstance(rows, list) and [r["id"] for r in rows] == [r["id"] for r in runs],
        "supplied extension transcript has different IDs or order",
    )
    for r in rows:
        require(
            set(r) == {"id", "exit", "stdout", "stderr"} and type(r["exit"]) is int,
            "invalid primary stream record",
        )
        for key in ("stdout", "stderr"):
            require(
                isinstance(r[key], str)
                and base64.b64encode(base64.b64decode(r[key], validate=True)).decode(
                    "ascii"
                )
                == r[key],
                "native stream is not canonical base64",
            )
    return {r["id"]: r for r in rows}


def fixture_root(directory, argv):
    require(
        argv[1] in ("plan", "tree") and len(argv) >= 5 and argv[4].startswith("--"),
        "fixture marker occurs outside a native root command",
    )
    operand = argv[3]
    CAPTURE["safe_relative"](operand, allow_root=True)
    path = (directory / operand).resolve().as_posix()
    if os.name == "nt":
        if path.startswith("//"):
            return "/Volumes/UNC/" + path[2:]
        require(len(path) > 2 and path[1:3] == ":/", "unexpected native Windows path")
        return "/Volumes/" + path[0].upper() + path[2:]
    return path


def assert_diagnostic(expected, actual, path):
    if not isinstance(actual, dict):
        return list(differences(expected, actual, path))
    # The authored provenance cites sources for the expectation. Additional runtime envelope members
    # (message, fingerprints, fix and unasserted related items) are outside this diagnostic projection.
    result = []
    for key, value in expected.items():
        if key == "provenance":
            continue
        if key == "data" and isinstance(actual.get(key), dict):
            for field, wanted in value.items():
                result.extend(
                    differences(
                        wanted,
                        actual[key].get(field, MISSING),
                        child_path(child_path(path, key), field),
                    )
                )
        else:
            result.extend(
                differences(value, actual.get(key, MISSING), child_path(path, key))
            )
    return result


def primary_differences(spec, native, directory):
    result = list(differences(spec["exit_code"], native["exit"], "/exit_code"))
    try:
        actual = CAPTURE["json_document"](
            base64.b64decode(native["stdout"], validate=True)
        )
    except (ValueError, UnicodeError):
        return result + [{"path": "/stdout_json", "reason": "invalid-json"}]
    # Everything except the explicitly scoped diagnostic envelopes is an exact JSON assertion.
    want = dict(spec["stdout_json"])
    if not isinstance(actual, dict):
        return result + list(differences(want, actual, "/stdout_json"))
    if isinstance(want.get("inputs"), dict) and isinstance(
        want["inputs"].get("root"), dict
    ):
        require(
            want["inputs"]["root"] == {"$fixtureRoot": "root"},
            "invalid fixture-root expectation",
        )
        want["inputs"] = {
            **want["inputs"],
            "root": fixture_root(directory, spec["invocation"]),
        }
    diagnostics = want.pop("diagnostics")
    observed = dict(actual)
    observed_diagnostics = observed.pop("diagnostics", MISSING)
    result.extend(differences(want, observed, "/stdout_json"))
    if not isinstance(observed_diagnostics, list):
        result.extend(
            differences(diagnostics, observed_diagnostics, "/stdout_json/diagnostics")
        )
    else:
        for i in range(max(len(diagnostics), len(observed_diagnostics))):
            p = child_path("/stdout_json/diagnostics", i)
            if i >= len(diagnostics) or i >= len(observed_diagnostics):
                result.extend(
                    differences(
                        diagnostics[i] if i < len(diagnostics) else MISSING,
                        observed_diagnostics[i]
                        if i < len(observed_diagnostics)
                        else MISSING,
                        p,
                    )
                )
            else:
                result.extend(
                    assert_diagnostic(diagnostics[i], observed_diagnostics[i], p)
                )
    return result


def projection_expectations(spec):
    for fmt, singular, plural in (
        ("lsp", "lsp_range_utf16_zero_based", "lsp_ranges_utf16_zero_based"),
        ("sarif", "sarif_projection", "sarif_projections"),
    ):
        if singular in spec or plural in spec:
            require(
                not (singular in spec and plural in spec),
                "duplicate projection assertion forms",
            )
            entries = [spec[singular]] if singular in spec else spec[plural]
            require(
                isinstance(entries, list) and len(entries) == len(spec["diagnostics"]),
                "projection assertion count differs",
            )
            yield fmt, entries


def expected_projection(fmt, claims, diagnostics, directory):
    if fmt == "lsp":
        result = {}
        for claim, diag in zip(claims, diagnostics):
            uri = (directory / diag["file"]).resolve().as_uri()
            result.setdefault(uri, []).append({"code": diag["code"], "range": claim})
        return result
    results = []
    for claim, diag in zip(claims, diagnostics):
        start, end = claim["region"]["start"], claim["region"]["end"]
        region = {
            "startLine": start["line"],
            "startColumn": start["column"],
            "endLine": end["line"],
            "endColumn": end["column"],
            "byteOffset": start["byteOffset"],
            "byteLength": end["byteOffset"] - start["byteOffset"],
        }
        results.append(
            {
                "ruleId": claim["ruleId"],
                "level": claim["level"],
                "properties": claim["properties"],
                "locations": [
                    {
                        "physicalLocation": {
                            "artifactLocation": {
                                "uri": urllib.parse.quote(diag["file"], safe="/")
                            },
                            "region": region,
                        }
                    }
                ],
            }
        )
    return results


def normalized_projection(fmt, doc):
    if fmt == "sarif":
        require(
            isinstance(doc.get("runs"), list) and len(doc["runs"]) == 1,
            "invalid SARIF run inventory",
        )
        rows = doc["runs"][0]["results"]
        require(isinstance(rows, list), "invalid SARIF results")
        return rows
    # Match relative filenames using the native document URI; an unexpected file cannot lose its diagnostics.
    require(doc["unlocated"] == [], "unexpected unlocated LSP diagnostic")
    result = {}
    seen = set()
    for x in doc["documents"]:
        uri = x["uri"]
        require(
            isinstance(uri, str)
            and uri not in seen
            and isinstance(x["diagnostics"], list),
            "duplicate or malformed LSP document",
        )
        seen.add(uri)
        if x["diagnostics"]:
            result[uri] = [
                {"code": d["code"], "range": d["range"]} for d in x["diagnostics"]
            ]
    return result


def projection_differences(fmt, claims, spec, native, directory):
    p = child_path("/projections", fmt)
    result = list(differences(spec["exit_code"], native["exit"], p + "/exit_code"))
    try:
        raw = CAPTURE["json_document"](
            base64.b64decode(native["stdout"], validate=True)
        )
        actual = normalized_projection(fmt, raw)
        expected = expected_projection(fmt, claims, spec["diagnostics"], directory)
        result.extend(
            differences(expected, actual, p + "/output", object_subset=(fmt == "sarif"))
        )
    except (ValueError, KeyError, TypeError, AttributeError, IndexError) as error:
        result.append(
            {
                "path": p + "/output",
                "reason": "invalid-projection",
                "detail": str(error),
            }
        )
    return result


def execute_bound(executable, argv, directory):
    try:
        return native_run(executable, argv, directory)
    except subprocess.TimeoutExpired as error:
        return {
            "exit": None,
            "outcome": "timeout",
            "stdout": base64.b64encode(error.stdout or b"").decode("ascii"),
            "stderr": base64.b64encode(error.stderr or b"").decode("ascii"),
        }
    except OSError as error:
        return {
            "exit": None,
            "outcome": "launch-error",
            "detail": str(error),
            "stdout": "",
            "stderr": "",
        }


def measure(executable, root, runs, specs, primary, primary_root):
    rows = []
    for run in runs:
        ident = run["id"]
        directory = root / run["cwd"]
        spec = specs[ident]
        argv = run["argv"][1:]
        raw = (
            primary[ident]
            if primary is not None
            else execute_bound(executable, argv, directory)
        )
        delta = primary_differences(spec, raw, primary_root / run["cwd"])
        projected = {}
        for fmt, claims in projection_expectations(spec):
            require(argv.count("--json") == 1, "projection needs one JSON flag")
            operation = ["--" + fmt if a == "--json" else a for a in argv]
            projection = execute_bound(executable, operation, directory)
            projected[fmt] = projection
            delta.extend(
                projection_differences(fmt, claims, spec, projection, directory)
            )
        rows.append(
            {
                "id": ident,
                "argv": run["argv"],
                "cwd": run["cwd"],
                "expectedSha256": CAPTURE["digest"](compact(spec)),
                **raw,
                "projections": projected,
                "differences": delta,
            }
        )
    return rows


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("executable", type=Path)
    parser.add_argument("extension_archive", type=Path)
    parser.add_argument("private_report", type=Path)
    parser.add_argument(
        "--transcript",
        type=Path,
        help="previously captured primary extension invocations in frozen order",
    )
    parser.add_argument(
        "--root",
        type=Path,
        help="original absolute extraction directory used to capture that transcript",
    )
    args = parser.parse_args()
    if (args.transcript is None) != (args.root is None) or (
        args.root is not None and not args.root.is_absolute()
    ):
        parser.error("provide both --transcript and its absolute --root")
    if args.private_report.resolve() in {
        x.resolve()
        for x in (args.executable, args.extension_archive, args.transcript)
        if x
    }:
        parser.error(
            "private report must differ from frozen inputs, executable and primary transcript"
        )
    try:
        exe = args.executable.resolve(strict=True)
        with tempfile.TemporaryDirectory(prefix="typify-extension-") as tmp:
            fixture = Path(tmp).resolve(strict=True) / "fixtures"
            with CAPTURE["unpack_extension"](
                args.extension_archive, fixture, ARCHIVE_SHA256
            ):
                runs, specs = inventory(fixture)
                primary = (
                    read_transcript(args.transcript, runs) if args.transcript else None
                )
                reader = reader_identity(exe, fixture)
                rows = measure(
                    exe,
                    fixture,
                    runs,
                    specs,
                    primary,
                    args.root.resolve() if args.root else fixture,
                )
                summary = save_report(
                    args.private_report, reader, rows, ARCHIVE_SHA256, TREE_SHA256
                )
        print(json.dumps(summary, sort_keys=True))
        return 0 if summary["pass"] else 1
    except (
        OSError,
        EOFError,
        ValueError,
        TypeError,
        KeyError,
        IndexError,
        tarfile.TarError,
        subprocess.TimeoutExpired,
    ) as error:
        print("extension measurement failed: " + str(error), file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
