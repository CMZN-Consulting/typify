"""Compare Stage 4 native diagnostics with actual SARIF and LSP CLI output."""

import argparse
import collections
import hashlib
import importlib.metadata
import json
import runpy
import subprocess
import sys
import tempfile
from pathlib import Path
from urllib.parse import unquote, urljoin

CAPTURE = runpy.run_path(str(Path(__file__).with_name("capture-release-corpus.py")))
SARIF_SHA256 = "ad6db49878699b091f3eeb765b6e29e92a34bad4da88664d000c923b549c3a25"
NOTICES = r"""#lang typify
declare : Notices
is : empty = /\x00/
cell : entries = shadow
primitive : wipe
writes : wipe : entries = delete 0
form : wipe!
does : wipe
end!
"""


def native_position(source, pos):
    """Compute UTF-16 from source bytes; native columns count Unicode code points."""
    line, col = pos["line"] - 1, pos["column"] - 1
    lines = source.split(b"\n")
    if not (0 <= line < len(lines)):
        raise ValueError(f"native line outside source: {pos}")
    text = lines[line].removesuffix(b"\r").decode("utf-8")
    if not (0 <= col <= len(text)):
        raise ValueError(f"native column outside source: {pos}")
    before = text[:col]
    offset = sum(len(row) + 1 for row in lines[:line]) + len(before.encode("utf-8"))
    if offset != pos["byteOffset"]:
        raise ValueError(
            f"native byte offset {pos['byteOffset']} disagrees with source offset {offset}"
        )
    units = len(before.encode("utf-16-le")) // 2
    return {"line": line, "character": units}, units != col


def expected_ranges(span, file):
    if span is None:
        return None, None, False
    start, end = span["start"], span["end"]
    region = {
        "startLine": start["line"],
        "startColumn": start["column"],
        "endLine": end["line"],
        "endColumn": end["column"],
        "byteOffset": start["byteOffset"],
        "byteLength": end["byteOffset"] - start["byteOffset"],
    }
    if region["byteLength"] < 0:
        raise ValueError("reversed native span")
    # Absent tree entries and planner-generated sources can carry the initial empty range.
    if file.is_file():
        source = file.read_bytes()
    elif (
        region["startLine"],
        region["startColumn"],
        region["endLine"],
        region["endColumn"],
        region["byteOffset"],
        region["byteLength"],
    ) == (1, 1, 1, 1, 0, 0):
        source = b""
    else:
        raise ValueError(f"no source available to check range for {file.name}")
    left, left_non_bmp = native_position(source, start)
    right, right_non_bmp = native_position(source, end)
    return region, {"start": left, "end": right}, left_non_bmp or right_non_bmp


def require(condition, explanation):
    if not condition:
        raise ValueError(explanation)


class SchemaValidationError(ValueError):
    pass


def schema_validator(path):
    raw = path.read_bytes()
    require(
        hashlib.sha256(raw).hexdigest() == SARIF_SHA256,
        "official SARIF schema SHA-256 differs",
    )
    try:
        import fastjsonschema
    except ImportError as exc:
        raise ValueError(
            "install fastjsonschema==2.21.2 in the measurement environment"
        ) from exc
    require(
        importlib.metadata.version("fastjsonschema") == "2.21.2",
        "requires fastjsonschema==2.21.2",
    )
    compiled = fastjsonschema.compile(
        json.loads(raw), use_default=False, use_formats=True
    )

    def validate(document):
        try:
            compiled(document)
        except fastjsonschema.JsonSchemaException as exc:
            raise SchemaValidationError(str(exc)) from exc

    return validate


class Measurements:
    def __init__(self):
        self.schema_docs = self.codes = self.ranges = self.non_bmp = 0
        self.sarif_codes = set()
        self.lsp_codes = set()
        self.registry = None
        self.counts = collections.Counter()
        self.by_origin = collections.defaultdict(set)
        self.failures = {"schema": [], "conversion": [], "range": []}

    def compare(self, label, origin, directory, native, sarif, lsp, validate):
        try:
            validate(sarif)
            self.schema_docs += 1
        except SchemaValidationError as exc:
            self.failures["schema"].append(f"{label}: {exc}")
        [run] = sarif["runs"]
        registered = [r["id"] for r in run["tool"]["driver"]["rules"]]
        require(len(set(registered)) == len(registered), "duplicate SARIF rule")
        if self.registry is None:
            self.registry = set(registered)
        require(set(registered) == self.registry, "SARIF rule catalog changed")
        require(
            run["columnKind"] == "unicodeCodePoints", "incorrect SARIF column units"
        )
        # SARIF preserves native order. LSP preserves order per URI and among unlocated items.
        located = {}
        for doc in lsp["documents"]:
            uri = unquote(doc["uri"])
            require(uri not in located, "duplicate LSP document")
            located[uri] = collections.deque(doc["diagnostics"])
        unlocated = collections.deque(lsp["unlocated"])
        results = run["results"]
        self.sarif_codes.update(item["ruleId"] for item in results)
        self.lsp_codes.update(
            item["code"] for group in located.values() for item in group
        )
        self.lsp_codes.update(item["code"] for item in unlocated)
        require(len(results) == len(native), "native and SARIF counts differ")
        for item, sr in zip(native, results):
            code, filename, span = item["code"], item["file"], item["range"]
            self.by_origin[origin].add(code)
            file = (directory / filename).resolve()
            uri = unquote(file.as_uri())
            locations = sr["locations"]
            require(
                len(locations) == int(bool(filename)),
                f"{code}: wrong SARIF location count",
            )
            physical = locations[0]["physicalLocation"] if locations else {}
            if filename:
                uri_ref = physical["artifactLocation"]["uri"]
                actual_uri = unquote(urljoin(directory.as_uri() + "/", uri_ref))
                require(uri == actual_uri, f"{code}: wrong SARIF artifact")
            group = unlocated if span is None else located.get(uri)
            require(bool(group), f"{code}: missing LSP diagnostic at native location")
            lp = group.popleft()
            require(
                code == sr["ruleId"] == lp["code"], f"{code}: projected code differs"
            )
            require(
                item["message"] == sr["message"]["text"] == lp["message"],
                f"{code}: message differs",
            )
            require(item["severity"] == sr["level"], f"{code}: SARIF severity differs")
            if span is None:
                require(lp == item, f"{code}: unlocated LSP report lost native fields")
            else:
                require(
                    lp["severity"] == {"error": 1, "note": 3}[item["severity"]],
                    f"{code}: LSP severity differs",
                )
            self.codes += 1
            try:
                rs, rl, non_bmp = expected_ranges(span, file)
                require(
                    physical.get("region") == rs and lp["range"] == rl,
                    "projected range differs",
                )
                self.ranges += 1
                self.non_bmp += int(non_bmp)
            except (OSError, ValueError) as exc:
                self.failures["range"].append(f"{label}/{code}/{filename}: {exc}")
        require(not unlocated and not any(located.values()), "extra LSP diagnostics")

    def result(self):
        for name, codes in (("SARIF", self.sarif_codes), ("LSP", self.lsp_codes)):
            if not self.registry or self.registry != codes:
                self.failures["conversion"].append(
                    f"{name} observations differ from the registered codes"
                )
        if not self.non_bmp:
            self.failures["range"].append(
                "no supplementary character affected a measured UTF-16 endpoint"
            )
        return {
            "sarifSchemaSha256": SARIF_SHA256,
            "sarifCodes": sorted(self.sarif_codes),
            "lspCodes": sorted(self.lsp_codes),
            "schemaDocuments": self.schema_docs,
            "codeComparisons": self.codes,
            "rangeComparisons": self.ranges,
            "nonBmpComparisons": self.non_bmp,
            "schemaFailures": len(self.failures["schema"]),
            "conversionFailures": len(self.failures["conversion"]),
            "rangeFailures": len(self.failures["range"]),
            "provenance": {
                k: {"invocations": n, "nativeCodes": sorted(self.by_origin[k])}
                for k, n in self.counts.items()
            },
            "failures": self.failures,
        }


def run_case(exe, directory, argv, status, mode):
    proc = subprocess.run(
        [exe, *argv],
        cwd=directory,
        capture_output=True,
        stdin=subprocess.DEVNULL,
        timeout=120,
    )
    require(
        proc.returncode == status, f"{mode} exit {proc.returncode}, expected {status}"
    )
    require(not proc.stderr, f"unexpected {mode} stderr")
    return proc.stdout


def measure_case(measurements, label, origin, exe, directory, argv, status, validate):
    measurements.counts[origin] += 1
    try:
        # Output formats are mutually exclusive. Replay the original GBNF invocation
        # too, then request each diagnostic format for the same emission command.
        if "--gbnf" in argv:
            run_case(exe, directory, argv, status, "frozen GBNF")
        base = [a for a in argv if a not in {"--json", "--gbnf"}]
        outputs = [
            CAPTURE["json_document"](
                run_case(exe, directory, [*base, mode], status, mode)
            )
            for mode in ("--json", "--sarif", "--lsp")
        ]
        native = outputs[0]
        if "diagnostics" not in native:
            require(status == 0 and base[0] == "emit", "native JSON lacks diagnostics")
        items = native.get("diagnostics", [])
        if origin == "releaseSourceFixture":
            require(
                [d["code"] for d in items] == ["N_CLASS_EMPTY", "N_CELL_WIDTH"],
                "notice fixture failed",
            )
        measurements.compare(
            label, origin, directory, items, outputs[1], outputs[2], validate
        )
    except (
        OSError,
        ValueError,
        KeyError,
        TypeError,
        IndexError,
        subprocess.SubprocessError,
    ) as exc:
        measurements.failures["conversion"].append(f"{label}: {exc}")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("executable", type=Path)
    parser.add_argument("corpus", type=Path)
    parser.add_argument(
        "--schema", required=True, type=Path, help="local OASIS SARIF 2.1.0 OS schema"
    )
    parser.add_argument(
        "--output", type=Path, help="write the projections provider member as JSON"
    )
    args = parser.parse_args()
    exe, corpus, schema = (
        p.resolve(strict=True) for p in (args.executable, args.corpus, args.schema)
    )
    if args.output and (
        args.output.resolve() in {exe, schema, corpus}
        or corpus in args.output.resolve().parents
    ):
        parser.error("choose output outside the corpus, reader and schema")
    validate = schema_validator(schema)
    cases = CAPTURE["verify_corpus"](corpus)
    measurements = Measurements()
    for case in cases:
        directory = corpus / case
        spec = CAPTURE["json_document"]((directory / "expected.json").read_bytes())
        for inv in ([spec] if "invocation" in spec else []) + spec.get("subcases", []):
            label = case if inv is spec else case + "/" + inv["name"]
            argv = inv["invocation"]
            require(argv[0] == "typify", "unexpected frozen executable")
            measure_case(
                measurements,
                label,
                "frozenV12",
                exe,
                directory,
                argv[1:],
                inv["exit_code"],
                validate,
            )
    with tempfile.TemporaryDirectory() as fixture:
        directory = Path(fixture).resolve()
        (directory / "notices.typify").write_bytes(NOTICES.encode("utf-8"))
        measure_case(
            measurements,
            "release/notes",
            "releaseSourceFixture",
            exe,
            directory,
            ["check", "notices.typify"],
            0,
            validate,
        )
    report = measurements.result()
    report["readerBuild"] = CAPTURE["json_document"](
        run_case(exe, corpus, ["--version", "--json"], 0, "version")
    )["build"]
    report["v12ImportSha256"] = CAPTURE["V12_RECEIPT"]
    report["validator"] = "fastjsonschema==2.21.2"
    raw_report = (
        json.dumps(report, indent=2, sort_keys=True, ensure_ascii=False) + "\n"
    ).encode("utf-8")
    if args.output:
        CAPTURE["write_transcript"](args.output, raw_report)
    else:
        sys.stdout.buffer.write(raw_report)
    return int(any(measurements.failures.values()))


if __name__ == "__main__":
    sys.exit(main())
