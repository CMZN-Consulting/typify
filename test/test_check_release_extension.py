import base64
import json
import os
import runpy
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

gate = runpy.run_path(
    str(Path(__file__).resolve().parents[1] / "scripts/check-release-extension.py")
)


class SemanticComparisonTests(unittest.TestCase):
    def test_types_order_absence_and_pointer_escaping(self):
        want = {"a/b~c": [False, 1], "missing": None}
        got = {"a/b~c": [0, 1, "added"], "more": {"text": "á"}}
        self.assertEqual(
            list(gate["differences"](want, got)),
            [
                {"path": "/a~1b~0c/0", "expected": False, "observed": 0},
                {"path": "/a~1b~0c/2", "observed": "added"},
                {"path": "/missing", "expected": None},
                {"path": "/more", "observed": {"text": "á"}},
            ],
        )

    def test_asserted_field_comparison_keeps_array_order_and_length(self):
        want = {"items": [{"code": "E", "range": {"line": 1}}]}
        got = {
            "items": [{"code": "E", "range": {"line": 2}, "other": 3}, {}],
            "different": True,
        }
        self.assertEqual(
            list(gate["differences"](want, got, object_subset=True)),
            [
                {"path": "/items/0/range/line", "expected": 1, "observed": 2},
                {"path": "/items/1", "observed": {}},
            ],
        )

    @staticmethod
    def capture(data, code=0):
        return {
            "exit": code,
            "stdout": base64.b64encode(json.dumps(data).encode()).decode("ascii"),
            "stderr": "",
        }

    def test_native_semantics_cover_asserted_diagnostic_fields_and_exact_result_objects(
        self,
    ):
        diag = {
            "code": "E_X",
            "file": "a.typify",
            "range": {"start": {"byteOffset": 4}},
            "data": {"k": "red"},
            "related": [{"kind": "program", "path": "b", "line": 3}],
            "provenance": ["source"],
        }
        spec = {
            "exit_code": 1,
            "invocation": ["typify", "check", "a.typify", "--json"],
            "stdout_json": {"ok": False, "terms": [], "diagnostics": [diag]},
        }
        actual = {
            **diag,
            "severity": "error",
            "message": "D",
            "data": {"k": "red", "runtime": 0},
        }
        del actual["provenance"]
        packet = self.capture({"ok": False, "terms": [], "diagnostics": [actual]}, 1)
        self.assertEqual(gate["primary_differences"](spec, packet, Path(".")), [])
        bad = self.capture(
            {
                "ok": False,
                "extra": 1,
                "terms": [],
                "diagnostics": [
                    {
                        **actual,
                        "file": "b.typify",
                        "data": {},
                        "related": [],
                        "range": {"start": {"byteOffset": 5}},
                    }
                ],
            },
            0,
        )
        paths = {d["path"] for d in gate["primary_differences"](spec, bad, Path("."))}
        self.assertEqual(
            paths,
            {
                "/exit_code",
                "/stdout_json/extra",
                "/stdout_json/diagnostics/0/file",
                "/stdout_json/diagnostics/0/related/0",
                "/stdout_json/diagnostics/0/data/k",
                "/stdout_json/diagnostics/0/range/start/byteOffset",
            },
        )

    def test_marker_validates_original_capture_directory(self):
        with tempfile.TemporaryDirectory() as tmp:
            cwd = Path(tmp).resolve(strict=True)
            for kind in ("tree", "plan"):
                argv = ["typify", kind, "decl.typify", ".", "--json"]
                spec = {
                    "exit_code": 0,
                    "invocation": argv,
                    "stdout_json": {
                        "inputs": {"root": {"$fixtureRoot": "root"}},
                        "diagnostics": [],
                    },
                }
                expected_path = gate["fixture_root"](cwd, argv)
                primary = self.capture(
                    {"inputs": {"root": expected_path}, "diagnostics": []}
                )
                self.assertEqual(gate["primary_differences"](spec, primary, cwd), [])
                self.assertTrue(
                    gate["primary_differences"](spec, primary, cwd / "somewhere-else")
                )

    def test_main_resolves_temporary_fixture_directory_before_execution(self):
        with tempfile.TemporaryDirectory() as outer:
            real_tmp = Path(outer).resolve(strict=True) / "real-temp"
            real_tmp.mkdir()
            alias_tmp = Path(outer).resolve(strict=True) / "alias-temp"
            os.symlink(real_tmp, alias_tmp)
            exe = real_tmp / "reader"
            exe.write_text("#!/bin/sh\nexit 0\n")
            exe.chmod(0o755)
            archive = real_tmp / "ext.tar"
            archive.write_bytes(b"")
            report = real_tmp / "report.json"
            seen = []

            class FakeTempDir:
                def __enter__(self):
                    return str(alias_tmp)

                def __exit__(self, *exc):
                    return False

            class DummyCtx:
                def __enter__(self):
                    return None

                def __exit__(self, *exc):
                    return False

            def fake_unpack(_archive, root, _pin):
                seen.append(root)
                return DummyCtx()

            with (
                patch.object(
                    sys,
                    "argv",
                    ["check-release-extension.py", str(exe), str(archive), str(report)],
                ),
                patch.object(
                    tempfile, "TemporaryDirectory", return_value=FakeTempDir()
                ),
                patch.dict(gate["CAPTURE"], {"unpack_extension": fake_unpack}),
                patch.dict(
                    gate["main"].__globals__,
                    {
                        "inventory": lambda _f: ([], {}),
                        "reader_identity": lambda _e, _f: {
                            "build": gate["READER_BUILD"]
                        },
                        "measure": lambda *_a: [{"id": "x", "differences": []}],
                        "save_report": lambda *_a: {"pass": True},
                    },
                ),
            ):
                rc = gate["main"]()
            self.assertEqual(rc, 0)
            self.assertEqual(seen, [real_tmp / "fixtures"])

    def test_sarif_and_lsp_read_native_file_id_ranges_and_properties(self):
        diag = {"code": "E_X", "file": "example.typify"}
        endpoint = {"line": 2, "column": 4, "byteOffset": 7}
        end = {"line": 2, "column": 6, "byteOffset": 12}
        sarif = {
            "ruleId": "E_X",
            "level": "error",
            "region": {"start": endpoint, "end": end},
            "properties": {"word": "cat"},
        }
        lsp = {"start": {"line": 1, "character": 4}, "end": {"line": 1, "character": 7}}
        spec = {"exit_code": 1, "diagnostics": [diag]}
        cwd = Path.cwd()
        lsp_wire = {
            "documents": [
                {
                    "uri": (cwd / "example.typify").as_uri(),
                    "diagnostics": [{"code": "E_X", "range": lsp}],
                }
            ],
            "unlocated": [],
        }
        expected = gate["expected_projection"]("sarif", [sarif], [diag], cwd)
        self.assertEqual(
            expected[0]["locations"][0]["physicalLocation"]["region"]["byteLength"], 5
        )
        sarif_wire = {"runs": [{"results": expected}]}
        for fmt, claims, obj in (
            ("lsp", [lsp], lsp_wire),
            ("sarif", [sarif], sarif_wire),
        ):
            self.assertEqual(
                gate["projection_differences"](
                    fmt, claims, spec, self.capture(obj, 1), cwd
                ),
                [],
            )
        lsp_wire["documents"][0]["uri"] = (cwd / "other.typify").as_uri()
        expected[0]["locations"][0]["physicalLocation"]["region"]["byteLength"] = 6
        self.assertTrue(
            gate["projection_differences"](
                "sarif", [sarif], spec, self.capture(sarif_wire, 1), cwd
            )
        )
        self.assertTrue(
            gate["projection_differences"](
                "lsp", [lsp], spec, self.capture(lsp_wire, 1), cwd
            )
        )

    def test_transcript_inventory_and_raw_streams(self):
        rows = [
            {"id": "invented/first", "exit": 1, "stdout": "AP8=", "stderr": "DQo="},
            {"id": "invented/second", "exit": 0, "stdout": "", "stderr": ""},
        ]
        with tempfile.TemporaryDirectory() as tmp:
            f = Path(tmp) / "transcript.json"
            runs = [{"id": r["id"]} for r in rows]
            f.write_text(json.dumps(rows))
            self.assertEqual(gate["read_transcript"](f, runs)[rows[0]["id"]], rows[0])
            for bad in (
                rows[::-1],
                rows[:1],
                [rows[0], rows[0]],
                [{**rows[0], "stderr": "!"}, rows[1]],
            ):
                f.write_text(json.dumps(bad))
                with self.assertRaises(ValueError):
                    gate["read_transcript"](f, runs)

    def test_actual_nonzero_and_streams_survive(self):
        command = [
            "-c",
            "import os\nos.write(1,b'\\xff\\r\\n')\nos.write(2,b'\\x00')\nraise SystemExit(7)",
        ]
        with tempfile.TemporaryDirectory() as directory:
            row = gate["native_run"](sys.executable, command, directory)
        self.assertEqual(row["exit"], 7)
        self.assertEqual(base64.b64decode(row["stdout"], validate=True), b"\xff\r\n")
        self.assertEqual(base64.b64decode(row["stderr"], validate=True), b"\x00")

    def test_reader_identity_uses_build_pin(self):
        for build, admitted in [
            (gate["READER_BUILD"], True),
            ("sha256:" + "0" * 64, False),
        ]:
            version = {"version": "1.0.1", "build": build, "diagnostics": []}
            result = subprocess.CompletedProcess(
                [], 0, json.dumps(version).encode(), b""
            )
            with (
                self.subTest(build=build),
                patch.object(subprocess, "run", return_value=result),
            ):
                if admitted:
                    self.assertEqual(
                        gate["reader_identity"]("reader", ".")["build"], build
                    )
                else:
                    with self.assertRaises(ValueError):
                        gate["reader_identity"]("reader", ".")

    def test_lossless_private_report_and_redacted_summary(self):
        row = {
            "id": "invented-id",
            "exit": 1,
            "stdout": "AP8=",
            "stderr": "Cg==",
            "differences": [
                {"path": "/field", "expected": None, "observed": "private-value"}
            ],
        }
        with tempfile.TemporaryDirectory() as directory:
            dest = Path(directory) / "report.json"
            summary = gate["save_report"](
                dest, {"build": "typed"}, [row], "archive", "tree"
            )
            self.assertFalse(summary["pass"])
            self.assertEqual(
                (summary["invocations"], summary["failures"], summary["differences"]),
                (1, 1, 1),
            )
            self.assertNotIn("private-value", json.dumps(summary))
            self.assertNotIn("invented-id", json.dumps(summary))
            self.assertEqual(json.loads(dest.read_bytes())["results"], [row])
            if os.name == "posix":
                self.assertEqual(dest.stat().st_mode & 0o777, 0o600)


if __name__ == "__main__":
    unittest.main()
