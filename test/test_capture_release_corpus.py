import base64
import hashlib
import io
import json
from contextlib import redirect_stderr, redirect_stdout
from pathlib import Path
import runpy
import subprocess
import sys
import tarfile
import tempfile
import unittest
from unittest.mock import patch


reader = runpy.run_path(str(Path(__file__).resolve().parents[1] / "scripts/capture-release-corpus.py"))


def encoded(obj):
    return json.dumps(obj, ensure_ascii=False, separators=(",", ":"), sort_keys=True).encode()


def sha(raw):
    return hashlib.sha256(raw).hexdigest()


class CaptureTests(unittest.TestCase):
    def setUp(self):
        temp = tempfile.TemporaryDirectory()
        self.addCleanup(temp.cleanup)
        self.top = Path(temp.name)
        self.archive = self.top / "frozen.tar.gz"
        self.fixture = self.top / "workspace"
        self.index = {"schema": "typify-stage4-invocations-v1", "runs": [
            {"id": "extra-a", "cwd": "examples", "argv": ["typify", "ask", "forms", "Sample", "what"], "exit": 0},
            {"id": "extra-b", "cwd": ".", "argv": ["typify", "--version"], "exit": 0},
        ]}

    def freeze(self, entries=None, sidecar=False):
        members = [("examples/forms", b"opaque source bytes", tarfile.REGTYPE)]
        if not sidecar:
            members.append(("capture.json", encoded(self.index), tarfile.REGTYPE))
        with tarfile.open(self.archive, "w:gz") as bundle:
            for name, data, kind in members + (entries or []):
                info = tarfile.TarInfo(name)
                info.type = kind
                if kind == tarfile.REGTYPE:
                    info.size = len(data)
                elif kind in (tarfile.SYMTYPE, tarfile.LNKTYPE):
                    info.linkname = "examples/forms"
                bundle.addfile(info, io.BytesIO(data) if info.size else None)
        manifest = {"ids": ["extra-a", "extra-b"], "doubleFailureId": "extra-b",
                    "archiveSha256": sha(self.archive.read_bytes())}
        index_path = None
        if sidecar:
            index_path = self.top / "invocations.json"
            index_path.write_bytes(encoded(self.index))
            manifest["invocationsSha256"] = sha(index_path.read_bytes())
        return manifest, index_path

    def capture(self, manifest, path=None):
        return reader["capture_extension"](Path(sys.executable), self.archive, self.fixture, manifest, path,
                                           ["baseline"])

    def test_transcript_bytes_and_order(self):
        for sidecar in (False, True):
            with self.subTest(sidecar=sidecar):
                manifest, path = self.freeze(sidecar=sidecar)
                validated = reader["release_manifest"](json.dumps(manifest, indent=2).encode(), sha(encoded(manifest)))
                raw_out = b'native\r\n\x00\xff'
                raw_err = b'warning\r\n\x80'
                def observe(argv, **options):
                    self.assertTrue((self.fixture / "examples" / "forms").is_file())
                    self.assertIn(options["cwd"], (self.fixture / "examples", self.fixture))
                    self.assertEqual(argv[0], Path(sys.executable))
                    return subprocess.CompletedProcess(argv, 0, raw_out, raw_err)
                with patch.object(subprocess, "run", side_effect=observe) as called:
                    rows = self.capture(validated, path)
                self.assertEqual(called.call_count, 2)
                self.assertEqual([r["id"] for r in rows], manifest["ids"])
                for row in rows:
                    self.assertEqual(base64.b64decode(row["stdout"]), raw_out)
                    self.assertEqual(base64.b64decode(row["stderr"]), raw_err)
                    self.assertEqual(row["exit"], 0)
                self.assertFalse(self.fixture.exists())

    def test_nested_argv_rejected_before_first_run(self):
        self.index["runs"][1]["argv"] = ["typify", ["check", "forms"]]
        manifest, _ = self.freeze()
        with patch.object(subprocess, "run") as called, self.assertRaises(ValueError):
            self.capture(manifest)
        called.assert_not_called()
        self.assertFalse(self.fixture.exists())

    def test_pins_and_frozen_inventory(self):
        manifest, index_path = self.freeze(sidecar=True)
        pin = sha(encoded(manifest))
        for key, value in [("ids", ["extra-b", "extra-a"]), ("archiveSha256", "0"*64)]:
            with self.subTest(key=key), self.assertRaises(ValueError):
                reader["release_manifest"](encoded(dict(manifest, **{key: value})), pin)
        for ids in (["extra-a"], ["extra-a", "extra-a"], ["extra-b", "extra-a"]):
            with self.subTest(ids=ids), self.assertRaises(ValueError):
                self.capture(dict(manifest, ids=ids), index_path)
        with self.assertRaises(ValueError):
            self.capture(dict(manifest, archiveSha256="0"*64), index_path)
        index_path.write_bytes(b"changed")
        with self.assertRaises(ValueError):
            self.capture(manifest, index_path)
        with self.assertRaises(ValueError):
            self.capture(manifest)
        self.assertFalse(self.fixture.exists())

    def test_unsafe_archive_and_operands_refused(self):
        for name, kind in [("../outside", tarfile.REGTYPE), ("X:/outside", tarfile.REGTYPE),
                           ("dir/link", tarfile.SYMTYPE), ("dir/hard", tarfile.LNKTYPE),
                           ("EXAMPLES/forms", tarfile.REGTYPE), ("examples/forms/more", tarfile.REGTYPE),
                           ("examples/forms", tarfile.REGTYPE), ("dir/con.txt", tarfile.REGTYPE)]:
            manifest, _ = self.freeze([(name, b"", kind)])
            with self.subTest(name=name), self.assertRaises(ValueError):
                self.capture(manifest)
            self.assertFalse(self.fixture.exists())
        for arg in ("../../outside", "X:outside", "/outside", r"folder\outside"):
            self.index["runs"][1]["argv"] = ["typify", "check", arg]
            manifest, _ = self.freeze()
            with self.subTest(arg=arg), self.assertRaises(ValueError):
                self.capture(manifest)
        self.assertFalse((self.top / "outside").exists())

    def test_names_with_one_filesystem_spelling(self):
        manifest, _ = self.freeze([("café", b"a", tarfile.REGTYPE), ("cafe\u0301", b"b", tarfile.REGTYPE)])
        with self.assertRaises(ValueError):
            self.capture(manifest)
        self.assertFalse(self.fixture.exists())

    def test_target_mismatch_and_invalid_json_refused(self):
        main = reader["main"]
        argv = ["capture", "exe", "baseline", "output", "--extension-archive", "frozen",
                "--extension-root", "fixture", "--extension-manifest", "manifest",
                "--extension-manifest-sha256", "0"*64, "--target", "macos-aarch64"]
        with patch.dict(main.__globals__, {"native_target": lambda: "linux-x86_64"}):
            with patch.object(sys, "argv", argv), redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as error:
                main()
            self.assertEqual(error.exception.code, 2)
        with patch.object(subprocess, "run", return_value=subprocess.CompletedProcess([], 0, b'{"x":NaN}', b'')):
            with self.assertRaises(ValueError):
                reader["run_reader"](Path(sys.executable), self.top, "invalid", ["typify", "--version", "--json"], 0)

    def test_existing_destination_preserved(self):
        manifest, _ = self.freeze()
        self.fixture.mkdir()
        sentinel = self.fixture / "keep"
        sentinel.write_bytes(b"before")
        with self.assertRaises(FileExistsError):
            self.capture(manifest)
        self.assertEqual(sentinel.read_bytes(), b"before")

    def test_process_streams_captured_from_real_child(self):
        script = "import os, sys\nos.write(1,b'x\\r\\n')\nos.write(2,b'y\\x80')\nsys.exit(2)"
        argv = ["typify", "-c", script]
        row = reader["run_reader"](Path(sys.executable), self.top, "real", argv, 2)
        self.assertEqual((row["exit"], base64.b64decode(row["stdout"]), base64.b64decode(row["stderr"])),
                         (2, b"x\r\n", b"y\x80"))
        with self.assertRaises(ValueError):
            reader["run_reader"](Path(sys.executable), self.top, "real", argv, 0)

    def test_baseline_cli_and_combined_capture(self):
        corpus = self.top / "corpus"
        directory = corpus / "case"
        directory.mkdir(parents=True)
        spec = encoded({"invocation": ["typify", "--version"], "exit_code": 0})
        (directory / "expected.json").write_bytes(spec)
        receipt = encoded({"cases": ["case"], "directories": ["case"], "files": [
            {"path": "case/expected.json", "bytes": len(spec), "sha256": sha(spec)}]})
        (corpus / "import.json").write_bytes(receipt)
        (corpus / "hostile.json").write_bytes(b"{}")
        manifest, _ = self.freeze()
        manifest_path = self.top / "manifest.json"
        manifest_path.write_bytes(encoded(manifest))
        baseline_path, combined_path = self.top / "baseline.json", self.top / "combined.json"
        command = ["capture", sys.executable, str(corpus)]
        combined = [str(combined_path), "--extension-manifest", str(manifest_path),
                    "--extension-manifest-sha256", sha(encoded(manifest)), "--extension-root", str(self.fixture),
                    "--extension-archive", str(self.archive), "--target", "linux-x86_64"]
        main = reader["main"]
        out = io.StringIO()
        with patch.dict(main.__globals__, {"V12_RECEIPT": sha(receipt), "native_target": lambda: "linux-x86_64"}):
            with patch.object(subprocess, "run", return_value=subprocess.CompletedProcess([], 0, b"x\r\n", b"\x80")):
                with patch.object(sys, "argv", command + [str(baseline_path)]), redirect_stdout(io.StringIO()):
                    self.assertEqual(main(), 0)
                with patch.object(sys, "argv", command + combined), redirect_stdout(out):
                    self.assertEqual(main(), 0)
        baseline = json.loads(baseline_path.read_bytes())
        all_rows = json.loads(combined_path.read_bytes())
        self.assertEqual(baseline, all_rows[:1])
        self.assertEqual([r["id"] for r in all_rows], ["case", "extra-a", "extra-b"])
        self.assertEqual(json.loads(out.getvalue()), {"target": "linux-x86_64", "v12Invocations": 1,
                         "extensionInvocations": 2, "transcriptSha256": sha(combined_path.read_bytes())})


if __name__ == "__main__":
    unittest.main()
