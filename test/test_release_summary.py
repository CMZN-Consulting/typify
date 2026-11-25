import runpy
import unittest
from pathlib import Path


module = runpy.run_path(
    str(Path(__file__).resolve().parents[1] / "scripts/release-summary.py")
)


class NativeExtensionReceipt(unittest.TestCase):
    def setUp(self):
        self.version = "1.0.0 sha256:" + "2" * 64
        self.frozen = {"archiveSha256": "3" * 64, "ids": ["one", "two"]}
        self.result = {
            "schema": "typify-stage4-extension-result-v1",
            "pass": True,
            "readerBuild": self.version.split()[1],
            "archiveSha256": self.frozen["archiveSha256"],
            "invocations": 2,
            "failures": 0,
            "differences": 0,
            "reportSha256": "4" * 64,
        }

    def check(self, result):
        return module["check_extension"](result, self.version, 2, self.frozen)

    def test_full_receipt(self):
        self.check(self.result)

    def test_a_summarized_failure_cannot_pass(self):
        corrupt = {
            "pass": False,
            "invocations": 1,
            "readerBuild": "sha256:" + "0" * 64,
            "archiveSha256": "a" * 64,
            "failures": 1,
            "differences": 1,
            "reportSha256": "",
            "schema": "",
        }
        for name, wrong in corrupt.items():
            with self.subTest(field=name), self.assertRaises(ValueError):
                self.check(self.result | {name: wrong})
        with self.assertRaises(ValueError):
            self.check(self.result | {"differences": False})


class ForeignArchitectureReceipt(unittest.TestCase):
    def test_header_evidence_is_required_for_every_foreign_executable(self):
        launcher = dict(
            path="typify/bin/typify.exe",
            format="pe",
            architecture="x86_64",
            sha256="1" * 64,
            native=True,
        )
        dll = dict(launcher, path="typify/lib/runtime.dll", sha256="2" * 64)
        build = {
            "platform": "windows-x86_64",
            "architecture": {"pass": True, "executables": [launcher, dll]},
        }
        rows = [dict(row, native=False) for row in (launcher, dll)]
        valid = {"pass": False, "platform": "linux-x86_64", "executables": rows}
        check = module["check_foreign_headers"]
        check(valid, "linux-x86_64", build)
        for bad in (
            {"error": "bad archive", "executables": None},
            {"executables": []},
            {"executables": rows[:1]},
            {"executables": [launcher, rows[1]]},
            {"executables": [dict(rows[0], format="elf"), rows[1]]},
            {"pass": True},
            {"platform": "macos-aarch64"},
        ):
            with self.subTest(bad=bad), self.assertRaises(ValueError):
                check(valid | bad, "linux-x86_64", build)


if __name__ == "__main__":
    unittest.main()
