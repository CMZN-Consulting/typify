import runpy
import unittest
from pathlib import Path


module = runpy.run_path(str(Path(__file__).resolve().parents[1] / "scripts/release-summary.py"))


class NativeExtensionReceipt(unittest.TestCase):
    def setUp(self):
        self.version = "1.0.0 sha256:" + "2" * 64
        self.frozen = {"archiveSha256": "3" * 64, "ids": ["one", "two"]}
        self.result = {
            "schema": "typify-stage4-extension-result-v1", "pass": True,
            "readerBuild": self.version.split()[1], "archiveSha256": self.frozen["archiveSha256"],
            "invocations": 2, "failures": 0, "differences": 0, "reportSha256": "4" * 64,
        }

    def check(self, result):
        return module["check_extension"](result, self.version, 2, self.frozen)

    def test_full_receipt(self):
        self.check(self.result)

    def test_a_summarized_failure_cannot_pass(self):
        corrupt = {"pass": False, "invocations": 1, "readerBuild": "sha256:" + "0" * 64,
                   "archiveSha256": "a" * 64, "failures": 1, "differences": 1,
                   "reportSha256": "", "schema": ""}
        for name, wrong in corrupt.items():
            with self.subTest(field=name), self.assertRaises(ValueError):
                self.check(self.result | {name: wrong})
        with self.assertRaises(ValueError):
            self.check(self.result | {"differences": False})


if __name__ == "__main__":
    unittest.main()
