import copy
import pathlib
import runpy
import tempfile
import unittest

MEASURE = runpy.run_path(str(pathlib.Path(__file__).resolve().parents[1] / "scripts/measure-release-diagnostics.py"))


class ProjectionMeasurementTest(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        self.root = pathlib.Path(temporary.name).resolve()
        self.path = self.root / "ordered source.typify"
        self.path.write_bytes("🚀a\r\n💡z".encode())
        self.span = {"start": {"line": 1, "column": 2, "byteOffset": 4},
                     "end": {"line": 2, "column": 2, "byteOffset": 11}}
        self.native = {"file": self.path.name, "code": "N_TEST", "message": "one",
                       "severity": "note", "range": self.span}
        self.unlocated = {"file": "", "code": "E_USAGE", "message": "two", "severity": "error", "range": None}
        self.sr = {"runs": [{"tool": {"driver": {"rules": [{"id": "N_TEST"}, {"id": "E_USAGE"}]}},
                             "columnKind": "unicodeCodePoints", "results": [
                                 {"ruleId": "N_TEST", "level": "note", "message": {"text": "one"}, "locations": [
                                     {"physicalLocation": {"artifactLocation": {"uri": "ordered%20source.typify"},
                                                           "region": {"startLine": 1, "endLine": 2,
                                                                      "startColumn": 2, "endColumn": 2,
                                                                      "byteOffset": 4, "byteLength": 7}}}]},
                                 {"ruleId": "E_USAGE", "level": "error", "message": {"text": "two"},
                                  "locations": []}]}]}
        self.lp = {"documents": [{"uri": self.path.as_uri(), "diagnostics": [
            {"code": "N_TEST", "severity": 3, "message": "one",
             "range": {"start": {"line": 0, "character": 2}, "end": {"line": 1, "character": 2}}}]}],
                   "unlocated": [copy.deepcopy(self.unlocated)]}

    def compare(self):
        metrics = MEASURE["Measurements"]()
        metrics.compare("example", "unit", self.root, [self.native, self.unlocated],
                        self.sr, self.lp, lambda doc: None)
        return metrics.result()

    def test_unpinned_schema_is_refused(self):
        with self.assertRaisesRegex(ValueError, "SHA-256"):
            MEASURE["schema_validator"](self.path)

    def test_utf16_crlf_and_null(self):
        result = self.compare()
        self.assertEqual([result[k] for k in ("codeComparisons", "rangeComparisons", "nonBmpComparisons")], [2, 2, 1])
        self.assertEqual(result["sarifCodes"], result["lspCodes"])
        self.assertFalse(any(result["failures"].values()))

    def test_codepoint_substitution_is_detected(self):
        self.lp["documents"][0]["diagnostics"][0]["range"]["end"]["character"] = 1
        self.assertGreater(self.compare()["rangeFailures"], 0)

    def test_agreeing_native_and_sarif_wrong_byte_offset_is_detected(self):
        self.span["end"]["byteOffset"] += 1
        self.sr["runs"][0]["results"][0]["locations"][0]["physicalLocation"]["region"]["byteLength"] += 1
        self.assertGreater(self.compare()["rangeFailures"], 0)

    def test_missing_or_wrongly_grouped_lsp_diagnostic_is_detected(self):
        self.lp["documents"][0]["uri"] = (self.root / "other.typify").as_uri()
        with self.assertRaisesRegex(ValueError, "missing LSP"):
            self.compare()

    def test_unlocated_code_cannot_disappear(self):
        self.lp["unlocated"] = []
        with self.assertRaisesRegex(ValueError, "missing LSP"):
            self.compare()


if __name__ == "__main__":
    unittest.main()
