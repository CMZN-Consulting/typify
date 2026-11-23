import runpy
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch


CODE = runpy.run_path(str(Path(__file__).resolve().parents[1] / "scripts/check-release-attestations.py"))


class AttestationInventory(unittest.TestCase):
    def test_missing_inputs_stop_before_verification(self):
        with tempfile.TemporaryDirectory() as tmp:
            base = Path(tmp)
            builds, verification = base / "builds", base / "verification"
            required = []
            for platform in CODE["PLATFORMS"]:
                asset = "typify-" + platform + (".zip" if platform.startswith("windows-") else ".tar.gz")
                produced = builds / ("build-" + platform + "-a")
                canaries = verification / ("verify-" + platform) / "canaries"
                produced.mkdir(parents=True)
                canaries.mkdir(parents=True)
                for name in [asset, platform + ".MANIFEST.txt", asset + ".sha256", platform + ".build.json"]:
                    (produced / name).write_bytes(name.encode())
                required.extend([produced / asset, produced / (platform + ".MANIFEST.txt")])
                for kind in CODE["CONTROLS"]:
                    file = canaries / (kind + "-" + asset)
                    file.write_bytes(file.name.encode())
                    required.append(file)
            subjects, controls = CODE["inventory"](builds, verification)
            self.assertEqual((len(subjects), len(controls)), (8, 16))
            for file in required:
                with self.subTest(missing=file.name):
                    old = file.read_bytes()
                    file.unlink()
                    with patch("subprocess.check_output") as gh, self.assertRaises(ValueError):
                        CODE["check"](builds, verification, base / "bundle", "example/repo", "refs/tags/v1", "0"*40)
                    gh.assert_not_called()
                    file.write_bytes(old)

    def test_signed_subject_set_requires_each_name_and_digest(self):
        expected = {"archive": "aa", "manifest": "bb"}
        archive = {"name": "archive", "digest": {"sha256": "aa"}}
        manifest = {"name": "manifest", "digest": {"sha256": "bb"}}
        match = CODE["subjects_match"]
        self.assertTrue(match([archive, manifest], expected))
        self.assertFalse(match([archive], expected))
        self.assertFalse(match([archive, archive], expected))
        self.assertFalse(match([archive, {**manifest, "digest": {"sha256": "dd"}}], expected))


if __name__ == "__main__":
    unittest.main()
