import io
import json
import runpy
import subprocess
import tempfile
import unittest
from pathlib import Path
from contextlib import redirect_stdout
from unittest.mock import patch


CODE = runpy.run_path(
    str(Path(__file__).resolve().parents[1] / "scripts/check-release-attestations.py")
)


def fixture(base):
    builds, verification = base / "builds", base / "verification"
    required = []
    for platform in CODE["PLATFORMS"]:
        asset = (
            "typify-"
            + platform
            + (".zip" if platform.startswith("windows-") else ".tar.gz")
        )
        produced = builds / ("build-" + platform + "-a")
        canaries = verification / ("verify-" + platform) / "canaries"
        produced.mkdir(parents=True)
        canaries.mkdir(parents=True)
        for name in [
            asset,
            platform + ".MANIFEST.txt",
            asset + ".sha256",
            platform + ".build.json",
        ]:
            (produced / name).write_bytes(name.encode())
        required.extend([produced / asset, produced / (platform + ".MANIFEST.txt")])
        for kind in CODE["CONTROLS"]:
            file = canaries / (kind + "-" + asset)
            file.write_bytes(file.name.encode())
            required.append(file)
    return builds, verification, required


class AttestationInventory(unittest.TestCase):
    def test_missing_inputs_stop_before_verification(self):
        with tempfile.TemporaryDirectory() as tmp:
            base = Path(tmp)
            builds, verification, required = fixture(base)
            subjects, controls = CODE["inventory"](builds, verification)
            self.assertEqual((len(subjects), len(controls)), (8, 16))
            for file in required:
                with self.subTest(missing=file.name):
                    old = file.read_bytes()
                    file.unlink()
                    with (
                        patch("subprocess.check_output") as gh,
                        self.assertRaises(ValueError),
                    ):
                        CODE["check"](
                            builds,
                            verification,
                            base / "bundle",
                            "example/repo",
                            "refs/tags/v1",
                            "0" * 40,
                        )
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
        self.assertFalse(
            match([archive, {**manifest, "digest": {"sha256": "dd"}}], expected)
        )

    def test_same_bundle_uses_each_controls_genuine_archive(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            bundle = root / "local.json"
            builds, canaries, _ = fixture(root)
            subjects, controls = CODE["inventory"](builds, canaries)
            rows = [
                {"name": name, "digest": {"sha256": CODE["file_hash"](path)}}
                for name, path in subjects.items()
            ]
            signed = json.dumps(
                [{"verificationResult": {"statement": {"subject": rows}}}]
            )
            rejected = subprocess.CompletedProcess(
                [], 1, "", 'Error: verifying with issuer "sigstore.dev"'
            )
            with (
                patch("subprocess.check_output", return_value=signed) as positive,
                patch("subprocess.run", return_value=rejected) as negative,
                redirect_stdout(io.StringIO()),
            ):
                CODE["check"](
                    builds, canaries, bundle, "example/repo", "refs/tags/v1", "0" * 40
                )
            local_calls = [
                call.args[0]
                for call in positive.call_args_list
                if "--bundle" in call.args[0]
            ]
            self.assertCountEqual(
                [call[3] for call in local_calls],
                {subjects[asset] for asset, _ in controls},
            )
            for cmd in local_calls + [call.args[0] for call in negative.call_args_list]:
                self.assertEqual(cmd[cmd.index("--bundle") + 1], bundle)
            self.assertCountEqual(
                [call.args[0][3] for call in negative.call_args_list],
                [path for _, path in controls],
            )

    def test_wrong_subject_or_other_bundle_cannot_supply_a_positive(self):
        with tempfile.TemporaryDirectory() as tmp:
            archive, manifest = (Path(tmp) / name for name in ("archive", "manifest"))
            archive.write_bytes(b"first")
            manifest.write_bytes(b"second")
            expected = {p.name: CODE["file_hash"](p) for p in (archive, manifest)}
            rows = [
                {"name": name, "digest": {"sha256": value}}
                for name, value in expected.items()
            ]
            for path, signed_rows in [(manifest, rows), (archive, rows[:1])]:
                with self.subTest(path=path, subjects=len(signed_rows)):
                    verified = [
                        {"verificationResult": {"statement": {"subject": signed_rows}}}
                    ]
                    with (
                        patch(
                            "subprocess.check_output", return_value=json.dumps(verified)
                        ),
                        self.assertRaises(ValueError),
                    ):
                        CODE["verified_subjects"](
                            "archive", path, ["--bundle", "local.json"], expected
                        )

    def test_ambiguous_status_and_infrastructure_errors_stop_the_check(self):
        outcomes = [
            (0, ""),
            (1, ""),
            (1, "Error: no valid Sigstore verifiers could be initialized"),
            (
                1,
                "Error: could not load content from file path local.json: permission denied",
            ),
            (1, "Error: timeout contacting trust service"),
            (4, "authentication required"),
            (1, 'unexpected warning\nError: verifying with issuer "sigstore.dev"'),
        ]
        with tempfile.TemporaryDirectory() as tmp:
            control = Path(tmp) / "altered"
            control.write_bytes(b"changed")
            for rc, error in outcomes:
                with self.subTest(rc=rc, error=error):
                    with (
                        patch(
                            "subprocess.run",
                            return_value=subprocess.CompletedProcess([], rc, "", error),
                        ),
                        self.assertRaises(ValueError),
                    ):
                        CODE["verify_control"](
                            control, {"archive": "0" * 64}, ["--bundle", "local.json"]
                        )
            with patch("subprocess.run") as cli, self.assertRaises(ValueError):
                CODE["verify_control"](
                    control, {"other": CODE["file_hash"](control)}, []
                )
            cli.assert_not_called()


if __name__ == "__main__":
    unittest.main()
