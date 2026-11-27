#!/usr/bin/env python3
"""The gate's own test (marks G1 to G4).

    FMT_GATE_BASE_READER=/path/to/typify python3 test_gate.py

FMT_GATE_BASE_READER names a real reader: the stand-ins hand it every command
but `fmt`, and the laws below use its `check` to tell accepted texts from
refused ones. Without it the tests that need a reader are skipped.
Standard library only.
"""

import concurrent.futures
import filecmp
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import unittest

sys.dont_write_bytecode = True  # the repository takes no compiled files
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)

import gate  # noqa: E402
import make_cases  # noqa: E402
import ref_fmt  # noqa: E402

BASE = os.environ.get("FMT_GATE_BASE_READER") or shutil.which("typify")
STANDIN = os.path.join(HERE, "standin", "typify")
CASES = os.path.join(HERE, "cases")

# each fault, with the cases that must catch it (the report names them too)
FAULTS = {
    "strip-payload": [
        "d07-declarations-payload-untouched",
        "p10-program-payload-untouched",
    ],
    "blank-before-bang": ["d09-declarations-bang-line", "p12-program-bang-lines"],
    "nbsp-blank": ["p07-program-comment-blank-lookalikes"],
    "sort-is": ["d12-declarations-already-canonical"],
    "blank-prefix": ["b01-blank-prefix-plain", "b12-blank-prefix-accepted-program"],
    "print-note": ["n01-note-plain", "n02-note-check-loose", "n04-note-write"],
    "keep-blank-runs": ["d05-declarations-blank-line-runs", "p05-program-blank-lines"],
    "crlf": ["d12-declarations-already-canonical", "p14-program-already-canonical"],
}


def run_gate(reader, *extra, fault=""):
    env = dict(os.environ, FMT_GATE_BASE_READER=BASE or "typify")
    if fault:
        env["FMT_GATE_FAULT"] = fault
    done = subprocess.run(
        [sys.executable, os.path.join(HERE, "gate.py"), reader, *extra],
        capture_output=True,
        env=env,
    )
    return done.returncode, done.stdout.decode("utf-8", "replace")


def case_dirs():
    return sorted(os.listdir(CASES))


class CasesAreTheirGenerator(unittest.TestCase):
    def test_cases_match_make_cases(self):
        scratch = tempfile.mkdtemp(prefix="fmt-gate-gen-")
        try:
            target = os.path.join(scratch, "cases")
            count = make_cases.write_cases(target)
            self.assertEqual(count, len(case_dirs()))
            for name in case_dirs():
                here, there = os.path.join(CASES, name), os.path.join(target, name)
                files = sorted(
                    os.path.relpath(os.path.join(d, f), here)
                    for d, _s, fs in os.walk(here)
                    for f in fs
                )
                self.assertEqual(
                    files,
                    sorted(
                        os.path.relpath(os.path.join(d, f), there)
                        for d, _s, fs in os.walk(there)
                        for f in fs
                    ),
                    name,
                )
                for relative in files:
                    if relative == "case.json":
                        with (
                            open(os.path.join(here, relative), encoding="utf-8") as a,
                            open(os.path.join(there, relative), encoding="utf-8") as b,
                        ):
                            self.assertEqual(json.load(a), json.load(b), name)
                    else:
                        self.assertTrue(
                            filecmp.cmp(
                                os.path.join(here, relative),
                                os.path.join(there, relative),
                                False,
                            ),
                            "%s/%s" % (name, relative),
                        )
        finally:
            shutil.rmtree(scratch, ignore_errors=True)

    def test_every_case_cites_the_text(self):
        for name in case_dirs():
            with open(
                os.path.join(CASES, name, "case.json"), encoding="utf-8"
            ) as handle:
                meta = json.load(handle)
            self.assertTrue(meta["cites"], name)
            self.assertTrue(meta["title"], name)


class G1ReferenceMapsEveryCase(unittest.TestCase):
    """The reference gives each plain-mode case's expectation from its input."""

    def test_reference_on_every_form_case(self):
        count = 0
        for name in case_dirs():
            directory = os.path.join(CASES, name)
            with open(os.path.join(directory, "case.json"), encoding="utf-8") as handle:
                meta = json.load(handle)
            args = meta["args"]
            if meta["premise"] != "accepted" or any(a.startswith("--") for a in args):
                continue
            expect = meta["expect"]
            scratch = tempfile.mkdtemp(prefix="fmt-gate-ref-")
            try:
                table = meta.get("bytes", {})
                gate.materialize(directory, scratch, table)
                done = subprocess.run(
                    [sys.executable, os.path.join(HERE, "ref_fmt.py")] + args[1:],
                    cwd=scratch,
                    capture_output=True,
                )
            finally:
                shutil.rmtree(scratch, ignore_errors=True)
            want_out = (
                gate.restore(
                    gate.read(os.path.join(directory, expect["stdout"])), table
                )
                if "stdout" in expect
                else b""
            )
            want_err = (
                gate.restore(
                    gate.read(os.path.join(directory, expect["stderr"])), table
                )
                if "stderr" in expect
                else b""
            )
            self.assertEqual(done.stdout, want_out, name)
            self.assertEqual(done.stderr, want_err, name)
            self.assertEqual(done.returncode, expect["exit"], name)
            count += 1
        print("G1: the reference maps %d of %d cases" % (count, count))
        self.assertGreater(count, 40)


@unittest.skipUnless(BASE, "needs a real reader: set FMT_GATE_BASE_READER")
class G2Laws(unittest.TestCase):
    """L2, L3 and L5 of the reference on the corpus, the ask gate and the variants."""

    def test_idempotent_and_one_form_per_text(self):
        groups = gate.corpus_texts(
            os.path.join(gate.ROOT, "corpus"), os.path.join(gate.ROOT, "ask-gate")
        )
        work = [(b, d, l, t) for b, found in groups for d, l, t in found]

        def one(item):
            base, decl, lang, target = item
            scratch = tempfile.mkdtemp(prefix="fmt-gate-laws-")
            try:
                gate.copy_tree(base, scratch)
                check = [BASE, "check", decl] + ([lang, target] if lang else [])
                if gate.run(check, scratch)[0] != 0:
                    return None
                path = os.path.join(scratch, target)
                original = gate.read(path)
                prefix = ref_fmt.DEFAULT_PREFIX
                if lang:
                    prefix = ref_fmt.declared_prefix(
                        gate.read(os.path.join(scratch, decl)), lang.encode()
                    )
                    if ref_fmt.holds_blank(prefix):
                        return None
                same = gate.projection(BASE, scratch, decl, lang, target)
                form = ref_fmt.canonical(original, prefix)
                result = [ref_fmt.canonical(form, prefix) == form]  # L2 on the text
                alike = 0
                left_out = 0
                for name, text in gate.variants(original, prefix, lang is None):
                    with open(path, "wb") as handle:
                        handle.write(text)
                    if (
                        gate.run(check, scratch)[0] != 0
                        or gate.projection(BASE, scratch, decl, lang, target) != same
                    ):
                        left_out += 1
                        continue
                    alike += 1
                    result.append(ref_fmt.canonical(text, prefix) == form)  # L3
                    result.append(
                        ref_fmt.canonical(ref_fmt.canonical(text, prefix), prefix)
                        == ref_fmt.canonical(text, prefix)
                    )  # L2 on the variant
                return result, alike, left_out
            finally:
                gate.release(scratch)

        with concurrent.futures.ThreadPoolExecutor(os.cpu_count() or 2) as pool:
            done = [r for r in pool.map(one, work) if r]
        checks = sum(len(r[0]) for r in done)
        passed = sum(sum(r[0]) for r in done)
        print(
            "G2: %d accepted texts, %d variants that read alike (%d left out), "
            "%d checks of L2 and L3, %d hold"
            % (
                len(done),
                sum(r[1] for r in done),
                sum(r[2] for r in done),
                checks,
                passed,
            )
        )
        self.assertEqual(checks, passed)
        self.assertGreater(len(done), 50)

    def test_self_declaration_is_a_fixed_point(self):
        data = gate.read(os.path.join(HERE, "fixtures", "self-declaration.txt"))
        self.assertEqual(ref_fmt.canonical(data), data)
        print(
            "G2: the reference maps the self-declaration (%d bytes) to itself"
            % len(data)
        )


@unittest.skipUnless(BASE, "needs a real reader: set FMT_GATE_BASE_READER")
class G3Teeth(unittest.TestCase):
    def test_stand_in_with_the_reference_passes(self):
        status, out = run_gate(STANDIN)
        total = len(case_dirs())
        self.assertIn("cases: %d, hold: %d, fail: 0" % (total, total), out)
        self.assertRegex(out, r"m5: texts compared: (\d+), equal: \1 ")
        self.assertEqual(status, 0, out)
        print(
            "G3: "
            + "; ".join(l for l in out.splitlines() if l.startswith(("cases:", "m5:")))
        )

    def test_each_fault_is_caught(self):
        for fault, names in FAULTS.items():
            with self.subTest(fault=fault):
                status, out = run_gate(STANDIN, "--no-m5", fault=fault)
                failed = re.findall(r"^FAIL (\S+?):", out, re.M)
                self.assertEqual(status, 1, fault)
                for name in names:
                    self.assertIn(name, failed, "%s must catch %s" % (fault, name))
                print(
                    "G3: %s fails %d cases, among them %s"
                    % (fault, len(failed), ", ".join(names))
                )


@unittest.skipUnless(BASE, "needs a real reader: set FMT_GATE_BASE_READER")
class G4ReaderWithoutTheCommand(unittest.TestCase):
    def test_gate_fails_with_a_count_and_does_not_crash(self):
        status, out = run_gate(STANDIN, fault="no-command")
        total = len(case_dirs())
        self.assertEqual(status, 1)
        self.assertIn("cases: %d, hold: 0, fail: %d" % (total, total), out)
        self.assertIn("gate: does not hold", out)
        self.assertNotIn("Traceback", out)
        print(
            "G4: "
            + "; ".join(l for l in out.splitlines() if l.startswith(("cases:", "m5:")))
        )

    def test_gate_does_not_crash_on_a_missing_reader(self):
        status, out = run_gate("/nonexistent/typify", "--no-m5")
        self.assertEqual(status, 1)
        self.assertNotIn("Traceback", out)


if __name__ == "__main__":
    unittest.main(verbosity=1)
