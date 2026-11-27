#!/usr/bin/env python3
"""The gate of `typify fmt`: does a reader's `fmt` do what the definition says?

    gate.py READER [--jobs N] [--only TEXT] [--no-m5] [--cases DIR]

READER is the path of a reader (a program that takes `check`, `fmt`, `emit`).
The gate treats it as a black box: it runs it and compares what comes out.

Part 1. For each case under cases/ it runs `READER fmt ...` in a scratch copy of
the case's files and compares standard output, standard error, the exit status
and the files afterwards with the case's expectation. Where the text says "as
check" the expectation is what the same reader's `check` gives.

Part 2 (mark M5). On every Typify text of the corpus and of the ask gate's
declarations that the reader's own `check` accepts, and on the variants of each
that law L3 names and that read alike it, the reader's `fmt` and the reference
formatter (ref_fmt.py) must agree byte for byte.

The gate prints one line per case, a count, and the M5 counts. It exits 0 only
when every case holds and every compared text is equal. Python standard library
only. Nothing outside a scratch directory is written.
"""

import argparse
import concurrent.futures
import json
import os
import shutil
import subprocess
import sys
import tempfile

sys.dont_write_bytecode = True  # the repository takes no compiled files
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)

import ref_fmt  # noqa: E402

TIMEOUT = 120
ROOT = os.path.dirname(HERE)  # the `test` directory


def run(argv, cwd):
    """(status, stdout, stderr) of a command; a reader that cannot run is status 127."""
    try:
        done = subprocess.run(
            argv,
            cwd=cwd,
            stdin=subprocess.DEVNULL,
            capture_output=True,
            timeout=TIMEOUT,
        )
    except subprocess.TimeoutExpired:
        return 124, b"", b"gate: the command ran longer than %d s\n" % TIMEOUT
    except OSError as error:
        return 127, b"", ("gate: cannot run %s: %s\n" % (argv[0], error)).encode()
    return done.returncode, done.stdout, done.stderr


def short(data, limit=90):
    shown = repr(data)
    return shown if len(shown) <= limit else shown[:limit] + "..."


def difference(name, expected, actual):
    if expected == actual:
        return None
    at = next(
        (i for i, (a, b) in enumerate(zip(expected, actual)) if a != b),
        min(len(expected), len(actual)),
    )
    return "%s differs at byte %d: expected %s, got %s" % (
        name,
        at,
        short(expected[at : at + 40]),
        short(actual[at : at + 40]),
    )


def read(path):
    with open(path, "rb") as handle:
        return handle.read()


def copy_tree(source, target, skip=lambda name: False):
    for here, _dirs, names in os.walk(source):
        for name in names:
            if skip(os.path.relpath(os.path.join(here, name), source)):
                continue
            destination = os.path.join(target, os.path.relpath(here, source), name)
            os.makedirs(os.path.dirname(destination), exist_ok=True)
            shutil.copyfile(os.path.join(here, name), destination)


def restore(data, table):
    """Put back the bytes that a case file holds as tokens (see `bytes` in case.json).

    The repository's publication check refuses a file that is no valid UTF-8, so a
    case that needs such a byte writes a token such as {{FF}} and names its value.
    """
    for token, value in table.items():
        data = data.replace(token.encode(), bytes.fromhex(value))
    return data


def materialize(directory, target, table):
    """Make the case's input files in `target`: a trailing `.in` is dropped."""
    for here, _dirs, names in os.walk(directory):
        for name in names:
            relative = os.path.relpath(os.path.join(here, name), directory)
            if not is_input(relative):
                continue
            if relative.endswith(".in"):
                relative = relative[: -len(".in")]
            destination = os.path.join(target, relative)
            os.makedirs(os.path.dirname(destination), exist_ok=True)
            with open(destination, "wb") as handle:
                handle.write(restore(read(os.path.join(here, name)), table))


def release(directory):
    """Make a scratch directory removable again."""
    for here, dirs, names in os.walk(directory):
        for name in dirs + names:
            try:
                os.chmod(os.path.join(here, name), 0o755)
            except OSError:
                pass
    shutil.rmtree(directory, ignore_errors=True)


# ---------------------------------------------------------------------------
# Part 1: the cases


def is_input(relative):
    base = os.path.basename(relative)
    return base != "case.json" and not base.startswith("expect.")


def run_case(reader, directory):
    """The list of reasons a case does not hold (empty when it holds)."""
    with open(os.path.join(directory, "case.json"), encoding="utf-8") as handle:
        meta = json.load(handle)
    expect = meta["expect"]
    scratch = tempfile.mkdtemp(prefix="fmt-gate-")
    problems = []
    try:
        table = meta.get("bytes", {})
        materialize(directory, scratch, table)
        inputs = {}
        for here, _dirs, names in os.walk(scratch):
            for name in names:
                path = os.path.join(here, name)
                inputs[os.path.relpath(path, scratch)] = read(path)
        for name in meta.get("readonly", []):
            if os.geteuid() == 0:
                return ["cannot make %s unwritable as the superuser" % name]
            os.chmod(os.path.join(scratch, name), 0o444)

        check_status, check_err = None, b""
        if meta.get("check_args"):
            check_status, _out, check_err = run([reader] + meta["check_args"], scratch)
            if meta["premise"] == "accepted" and check_status != 0:
                return [
                    "premise: the reader's check does not accept the input (status %d)"
                    % check_status
                ]
            if meta["premise"] == "failed" and check_status == 0:
                return ["premise: the reader's check accepts the input"]
            if meta["premise"] == "refused" and check_status != 1:
                return [
                    "premise: the reader's check does not refuse the input (status %d)"
                    % check_status
                ]

        status, out, err = run([reader] + meta["args"], scratch)

        want_out = (
            restore(read(os.path.join(directory, expect["stdout"])), table)
            if "stdout" in expect
            else b""
        )
        if expect.get("as_check"):
            want_err, want_status = check_err, check_status
        else:
            want_err = (
                restore(read(os.path.join(directory, expect["stderr"])), table)
                if "stderr" in expect
                else b""
            )
            want_status = expect["exit"]
        if status != want_status:
            problems.append("exit status expected %d, got %d" % (want_status, status))
        for line in (
            difference("stdout", want_out, out),
            difference("stderr", want_err, err),
        ):
            if line:
                problems.append(line)
        for name, before in inputs.items():
            label = expect.get("files", {}).get(name)
            want = (
                restore(read(os.path.join(directory, label)), table)
                if label
                else before
            )
            line = difference("file %s" % name, want, read(os.path.join(scratch, name)))
            if line:
                problems.append(line)
    finally:
        release(scratch)
    return problems


def part_one(reader, cases_dir, only, jobs):
    names = sorted(
        n
        for n in os.listdir(cases_dir)
        if os.path.isfile(os.path.join(cases_dir, n, "case.json")) and only in n
    )
    with concurrent.futures.ThreadPoolExecutor(jobs) as pool:
        results = list(
            pool.map(lambda n: run_case(reader, os.path.join(cases_dir, n)), names)
        )
    held = 0
    for name, problems in zip(names, results):
        if problems:
            print("FAIL %s: %s" % (name, "; ".join(problems)))
        else:
            held += 1
            print("ok   %s" % name)
    print("cases: %d, hold: %d, fail: %d" % (len(names), held, len(names) - held))
    return len(names), held


# ---------------------------------------------------------------------------
# Part 2: M5 on the corpus, the ask gate and the variants of law L3


def split_positionals(args):
    positional, skip = [], False
    for arg in args:
        if skip:
            skip = False
        elif arg == "--facts":
            skip = True
        elif not arg.startswith("--"):
            positional.append(arg)
    return positional


def corpus_texts(corpus_dir, ask_dir):
    """Groups of texts: (directory, [(declarations, language or None, file)])."""
    groups = []
    for case in sorted(os.listdir(corpus_dir)):
        base = os.path.join(corpus_dir, case)
        if not os.path.isdir(base):
            continue
        with open(os.path.join(base, "expected.json"), encoding="utf-8") as handle:
            expected = json.load(handle)
        runs = [expected] + [
            s for s in expected.get("subcases") or [] if isinstance(s, dict)
        ]
        found = set()
        for item in runs:
            invocation = item.get("invocation")
            if not invocation or len(invocation) < 3:
                continue
            positional = split_positionals(invocation[2:])
            if invocation[1] in ("check", "plan") and len(positional) == 3:
                found.add((positional[0], positional[1], positional[2]))
            programs = (item.get("stdout_json") or {}).get("programs")
            if invocation[1] in ("plan", "tree") and programs and positional:
                for program in programs:
                    found.add((positional[0], program["language"], program["path"]))
        for here, _dirs, names in os.walk(base):
            for name in sorted(names):
                relative = os.path.relpath(os.path.join(here, name), base)
                if name.endswith(".typify") and read(
                    os.path.join(here, name)
                ).startswith(b"#lang typify"):
                    found.add((relative, None, relative))
        groups.append((base, sorted(found, key=lambda t: (t[0], t[1] or "", t[2]))))
    declarations = os.path.join(ask_dir, "declarations")
    if os.path.isdir(declarations):
        found = [
            (os.path.join("declarations", n), None, os.path.join("declarations", n))
            for n in sorted(os.listdir(declarations))
            if n.endswith(".typify")
        ]
        groups.append((ask_dir, found))
    return groups


def lines_with_kinds(data, prefix):
    """(raw line, kind, left, right) for each line, by the reference's reading."""
    out = []
    for raw in ref_fmt.split_lines(data):
        line = ref_fmt.strip_blanks(raw)
        if not line:
            out.append((raw, "blank", b"", b""))
        elif prefix and line.startswith(prefix):
            out.append((raw, "comment", line, b""))
        else:
            colon = line.find(b":")
            arrow = line.find(b"->") if colon < 0 else -1
            if colon > 0:
                out.append(
                    (
                        raw,
                        "colon",
                        ref_fmt.strip_blanks(line[:colon]),
                        ref_fmt.strip_blanks(line[colon + 1 :]),
                    )
                )
            elif arrow > 0 and ref_fmt.strip_blanks(line[arrow + 2 :]):
                out.append(
                    (
                        raw,
                        "arrow",
                        ref_fmt.strip_blanks(line[:arrow]),
                        ref_fmt.strip_blanks(line[arrow + 2 :]),
                    )
                )
            else:
                out.append((raw, "bang", line, b""))
    return out


def render(items, sep, outer, end, runs, header_first):
    """A variant of a text: `sep(kind)` gives the separator, `outer(i)` the blanks,
    `runs` how many blank lines each blank line of the text grows by."""
    lines = []
    for index, (raw, kind, left, right) in enumerate(items):
        pad_left, pad_right = outer(index)
        if header_first and index == 0:
            pad_left = b""
        if kind == "blank":
            # law L3 as corrected: a run grows where a blank line stands, and no
            # blank line is made where the text has none
            lines.append(raw)
            for extra in range(runs):
                lines.append(b" \t" if extra % 2 else b"")
            continue
        if kind in ("comment", "bang"):
            body = left
        elif kind == "colon":
            a, b = sep(kind)
            body = left + a + b":" + (b + right if right else b"")
        else:
            a, b = sep(kind)
            body = left + a + b"->" + b + right
        lines.append(pad_left + body + pad_right)
    return lines


def variants(data, prefix, is_declarations):
    """The variants of a text that law L3 names, as (name, bytes)."""
    items = lines_with_kinds(data, prefix)
    none = lambda i: (b"", b"")  # noqa: E731
    mixed = lambda i: ((b"  ", b"\t "), (b"\t", b" "), (b"    ", b""))[i % 3]  # noqa: E731
    spaced = lambda kind: (b" ", b" ")  # noqa: E731
    tight = lambda kind: (b"", b"")  # noqa: E731
    tabs = lambda kind: (b"\t", b"\t")  # noqa: E731
    wide = lambda kind: (b"   ", b"  ")  # noqa: E731
    header = is_declarations
    forms = [
        ("crlf", render(items, spaced, none, b"\r\n", 0, header), b"\r\n"),
        ("no-last-newline", render(items, spaced, none, b"\n", 0, header), None),
        ("blank-lines", render(items, spaced, none, b"\n", 2, header), b"\n"),
        ("outer-blanks", render(items, spaced, mixed, b"\n", 0, header), b"\n"),
        ("tight-separators", render(items, tight, none, b"\n", 0, header), b"\n"),
        ("tab-separators", render(items, tabs, none, b"\n", 0, header), b"\n"),
        ("wide-separators", render(items, wide, none, b"\n", 0, header), b"\n"),
        ("all-together", render(items, tabs, mixed, b"\r\n", 3, header), None),
    ]
    out = []
    for name, lines, end in forms:
        if end is None:
            text = b"\n".join(lines)
            if name == "all-together":
                text = b"\r\n".join(lines)
        else:
            text = b"".join(line + end for line in lines)
        out.append((name, text))
    # blank lines at both ends (a declarations file keeps its header on line 1)
    body = b"".join(
        line + b"\n" for line in render(items, spaced, none, b"\n", 0, header)
    )
    if not is_declarations:
        out.append(("blank-lines-at-both-ends", b"\n \n\n" + body + b"\n\t\n\n"))
    else:
        out.append(("blank-lines-at-the-end", body + b"\n \n\n"))
    return out


def language_names(decl_data):
    names = []
    for raw in ref_fmt.split_lines(decl_data):
        line = ref_fmt.strip_blanks(raw)
        colon = line.find(b":")
        if colon > 0 and not line.startswith(b"#"):
            if ref_fmt.strip_blanks(line[:colon]) == b"declare":
                names.append(
                    ref_fmt.strip_blanks(line[colon + 1 :]).decode("utf-8", "replace")
                )
    return names


class Counts:
    def __init__(self):
        self.compared = 0
        self.equal = 0
        self.refused = 0
        self.absent = 0
        self.left_out = 0
        self.mismatches = []


def projection(reader, scratch, decl, lang, target):
    """What the reader reads of a text, positions out: what `read alike` means here."""
    if lang is None:
        names = language_names(read(os.path.join(scratch, decl)))
        parts = []
        for name in names:
            status, out, _err = run([reader, "emit", decl, name, "--json"], scratch)
            parts.append((name, status, out))
        return parts
    status, out, _err = run([reader, "check", decl, lang, target, "--json"], scratch)
    try:
        report = json.loads(out)
    except ValueError:
        return (status, out)
    return (status, report.get("ok"), report.get("terms"))


def one_text(reader, base, decl, lang, target, counts, lock):
    scratch = tempfile.mkdtemp(prefix="fmt-gate-m5-")
    try:
        copy_tree(base, scratch)
        path = os.path.join(scratch, target)
        if not os.path.isfile(path):
            with lock:
                counts.absent += 1
            return
        original = read(path)
        check = [reader, "check", decl] + ([lang, target] if lang else [])
        if run(check, scratch)[0] != 0:
            with lock:
                counts.refused += 1
            return
        prefix = ref_fmt.DEFAULT_PREFIX
        if lang:
            prefix = ref_fmt.declared_prefix(
                read(os.path.join(scratch, decl)), lang.encode()
            )
        if ref_fmt.holds_blank(prefix):
            with lock:
                counts.refused += 1
            return
        reference_projection = projection(reader, scratch, decl, lang, target)
        texts = [("original", original)] + variants(original, prefix, lang is None)
        for name, text in texts:
            with open(path, "wb") as handle:
                handle.write(text)
            if name != "original":
                # `check --json` of a program carries its own status, so it is run once
                same = projection(reader, scratch, decl, lang, target)
                accepted = same[1] if lang else run(check, scratch)[0] == 0
                if not accepted or same != reference_projection:
                    with lock:
                        counts.left_out += 1
                    continue
            argv = [reader, "fmt", decl] + ([lang, target] if lang else [])
            status, out, _err = run(argv, scratch)
            expected = ref_fmt.canonical(text, prefix)
            with lock:
                counts.compared += 1
                if status == 0 and out == expected:
                    counts.equal += 1
                else:
                    counts.mismatches.append(
                        "%s %s (%s): status %d, %s"
                        % (
                            os.path.basename(base),
                            target,
                            name,
                            status,
                            difference("stdout", expected, out) or "equal",
                        )
                    )
    finally:
        release(scratch)


def part_two(reader, jobs):
    import threading

    groups = corpus_texts(os.path.join(ROOT, "corpus"), os.path.join(ROOT, "ask-gate"))
    counts, lock = Counts(), threading.Lock()
    work = [
        (base, decl, lang, target)
        for base, found in groups
        for decl, lang, target in found
    ]

    def guarded(item):
        try:
            one_text(reader, item[0], item[1], item[2], item[3], counts, lock)
        except Exception as error:  # a text that breaks the gate is a text that fails
            with lock:
                counts.compared += 1
                counts.mismatches.append(
                    "%s %s: the gate could not compare it: %r"
                    % (os.path.basename(item[0]), item[3], error)
                )

    with concurrent.futures.ThreadPoolExecutor(jobs) as pool:
        list(pool.map(guarded, work))
    for line in sorted(counts.mismatches)[:20]:
        print("M5 differs: %s" % line)
    if len(counts.mismatches) > 20:
        print("M5 differs: ... and %d more" % (len(counts.mismatches) - 20))
    print(
        "m5: texts compared: %d, equal: %d (variants left out because they read "
        "otherwise: %d; texts that the reader's check refuses: %d; paths that name "
        "no file: %d)"
        % (
            counts.compared,
            counts.equal,
            counts.left_out,
            counts.refused,
            counts.absent,
        )
    )
    return counts


def main(argv):
    parser = argparse.ArgumentParser(description="gate of `typify fmt`")
    parser.add_argument("reader", help="path of the reader to measure")
    parser.add_argument("--cases", default=os.path.join(HERE, "cases"))
    parser.add_argument("--jobs", type=int, default=max(1, (os.cpu_count() or 2) - 1))
    parser.add_argument(
        "--only", default="", help="run the cases whose name holds TEXT"
    )
    parser.add_argument("--no-m5", action="store_true", help="skip part 2")
    args = parser.parse_args(argv)
    reader = os.path.abspath(args.reader)
    total, held = part_one(reader, args.cases, args.only, args.jobs)
    good = held == total and total > 0
    if not args.no_m5:
        counts = part_two(reader, args.jobs)
        good = good and counts.compared > 0 and counts.equal == counts.compared
    print("gate: %s" % ("every case holds" if good else "does not hold"))
    return 0 if good else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
