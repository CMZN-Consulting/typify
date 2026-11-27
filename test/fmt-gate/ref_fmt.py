#!/usr/bin/env python3
"""Reference formatter for `typify fmt`.

Written from the text of the formatter's definition alone (sections 1 to 11),
in Python with the standard library only. It works on bytes. It implements
the canonical form of section 3.2, the empty text of section 9 point 2, the
bytes rule of point 7, and the refusal under a comment prefix that holds a
blank (point 1). Whether a text is accepted is the reader's `check`; this file
is only ever given an accepted text.

Usage:
    ref_fmt.py DECLARATIONS               canonical form of a declarations file
    ref_fmt.py DECLARATIONS LANGUAGE PROGRAM
                                          canonical form of a program under the
                                          comment prefix its language declares
"""

import os
import sys

BLANKS = b" \t"
DEFAULT_PREFIX = b"#"
BLANK_PREFIX_REFUSAL = (
    b":0:0: error: fmt does not format a text whose comment prefix holds a blank "
    b"[E_USAGE]\n"
)


def split_lines(data):
    """The lines of a text without their line ends (LF, or CR directly before LF).

    A last line without a line end is a line. A CR that is no part of a line
    end stays in its line.
    """
    parts = data.split(b"\n")
    last = parts.pop()
    lines = [p[:-1] if p.endswith(b"\r") else p for p in parts]
    if last:
        lines.append(last)
    return lines


def strip_blanks(line):
    """Remove U+0020 and U+0009 from both ends, and no other character."""
    return line.strip(BLANKS)


def format_line(line, prefix):
    """The canonical spelling of one non-blank line whose outer blanks are gone."""
    if prefix and line.startswith(prefix):
        return line
    colon = line.find(b":")
    if colon > 0:
        left = strip_blanks(line[:colon])
        right = strip_blanks(line[colon + 1 :])
        return left + b" : " + right if right else left + b" :"
    if colon < 0:
        arrow = line.find(b"->")
        if arrow > 0:
            left = strip_blanks(line[:arrow])
            right = strip_blanks(line[arrow + 2 :])
            if left and right:
                return left + b" -> " + right
    return line


def canonical(data, prefix=DEFAULT_PREFIX):
    """C(t) for the bytes of an accepted text under a comment prefix.

    `prefix` is the declared prefix: b"#" when the body has no comment line,
    b"" when it is `comment :` (no comment lines).
    """
    out = []
    pending_blank = False
    for raw in split_lines(data):
        line = strip_blanks(raw)
        if not line:
            pending_blank = bool(out)
            continue
        if pending_blank:
            out.append(b"")
            pending_blank = False
        out.append(format_line(line, prefix))
    return b"".join(line + b"\n" for line in out)


def declared_prefix(declarations, language):
    """The comment prefix that the body named `language` declares.

    The declarations file is read under the reader's own declaration, whose
    prefix is `#`. A body opens with `declare : NAME` and closes with `end!`.
    """
    inside = False
    prefix = DEFAULT_PREFIX
    for raw in split_lines(declarations):
        line = strip_blanks(raw)
        if not line or line.startswith(DEFAULT_PREFIX):
            continue
        colon = line.find(b":")
        if colon > 0:
            left = strip_blanks(line[:colon])
            right = strip_blanks(line[colon + 1 :])
            if left == b"declare":
                inside = right == language
            elif inside and left == b"comment":
                prefix = right
        elif line == b"end!":
            inside = False
    return prefix


def holds_blank(prefix):
    return b" " in prefix or b"\t" in prefix


def main(argv):
    if len(argv) not in (1, 3):
        sys.stderr.write("usage: ref_fmt.py DECLARATIONS [LANGUAGE PROGRAM]\n")
        return 2
    with open(argv[0], "rb") as handle:
        data = handle.read()
    prefix = DEFAULT_PREFIX
    if len(argv) == 3:
        with open(argv[2], "rb") as handle:
            program = handle.read()
        prefix = declared_prefix(data, os.fsencode(argv[1]))
        if holds_blank(prefix):
            sys.stderr.buffer.write(BLANK_PREFIX_REFUSAL)
            return 2
        data = program
    sys.stdout.buffer.write(canonical(data, prefix))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
