#!/usr/bin/env python3
"""Writes the cases of the `typify fmt` gate into a directory (default: cases/).

Every expectation below is written by hand from the text of the formatter's
definition (sections 1 to 11), with the number of the rule or ruling beside it.
Nothing is taken from the output of the reference formatter or of any reader.
The directory `cases/` is this script's output, and test_gate.py checks that
the two agree.

Case directory layout:
    case.json         the run and the expectation
    <name>.in         an input file: the run reads it as <name>
    expect.stdout     expected standard output (absent: empty)
    expect.stderr     expected standard error (absent: empty)
    expect.after.<n>.in
                      expected bytes of input file <n> after the run (an input
                      file with no such file must stay as it was)
    "bytes" in case.json names the tokens that stand for bytes that are no UTF-8
"""

import json
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))

USAGE = (
    ":0:0: error: usage: typify fmt DECLARATIONS [LANGUAGE PROGRAM] "
    "[--write | --check] [E_USAGE]\n"
)
BLANK_PREFIX = (
    ":0:0: error: fmt does not format a text whose comment prefix holds a blank "
    "[E_USAGE]\n"
)
NOT_CANONICAL = ": not in canonical form\n"


def text(lines, end="\n"):
    """Lines joined and ended by `end` (a str; the cases are ASCII unless said)."""
    return "".join(line + end for line in lines).encode("utf-8", "surrogateescape")


def raw(*parts):
    """Bytes from str parts (UTF-8) and bytes parts, as given."""
    return b"".join(p if isinstance(p, bytes) else p.encode("utf-8") for p in parts)


# ---------------------------------------------------------------------------
# The declarations that the cases read programs under.


def shapes(comment=None, hash_form=False, name="Shapes", file="shapes.typify"):
    head = ["#lang typify", f"declare : {name}", f"file : {file}"]
    if comment is not None:
        head.append(f"comment :{comment}")
    body = [
        "is : label = /[A-Za-z0-9_:\\/ >.!=#%-]+/",
        "is : words = /[a-z ]*/",
        "is : node = /[a-z][a-z0-9_-]*/",
        "is : free = /.+/",
        "term : ready",
        "term : note-line : label",
        "term : memo : words",
        "term : link : node label",
        "term : raw : free",
    ]
    if hash_form:
        body.append("term : hash-tag : node")
    body += [
        "form : ready!",
        "does : ready",
        "example : ready!",
        "form : note-line : <label>",
        "does : note-line <label>",
        "example : note-line : hello : world -> arrow-inside-colon",
        "form : memo : <words>",
        "does : memo <words>",
        "example : memo : some words",
        "form : <node> -> <label>",
        "does : link <node> <label>",
        "example : alpha -> beta -> gamma",
        "form : raw : <free>",
        "does : raw <free>",
        "example : raw : anything",
    ]
    if hash_form:
        body += [
            "form : #<node> : go",
            "does : hash-tag <node>",
            "example : #here : go",
        ]
    return text(head + body + ["end!"])


SHAPES = shapes()
SLASH = shapes(" //", hash_form=True)
NONE = shapes("", hash_form=True)
BLANK = shapes(" x :")
AFIX = shapes(
    " a:"
).replace(  # a prefix `a:` and one more form whose lines start with `a : `
    b"end!\n",
    b"form : a : <words>\ndoes : memo <words>\nexample : a : b\nend!\n",
)
DASH = shapes(" a-")

TWO = text(
    [
        "#lang typify",
        "declare : Shapes",
        "file : shapes.typify",
        "is : words = /[a-z ]*/",
        "term : memo : words",
        "form : memo : <words>",
        "does : memo <words>",
        "example : memo : some words",
        "end!",
        "declare : Marked",
        "file : marked.typify",
        "comment : x :",
        "is : words = /[a-z ]*/",
        "term : memo : words",
        "form : memo : <words>",
        "does : memo <words>",
        "example : memo : some words",
        "end!",
    ]
)

# The declarations file that the declaration cases format.
CATALOG = text(
    [
        "#lang typify",
        "",
        "declare : Catalog",
        "file : catalog.typify",
        "is : title = /[^=]+/",
        'is : category = "book" | "journal"',
        "term : item : category title",
        "form : item : <category> = <title>",
        "does : item <category> <title>",
        "example : item : book = Algebra",
        "",
        "count : item = 1..*",
        "end!",
    ]
)

# The program used by the check cases, in its canonical form (7 lines).
P1 = text(
    [
        "# a comment",
        "ready!",
        "note-line : first : second -> third",
        "memo :",
        "",
        "memo : some words",
        "alpha -> beta -> gamma",
    ]
)

CASES = []


def case(
    cid,
    title,
    cites,
    files,
    args,
    check=None,
    premise="accepted",
    stdout=b"",
    stderr=b"",
    exit=0,
    after=None,
    as_check=False,
    readonly=(),
):
    """Register a case. `check` is the argument list of the reader's own `check`."""
    CASES.append(
        dict(
            id=cid,
            title=title,
            cites=cites,
            files=files,
            args=args,
            check=check,
            premise=premise,
            stdout=stdout,
            stderr=stderr,
            exit=exit,
            after=after or {},
            as_check=as_check,
            readonly=list(readonly),
        )
    )


S = "shapes-language.typify"
PRG = "shapes.typify"


def prog(cid, title, cites, source, expected, decl=SHAPES, decl_name=S, lang="Shapes"):
    """A plain `fmt DECLARATIONS LANGUAGE PROGRAM` case: output is the expectation."""
    files = {decl_name: decl, PRG: source}
    check = ["check", decl_name, lang, PRG]
    case(
        cid, title, cites, files, ["fmt", decl_name, lang, PRG], check, stdout=expected
    )


def decl(cid, title, cites, source, expected, name="catalog-language.typify"):
    case(
        cid,
        title,
        cites,
        {name: source},
        ["fmt", name],
        ["check", name],
        stdout=expected,
    )


# ---------------------------------------------------------------------------
# Declarations files, plain mode: the eight rules of 3.2.

CAT = CATALOG
decl(
    "d01-declarations-crlf",
    "every line of a declarations file ends with one LF: CR LF becomes LF",
    ["3.2 rule 1", "3.1 row 1 ignored", "2 line end"],
    CAT.replace(b"\n", b"\r\n"),
    CAT,
)
decl(
    "d02-declarations-no-last-newline",
    "the text ends with exactly one LF: a missing last newline is added",
    ["3.2 rule 1", "3.1 row 2 ignored"],
    CAT[:-1],
    CAT,
)
decl(
    "d03-declarations-many-final-newlines",
    "the text ends with exactly one LF: blank lines at the end are dropped",
    ["3.2 rule 1", "3.2 rule 3", "3.1 row 2 ignored"],
    CAT + b"\n\n  \n\t\n",
    CAT,
)
decl(
    "d04-declarations-outer-blanks",
    "no line is indented and no line keeps blanks at its end, line 1 included",
    ["3.2 rule 2", "3.1 row 4 ignored"],
    raw(
        "#lang typify \t\n",
        "\n",
        "  declare : Catalog\n",
        "\tfile : catalog.typify  \n",
        "    is : title = /[^=]+/\t\n",
        ' \t is : category = "book" | "journal"\n',
        "term : item : category title   \n",
        "form : item : <category> = <title>\n",
        "does : item <category> <title>\n",
        "example : item : book = Algebra\n",
        "\n",
        "count : item = 1..*\n",
        "  end!  \n",
    ),
    CAT,
)
decl(
    "d05-declarations-blank-line-runs",
    "a run of blank lines is one empty line, and a single blank line stays",
    ["3.2 rule 3", "3.1 row 3 ignored", "3.4 L3"],
    raw(
        "#lang typify\n",
        "\n",
        "\n",
        "\n",
        "declare : Catalog\n",
        "file : catalog.typify\n",
        "is : title = /[^=]+/\n",
        "  \t \n",
        'is : category = "book" | "journal"\n',
        "term : item : category title\n",
        "form : item : <category> = <title>\n",
        "does : item <category> <title>\n",
        "example : item : book = Algebra\n",
        "\n",
        "\t\n",
        "\n",
        "count : item = 1..*\n",
        "end!\n",
    ),
    raw(
        "#lang typify\n",
        "\n",
        "declare : Catalog\n",
        "file : catalog.typify\n",
        "is : title = /[^=]+/\n",
        "\n",
        'is : category = "book" | "journal"\n',
        "term : item : category title\n",
        "form : item : <category> = <title>\n",
        "does : item <category> <title>\n",
        "example : item : book = Algebra\n",
        "\n",
        "count : item = 1..*\n",
        "end!\n",
    ),
)
decl(
    "d06-declarations-first-separator",
    "a colon line is LEFT : RIGHT: tight, loose and tab-spaced first colons",
    ["3.2 rule 5", "3.1 row 5 ignored"],
    raw(
        "#lang typify\n",
        "declare:Catalog\n",
        "file\t:\tcatalog.typify\n",
        "is   :   title = /[^=]+/\n",
        'is\t: category = "book" | "journal"\n',
        "term :item : category title\n",
        "form: item : <category> = <title>\n",
        "does :\titem <category> <title>\n",
        "example   :   item : book = Algebra\n",
        "count : item = 1..*\n",
        "end!\n",
    ),
    raw(
        "#lang typify\n",
        "declare : Catalog\n",
        "file : catalog.typify\n",
        "is : title = /[^=]+/\n",
        'is : category = "book" | "journal"\n',
        "term : item : category title\n",
        "form : item : <category> = <title>\n",
        "does : item <category> <title>\n",
        "example : item : book = Algebra\n",
        "count : item = 1..*\n",
        "end!\n",
    ),
)
decl(
    "d07-declarations-payload-untouched",
    "blanks inside a payload and around its second colon stay as written",
    ["3.2 rule 5", "3.2 rule 8", "2 payload", "3.1 row 3 read"],
    raw(
        "#lang typify\n",
        "declare:Catalog\n",
        "file : catalog.typify\n",
        "is : title = /[^=]+/\n",
        'is : category = "book" | "journal"\n',
        "term : item : category title\n",
        "form : item : <category> = <title>\n",
        "does : item <category> <title>\n",
        "example : item : book = Algebra\n",
        "  note :  item =   a  b  :  c -> d   e   \n",
        "count : item = 1..*\n",
        "end!\n",
    ),
    raw(
        "#lang typify\n",
        "declare : Catalog\n",
        "file : catalog.typify\n",
        "is : title = /[^=]+/\n",
        'is : category = "book" | "journal"\n',
        "term : item : category title\n",
        "form : item : <category> = <title>\n",
        "does : item <category> <title>\n",
        "example : item : book = Algebra\n",
        "note : item =   a  b  :  c -> d   e\n",
        "count : item = 1..*\n",
        "end!\n",
    ),
)
decl(
    "d08-declarations-empty-right-side",
    "a colon line with an empty right side is LEFT :",
    ["3.2 rule 5"],
    raw(
        "#lang typify\n",
        "declare : Catalog\n",
        "comment:   \n",
        "file : catalog.typify\n",
        "end!\n",
    ),
    raw(
        "#lang typify\n",
        "declare : Catalog\n",
        "comment :\n",
        "file : catalog.typify\n",
        "end!\n",
    ),
)
decl(
    "d09-declarations-bang-line",
    "a bang line is unchanged, apart from the blanks at its ends",
    ["3.2 rule 7", "3.2 rule 2"],
    raw("#lang typify\n", "declare : Catalog\n", "\t end!\t \n"),
    raw("#lang typify\n", "declare : Catalog\n", "end!\n"),
)
decl(
    "d10-declarations-order-and-count",
    "no line moves, goes or is added: a body whose lines stand out of the usual order keeps it",
    ["3.2 rule 8", "3.1 row 6 read", "1 no sorter"],
    raw(
        "#lang typify\n",
        "declare : Catalog\n",
        "  count : item = 1..*\n",
        "term : item : category title\n",
        "is : title = /[^=]+/\n",
        "file : catalog.typify\n",
        'is : category = "book" | "journal"\n',
        "form : item : <category> = <title>\n",
        "does : item <category> <title>\n",
        "example : item : book = Algebra\n",
        "end!\n",
    ),
    raw(
        "#lang typify\n",
        "declare : Catalog\n",
        "count : item = 1..*\n",
        "term : item : category title\n",
        "is : title = /[^=]+/\n",
        "file : catalog.typify\n",
        'is : category = "book" | "journal"\n',
        "form : item : <category> = <title>\n",
        "does : item <category> <title>\n",
        "example : item : book = Algebra\n",
        "end!\n",
    ),
)
decl(
    "d11-declarations-specimen",
    "the specimen of section 6, loose text to canonical form",
    ["6 specimen", "3.2"],
    raw(
        "#lang typify\n",
        "\n",
        "declare:Catalog\n",
        "  file : catalog.typify\n",
        '  is   : category = "book" | "journal"\n',
        "  is   : title = /[^=]+/\n",
        "  term : item : category title\n",
        "  form : item : <category> = <title>\n",
        "  does : item <category> <title>\n",
        "  example : item : book = Algebra\n",
        "\n",
        "\n",
        "  count : item = 1..*\n",
        "end!",
    ),
    raw(
        "#lang typify\n",
        "\n",
        "declare : Catalog\n",
        "file : catalog.typify\n",
        'is : category = "book" | "journal"\n',
        "is : title = /[^=]+/\n",
        "term : item : category title\n",
        "form : item : <category> = <title>\n",
        "does : item <category> <title>\n",
        "example : item : book = Algebra\n",
        "\n",
        "count : item = 1..*\n",
        "end!\n",
    ),
)
decl(
    "d12-declarations-already-canonical",
    "a canonical text is its own form",
    ["3.4 L2"],
    CAT,
    CAT,
)
decl(
    "d13-declarations-header-outer-blanks",
    "line 1 of a declarations file loses the blanks at its two ends and keeps its text",
    ["3.2 rule 4", "3.2 rule 2", "3.1 row 4 read"],
    raw("\t  #lang typify \t \n", "declare : A\n", "end!\n"),
    raw("#lang typify\n", "declare : A\n", "end!\n"),
)
decl(
    "d14-declarations-two-bodies",
    "a declarations file with two bodies is formatted as one text",
    ["3.2", "3.3 first bullet"],
    TWO.replace(b"end!\n", b"  end!  \n\n\n"),
    TWO.replace(b"end!\ndeclare", b"end!\n\ndeclare"),
)

# ---------------------------------------------------------------------------
# Programs under their declarations, plain mode: the eight rules of 3.2.

P1_LOOSE = raw(
    "# a comment\n",
    "ready!\n",
    "note-line : first : second -> third\n",
    "memo :\n",
    "\n",
    "memo : some words\n",
    "alpha -> beta -> gamma\n",
)

prog(
    "p01-program-crlf",
    "every line ends with one LF: CR LF becomes LF in a program",
    ["3.2 rule 1", "3.1 row 1 ignored"],
    P1.replace(b"\n", b"\r\n"),
    P1,
)
prog(
    "p02-program-no-last-newline",
    "a program without a last newline gets one",
    ["3.2 rule 1", "3.1 row 2 ignored"],
    P1[:-1],
    P1,
)
prog(
    "p03-program-many-final-newlines",
    "blank lines after the last line are dropped",
    ["3.2 rule 3", "3.2 rule 1"],
    P1 + b"\n\n \t \n\n",
    P1,
)
prog(
    "p04-program-outer-blanks",
    "no program line is indented or keeps blanks at its end",
    ["3.2 rule 2", "3.1 row 4 ignored"],
    raw(
        "   # a comment   \n",
        "\tready!\t\n",
        " \t note-line : first : second -> third \t \n",
        "memo :\t \n",
        "\n",
        "    memo : some words\n",
        "\t\talpha -> beta -> gamma\n",
    ),
    P1,
)
prog(
    "p05-program-blank-lines",
    "runs of blank lines fold to one, none stays before the first or after the last",
    ["3.2 rule 3", "3.1 row 3 ignored", "3.4 L3"],
    raw(
        "\n",
        "  \n",
        "\n",
        "# a comment\n",
        "\n",
        "ready!\n",
        "\n",
        "\n",
        "note-line : first : second -> third\n",
        "memo :\n",
        "\n",
        "\n",
        "\t\n",
        "\n",
        "memo : some words\n",
        "alpha -> beta -> gamma\n",
        "\n",
        "\n",
    ),
    raw(
        "# a comment\n",
        "\n",
        "ready!\n",
        "\n",
        "note-line : first : second -> third\n",
        "memo :\n",
        "\n",
        "memo : some words\n",
        "alpha -> beta -> gamma\n",
    ),
)
prog(
    "p06-program-comment-lines",
    "a comment line is its text from the prefix on: only its outer blanks go",
    ["3.2 rule 4", "3.1 row 6 ignored", "2 kernel line"],
    raw(
        "   # indented comment : with -> arrows and   spaces   \n",
        "ready!\n",
        "\t#\ttab inside\tthe comment\t\n",
        "#here  :  go\n",
        "  #\n",
    ),
    raw(
        "# indented comment : with -> arrows and   spaces\n",
        "ready!\n",
        "#\ttab inside\tthe comment\n",
        "#here  :  go\n",
        "#\n",
    ),
)
prog(
    "p07-program-comment-blank-lookalikes",
    "a comment keeps U+00A0, U+3000, U+FEFF and form feed: they are no blanks",
    ["3.1 row 5 read", "2 blank", "3.2 rule 4"],
    raw(
        "  # nb\u00a0\n",
        "#\ufeff  bom\ufeff \n",
        "\t# ff\x0c \t\n",
        "# ideographic\u3000\n",
        "ready!\n",
    ),
    raw(
        "# nb\u00a0\n",
        "#\ufeff  bom\ufeff\n",
        "# ff\x0c\n",
        "# ideographic\u3000\n",
        "ready!\n",
    ),
)
prog(
    "p08-program-colon-lines",
    "a colon line is LEFT : RIGHT: tight, loose and tab-spaced first colons",
    ["3.2 rule 5", "3.1 row 5 ignored"],
    raw(
        "note-line:hello\n",
        "note-line   :   hello\n",
        "note-line\t:\thello\n",
        "  memo :some words  \n",
        "memo: some words\n",
    ),
    raw(
        "note-line : hello\n",
        "note-line : hello\n",
        "note-line : hello\n",
        "memo : some words\n",
        "memo : some words\n",
    ),
)
prog(
    "p09-program-empty-right-side",
    "a colon line with an empty right side is LEFT :",
    ["3.2 rule 5"],
    raw("memo:\n", "  memo :   \n", "memo\t:\t\n"),
    raw("memo :\n", "memo :\n", "memo :\n"),
)
prog(
    "p10-program-payload-untouched",
    "blanks inside a payload, around its second colon and arrow stay as written",
    ["3.2 rule 5", "3.2 rule 8", "2 payload", "3.1 row 3 read"],
    raw(
        "note-line :  first  :  second  ->  third  \n",
        "note-line:a  b   c\n",
        "memo :  some   words \n",
        "note-line : a -> b\n",
    ),
    raw(
        "note-line : first  :  second  ->  third\n",
        "note-line : a  b   c\n",
        "memo : some   words\n",
        "note-line : a -> b\n",
    ),
)
prog(
    "p11-program-arrow-lines",
    "an arrow line is LEFT -> RIGHT, and a second arrow belongs to the right payload",
    ["3.2 rule 6", "2 first separator", "3.1 row 5 ignored"],
    raw(
        "alpha->beta\n",
        "alpha\t->\tbeta\n",
        "  alpha   ->   beta   \n",
        "alpha  ->  beta  ->  gamma\n",
    ),
    raw(
        "alpha -> beta\n",
        "alpha -> beta\n",
        "alpha -> beta\n",
        "alpha -> beta  ->  gamma\n",
    ),
)
prog(
    "p12-program-bang-lines",
    "a bang line is unchanged, apart from the blanks at its ends",
    ["3.2 rule 7", "3.2 rule 2"],
    raw("ready!\n", "  ready!  \n", "\tready!\t\n"),
    raw("ready!\n", "ready!\n", "ready!\n"),
)
prog(
    "p13-program-order-and-count",
    "no line moves, goes or is added: repeats and unsorted lines stay",
    ["3.2 rule 8", "3.1 row 6 read", "1 no sorter"],
    raw(
        "memo : b\n",
        "ready!\n",
        " memo : b\n",
        "# note\n",
        "  # note\n",
        "ready!\n",
        "alpha -> beta\n",
        "# note\n",
    ),
    raw(
        "memo : b\n",
        "ready!\n",
        "memo : b\n",
        "# note\n",
        "# note\n",
        "ready!\n",
        "alpha -> beta\n",
        "# note\n",
    ),
)
prog(
    "p14-program-already-canonical",
    "a canonical program is its own form",
    ["3.4 L2"],
    P1,
    P1,
)
prog(
    "p15-program-comment-prefix-declared",
    "a declared prefix `//`: its lines are comments, a `#` line is content",
    ["2 kernel line", "3.2 rule 4", "3.2 rule 5", "3.2 body that governs"],
    raw(
        "  //  a comment : with -> arrows  \n",
        "#here  :  go\n",
        "ready!\n",
        "//\n",
    ),
    raw("//  a comment : with -> arrows\n", "#here : go\n", "ready!\n", "//\n"),
    decl=SLASH,
    decl_name="slash-language.typify",
)
prog(
    "p16-program-comment-prefix-absent",
    "no comment line in the body: the prefix is `#`, and `#here : go` is a comment",
    ["3 intro: `#` when no comment line stands", "3.2 rule 4"],
    raw("  #here   :   go  \n", "ready!\n"),
    raw("#here   :   go\n", "ready!\n"),
)
prog(
    "p17-program-comment-prefix-empty",
    "`comment :` declares no prefix: a line that starts with `#` is content",
    ["3 intro: none when the line is `comment :`", "3.2 rule 5"],
    raw("  #here   :   go  \n", "#here:go\n", "ready!\n"),
    raw("#here : go\n", "#here : go\n", "ready!\n"),
    decl=NONE,
    decl_name="none-language.typify",
)
prog(
    "p18-program-prefix-ending-in-colon-spaced",
    "under the prefix `a:` the program `a : b` is a colon line and its own form",
    ["11 L3 as corrected", "3.2 rule 5", "3.4 L1"],
    raw("a : b\n"),
    raw("a : b\n"),
    decl=AFIX,
    decl_name="afix-language.typify",
)
prog(
    "p19-program-prefix-ending-in-colon-tight",
    "under the prefix `a:` the program `a:b` is a comment line and its own form",
    ["11 L3 as corrected", "3.2 rule 4", "3.4 L1"],
    raw("a:b\n"),
    raw("a:b\n"),
    decl=AFIX,
    decl_name="afix-language.typify",
)
prog(
    "p20-program-prefix-ending-in-colon-loose-lines",
    "under the prefix `a:` a loose colon line is formatted, a loose comment only trimmed",
    ["11 L3 as corrected", "3.2 rules 4 and 5"],
    raw("  a  :  b  \n", "  a:b  \n", "a:  b\n"),
    raw("a : b\n", "a:b\n", "a:  b\n"),
    decl=AFIX,
    decl_name="afix-language.typify",
)
prog(
    "p21-program-prefix-ending-in-dash-spaced",
    "under the prefix `a-` the program `a -> b` is an arrow line and its own form",
    ["11 L3 as corrected", "3.2 rule 6", "3.4 L1"],
    raw("a -> b\n"),
    raw("a -> b\n"),
    decl=DASH,
    decl_name="dash-language.typify",
)
prog(
    "p22-program-prefix-ending-in-dash-tight",
    "under the prefix `a-` the program `a->b` is a comment line and its own form",
    ["11 L3 as corrected", "3.2 rule 4", "3.4 L1"],
    raw("a->b\n"),
    raw("a->b\n"),
    decl=DASH,
    decl_name="dash-language.typify",
)
prog(
    "p23-program-prefix-ending-in-dash-loose-lines",
    "under the prefix `a-` a loose arrow line is formatted, a loose comment only trimmed",
    ["11 L3 as corrected", "3.2 rules 4 and 6"],
    raw("  a  ->  b  \n", "  a->b  \n", "a->  b\n"),
    raw("a -> b\n", "a->b\n", "a->  b\n"),
    decl=DASH,
    decl_name="dash-language.typify",
)
prog(
    "p24-program-governing-body-only",
    "the prefix of the body that governs counts, not that of another body of the file",
    ["9 point 1: the body that governs the text", "3.2 rule 5"],
    raw("  memo:  a b  \n", "\n", "\n", "memo : c\n"),
    raw("memo : a b\n", "\n", "memo : c\n"),
    decl=TWO,
    decl_name="two-language.typify",
)

# bytes (9 point 7)
prog(
    "p25-program-payload-not-utf8",
    "a payload byte that is no valid UTF-8 is kept, never written as U+FFFD",
    ["9 point 7", "3.2 rule 8", "3.2 rule 5"],
    raw("  raw :  a", b"\xff", "b  \n", "raw:", b"\xfe\xff", "\n"),
    raw("raw : a", b"\xff", "b\n", "raw : ", b"\xfe\xff", "\n"),
)
prog(
    "p26-program-comment-not-utf8",
    "a comment line with bytes that are no valid UTF-8 is kept byte for byte",
    ["9 point 7", "3.2 rule 4"],
    raw(" # c", b"\xff\xfe", " \n", "ready!\n"),
    raw("# c", b"\xff\xfe", "\n", "ready!\n"),
)
case(
    "d15-declarations-payload-not-utf8",
    "a payload byte that is no valid UTF-8 in a declarations file is kept",
    ["9 point 7", "3.2 rule 5"],
    {
        "raw-language.typify": raw(
            "#lang typify\n",
            "declare : Raw\n",
            "file : raw.typify\n",
            "term : t\n",
            "form : t!\n",
            "does : t\n",
            "example : t!\n",
            "  note:t = a",
            b"\xff",
            "b  \n",
            "end!\n",
        )
    },
    ["fmt", "raw-language.typify"],
    ["check", "raw-language.typify"],
    stdout=raw(
        "#lang typify\n",
        "declare : Raw\n",
        "file : raw.typify\n",
        "term : t\n",
        "form : t!\n",
        "does : t\n",
        "example : t!\n",
        "note : t = a",
        b"\xff",
        "b\n",
        "end!\n",
    ),
)

# Empty text and blank lines alone (9 point 2).
for cid, title, source in [
    (
        "e01-empty-program",
        "the empty program is its own canonical form, zero bytes",
        b"",
    ),
    (
        "e02-blank-lines-program",
        "blank lines alone have the empty text as canonical form",
        b"\n\n  \n\t\n",
    ),
    (
        "e03-blanks-without-newline",
        "one line of blanks and no LF is the empty text too",
        b"  \t ",
    ),
    (
        "e04-blank-lines-crlf",
        "CR LF blank lines alone are the empty text too",
        b"\r\n\r\n",
    ),
]:
    prog(cid, title, ["9 point 2"], source, b"")
prog(
    "e05-comment-only-program",
    "a program of one comment line has a line: its form ends with one LF",
    ["9 point 2 (a comment line is a line)", "3.2 rule 1"],
    b"\n  # only  \n\n",
    b"# only\n",
)

# ---------------------------------------------------------------------------
# The reader's self-declaration is its own canonical form (L5).

SELF = "typify.typify"


def self_case(cid, title, cites, files, args, **kw):
    case(cid, title, cites, files, args, ["check", SELF], **kw)


# ---------------------------------------------------------------------------
# `--check` (9 point 3).

P1C = ["check", S, "Shapes", PRG]
CHK = ["fmt", S, "Shapes", PRG, "--check"]


def chk(cid, title, cites, source, line=None, path=PRG, decl=SHAPES):
    files = {S: decl, PRG: source}
    if line is None:
        case(cid, title, cites, files, CHK, P1C)
    else:
        case(
            cid,
            title,
            cites,
            files,
            CHK,
            P1C,
            stdout=f"{PRG}:{line}{NOT_CANONICAL}".encode(),
            exit=1,
        )


chk(
    "c01-check-canonical-program",
    "a program that is its own form: no output, exit 0",
    ["9 point 3", "3.3 --check"],
    P1,
)
chk(
    "c02-check-crlf-line-one",
    "a CR LF text differs on line 1: a line is compared with its end",
    ["9 point 3"],
    P1.replace(b"\n", b"\r\n"),
    1,
)
chk(
    "c03-check-trailing-blanks-line-three",
    "trailing blanks on line 3 only: line 3",
    ["9 point 3"],
    P1.replace(b"third\n", b"third  \n"),
    3,
)
chk(
    "c04-check-last-line-without-newline",
    "a last line without LF differs: line 7",
    ["9 point 3"],
    P1[:-1],
    7,
)
chk(
    "c05-check-extra-final-blank-line",
    "a line that the canonical form does not have: line 8",
    ["9 point 3"],
    P1 + b"\n",
    8,
)
chk(
    "c06-check-run-of-two-blank-lines",
    "a run of two blank lines: the first line that differs is 6",
    ["9 point 3", "3.2 rule 3"],
    P1.replace(b"memo :\n\n", b"memo :\n\n\n"),
    6,
)
chk(
    "c07-check-leading-blank-line",
    "a blank line before the first line: line 1",
    ["9 point 3", "3.2 rule 3"],
    b"\n" + P1,
    1,
)
chk(
    "c08-check-loose-first-colon",
    "a loose first colon on line 4: line 4",
    ["9 point 3", "3.2 rule 5"],
    P1.replace(b"memo :\n", b"memo:\n"),
    4,
)
chk(
    "c09-check-indented-line-two",
    "an indented line 2: line 2",
    ["9 point 3", "3.2 rule 2"],
    P1.replace(b"ready!", b"  ready!"),
    2,
)
chk(
    "c10-check-comment-trailing-blank",
    "a comment with a trailing blank on line 1: line 1",
    ["9 point 3", "3.2 rule 4"],
    P1.replace(b"# a comment\n", b"# a comment \n"),
    1,
)
chk(
    "c11-check-last-line-crlf",
    "only the last line ends in CR LF: line 7",
    ["9 point 3"],
    P1[:-1] + b"\r\n",
    7,
)
chk(
    "c12-check-loose-arrow-line-seven",
    "a loose arrow on line 7: line 7",
    ["9 point 3", "3.2 rule 6"],
    P1.replace(b"alpha -> beta", b"alpha->beta"),
    7,
)
chk(
    "c13-check-no-blank-line-stays-none",
    "no blank line between two lines is the author's: the text is its own form",
    ["9 point 3", "3.4 L3", "3.2 rule 3"],
    P1.replace(b"memo :\n\nmemo : some words", b"memo :\nmemo : some words"),
)
chk(
    "c14-check-empty-program",
    "the empty program is its own form: exit 0",
    ["9 point 2", "9 point 3"],
    b"",
)
chk(
    "c15-check-blank-lines-program",
    "blank lines alone: line 1 is a line the form does not have",
    ["9 point 2", "9 point 3"],
    b"\n\n",
    1,
)
chk(
    "c16-check-blanks-without-newline",
    "one blank line without LF: line 1",
    ["9 point 2", "9 point 3"],
    b"  ",
    1,
)

CAT_CHECK = ["fmt", "catalog-language.typify", "--check"]
case(
    "c20-check-canonical-declarations",
    "a declarations file that is its own form: no output, exit 0",
    ["9 point 3"],
    {"catalog-language.typify": CAT},
    CAT_CHECK,
    ["check", "catalog-language.typify"],
)
case(
    "c21-check-declarations-loose-colon",
    "a declarations file with a loose first colon on line 4: line 4",
    ["9 point 3", "3.2 rule 5"],
    {"catalog-language.typify": CAT.replace(b"file : catalog", b"file: catalog")},
    CAT_CHECK,
    ["check", "catalog-language.typify"],
    stdout=b"catalog-language.typify:4" + NOT_CANONICAL.encode(),
    exit=1,
)
case(
    "c22-check-path-as-given",
    "PATH is the file as the command line gives it, with its directory",
    ["9 point 3"],
    {
        "sub/catalog-language.typify": CAT.replace(
            b"catalog.typify\n", b"catalog.typify  \n"
        )
    },
    ["fmt", "sub/catalog-language.typify", "--check"],
    ["check", "sub/catalog-language.typify"],
    stdout=b"sub/catalog-language.typify:4" + NOT_CANONICAL.encode(),
    exit=1,
)
case(
    "c23-check-comment-prefix-pair-spaced",
    "under the prefix `a:` the program `a : b` is its own form: exit 0",
    ["11", "9 point 3"],
    {"afix-language.typify": AFIX, PRG: b"a : b\n"},
    ["fmt", "afix-language.typify", "Shapes", PRG, "--check"],
    ["check", "afix-language.typify", "Shapes", PRG],
)
case(
    "c24-check-comment-prefix-pair-tight",
    "under the prefix `a:` the program `a:b` is its own form: exit 0",
    ["11", "9 point 3"],
    {"afix-language.typify": AFIX, PRG: b"a:b\n"},
    ["fmt", "afix-language.typify", "Shapes", PRG, "--check"],
    ["check", "afix-language.typify", "Shapes", PRG],
)
case(
    "c25-check-comment-prefix-dash-pair",
    "under the prefix `a-` both `a -> b` and `a->b` are their own forms: exit 0",
    ["11", "9 point 3"],
    {"dash-language.typify": DASH, PRG: b"a -> b\na->b\n"},
    ["fmt", "dash-language.typify", "Shapes", PRG, "--check"],
    ["check", "dash-language.typify", "Shapes", PRG],
)

# ---------------------------------------------------------------------------
# `--write` (9 point 6).

W = ["fmt", S, "Shapes", PRG, "--write"]
case(
    "w01-write-program",
    "`--write` replaces a loose program by its form, prints nothing, exits 0",
    ["3.3 --write", "9 point 6"],
    {S: SHAPES, PRG: P1.replace(b"\n", b"\r\n") + b"\n"},
    W,
    P1C,
    after={PRG: P1},
)
case(
    "w02-write-declarations",
    "`--write` on a declarations file",
    ["3.3 --write", "9 point 6"],
    {"catalog-language.typify": CAT.replace(b"\n", b" \n")},
    ["fmt", "catalog-language.typify", "--write"],
    ["check", "catalog-language.typify"],
    after={"catalog-language.typify": CAT},
)
case(
    "w03-write-nothing-differs",
    "`--write` on a text that is its own form writes nothing: a read-only file is no error",
    ["3.3 --write: only when the form differs", "9 point 6"],
    {S: SHAPES, PRG: P1},
    W,
    P1C,
    readonly=[PRG],
)
case(
    "w04-write-cannot-be-written",
    "`--write` on a file that cannot be written: E_FILE, exit 2, the file keeps its bytes",
    ["9 point 6"],
    {S: SHAPES, PRG: P1.replace(b"memo :\n", b"memo:\n")},
    W,
    P1C,
    stderr=f"{PRG}:0:0: error: The requested file cannot be written [E_FILE]\n".encode(),
    exit=2,
    readonly=[PRG],
)
case(
    "w05-write-blank-lines-to-empty-file",
    "`--write` turns a program of blank lines into a file of zero bytes",
    ["9 point 2", "9 point 6"],
    {S: SHAPES, PRG: b"\n\n  \n"},
    W,
    P1C,
    after={PRG: b""},
)
case(
    "w06-write-path-with-directory",
    "`--write` on a file named with a directory writes that file",
    ["9 point 6"],
    {"sub/catalog-language.typify": CAT + b"\n\n"},
    ["fmt", "sub/catalog-language.typify", "--write"],
    ["check", "sub/catalog-language.typify"],
    after={"sub/catalog-language.typify": CAT},
)
case(
    "w07-write-bytes-kept",
    "`--write` keeps a payload byte that is no valid UTF-8",
    ["9 point 7", "9 point 6"],
    {S: SHAPES, PRG: raw("  raw : a", b"\xff", "b  \n")},
    W,
    P1C,
    after={PRG: raw("raw : a", b"\xff", "b\n")},
)
case(
    "w08-write-empty-program-read-only",
    "`--write` on the empty program, which is its own form: nothing is written",
    ["9 point 2", "9 point 6"],
    {S: SHAPES, PRG: b""},
    W,
    P1C,
    readonly=[PRG],
)

# ---------------------------------------------------------------------------
# A refused text (L4, 9 point 5): standard error and status are `check`'s.

REFUSED = {
    "x01-refused-program-no-form": (
        "a line that no form takes: refused",
        ["9 point 5", "3.3 refused"],
        raw("  ready!  \n", "unknown-thing : x\n"),
    ),
    "x02-refused-lone-carriage-return": (
        "a CR that is no part of a line end: refused",
        ["9 point 5", "2 line end", "3.1 row 1 read"],
        raw("# a\rb\n", "ready!\n"),
    ),
    "x03-refused-blank-before-bang": (
        "a blank before `!` is read, not folded: refused",
        ["9 point 5", "3.1 row 4 read"],
        raw("  ready !\n"),
    ),
    "x04-refused-class-and-loose-spacing": (
        "a text with loose spacing and a refused code point is refused, not formatted",
        ["9 point 5", "3.1 row 5 read"],
        raw("  memo:   a", "\u00a0", "b  \n", "ready!\n"),
    ),
}
for cid, (title, cites, source) in REFUSED.items():
    files = {S: SHAPES, PRG: source}
    chk_args = ["check", S, "Shapes", PRG]
    for mode, suffix in [(None, ""), ("--check", "-check"), ("--write", "-write")]:
        args = ["fmt", S, "Shapes", PRG] + ([mode] if mode else [])
        case(
            cid + suffix,
            title + (f" ({mode})" if mode else ""),
            cites,
            files,
            args,
            chk_args,
            premise="refused",
            as_check=True,
        )

HEADERLESS = raw("\n", "#lang typify\n", "declare : A\n", "end!\n")
case(
    "x05-refused-declarations-blank-above-header",
    "a declarations file with a blank line above line 1 is refused, not formatted",
    ["9 point 5", "3.1 row 2 read"],
    {"a-language.typify": HEADERLESS},
    ["fmt", "a-language.typify"],
    ["check", "a-language.typify"],
    premise="refused",
    as_check=True,
)
case(
    "x06-refused-declarations-blank-above-header-write",
    "the same, with `--write`: the file keeps its bytes",
    ["9 point 5", "L4"],
    {"a-language.typify": HEADERLESS},
    ["fmt", "a-language.typify", "--write"],
    ["check", "a-language.typify"],
    premise="refused",
    as_check=True,
)
case(
    "x07-refused-declarations-malformed-line",
    "a declarations file with a line of no kernel shape is refused",
    ["9 point 5", "3.3 refused"],
    {
        "a-language.typify": raw(
            "#lang typify\n", "declare : A\n", "  not a line  \n", "end!\n"
        )
    },
    ["fmt", "a-language.typify"],
    ["check", "a-language.typify"],
    premise="refused",
    as_check=True,
)
case(
    "x08-refused-declarations-under-program",
    "a refused declarations file refuses the program under it: check's diagnostics",
    ["9 point 5", "3.3 refused"],
    {
        "bad-language.typify": raw(
            "#lang typify\n",
            "declare : Shapes\n",
            "file : shapes.typify\n",
            "  not a line  \n",
            "end!\n",
        ),
        PRG: b"  ready!  \n",
    },
    ["fmt", "bad-language.typify", "Shapes", PRG],
    ["check", "bad-language.typify", "Shapes", PRG],
    premise="refused",
    as_check=True,
)

# What `check` answers with another status than 1 is `fmt`'s answer too (12 test 2).
for cid, title, files, args in [
    (
        "x09-check-unreadable-declarations",
        "a declarations file that cannot be read",
        {},
        ["fmt", "missing-language.typify"],
    ),
    (
        "x10-check-unreadable-declarations-write",
        "the same, with `--write`: nothing is created",
        {},
        ["fmt", "missing-language.typify", "--write"],
    ),
    (
        "x11-check-unreadable-declarations-check",
        "the same, with `--check`",
        {},
        ["fmt", "missing-language.typify", "--check"],
    ),
    (
        "x12-check-language-not-declared",
        "a language that the declarations do not declare",
        {S: SHAPES, PRG: b"ready!\n"},
        ["fmt", S, "Nope", PRG],
    ),
]:
    case(
        cid,
        title + ": check's answer and status, nothing on stdout",
        ["12 test 2", "9 point 5"],
        files,
        args,
        ["check"] + [a for a in args[1:] if not a.startswith("--")],
        premise="failed",
        as_check=True,
    )

# ---------------------------------------------------------------------------
# A comment prefix that holds a blank (9 point 1).

BP = ["fmt", "blank-language.typify", "Shapes", PRG]
for cid, mode, source in [
    ("b01-blank-prefix-plain", None, b"  ready!  \nnote-line:a\n"),
    ("b02-blank-prefix-check", "--check", b"  ready!  \nnote-line:a\n"),
    ("b03-blank-prefix-write", "--write", b"  ready!  \nnote-line:a\n"),
    ("b04-blank-prefix-canonical-text", None, b"ready!\n"),
    ("b05-blank-prefix-canonical-text-check", "--check", b"ready!\n"),
    ("b06-blank-prefix-empty-program", None, b""),
]:
    case(
        cid,
        "a body whose comment prefix holds a blank: nothing on stdout, a message, exit 2"
        + (f" ({mode})" if mode else ""),
        ["9 point 1"],
        {"blank-language.typify": BLANK, PRG: source},
        BP + ([mode] if mode else []),
        ["check", "blank-language.typify", "Shapes", PRG],
        stderr=BLANK_PREFIX.encode(),
        exit=2,
    )
case(
    "b07-blank-prefix-declarations-are-formatted",
    "the declarations file of such a body is read under `#` and is formatted",
    ["9 point 1: a declarations file is never refused for this"],
    {
        "blank-language.typify": BLANK.replace(
            b"declare : Shapes", b"  declare:Shapes"
        ).replace(b"end!\n", b"  end!  \n")
    },
    ["fmt", "blank-language.typify"],
    ["check", "blank-language.typify"],
    stdout=BLANK,
)
case(
    "b08-blank-prefix-of-another-body",
    "the body that governs the program is Marked: its prefix holds a blank",
    ["9 point 1: the body that governs the text"],
    {"two-language.typify": TWO, "marked.typify": b"memo : a\n"},
    ["fmt", "two-language.typify", "Marked", "marked.typify"],
    ["check", "two-language.typify", "Marked", "marked.typify"],
    stderr=BLANK_PREFIX.encode(),
    exit=2,
)

# The order of fmt's tests (12): usage, then check, then the prefix, then the form.

XS = text(
    [
        "#lang typify",
        "declare : Xs",
        "file : xs.typify",
        "comment : x :",
        "is : word = /[^ ]+/",
        "term : x : word",
        "form : x : <word>",
        "does : x <word>",
        "end!",
    ]
)
XD = "xs-language.typify"
for cid, mode in [
    ("b09-blank-prefix-refused-program", None),
    ("b10-blank-prefix-refused-program-check", "--check"),
    ("b11-blank-prefix-refused-program-write", "--write"),
]:
    case(
        cid,
        "a program that check refuses, under a prefix that holds a blank: check's answer"
        + (f" ({mode})" if mode else ""),
        ["12 test 2 before test 3", "9 point 5"],
        {XD: XS, "xs.typify": b"x:a b\n"},
        ["fmt", XD, "Xs", "xs.typify"] + ([mode] if mode else []),
        ["check", XD, "Xs", "xs.typify"],
        premise="refused",
        as_check=True,
    )
for cid, mode in [
    ("b12-blank-prefix-accepted-program", None),
    ("b13-blank-prefix-accepted-program-check", "--check"),
    ("b14-blank-prefix-accepted-program-write", "--write"),
]:
    case(
        cid,
        "an accepted program under the same language: the line of 9 point 1, exit 2"
        + (f" ({mode})" if mode else ""),
        ["12 test 3", "9 point 1"],
        {XD: XS, "xs.typify": b"x : a\n"},
        ["fmt", XD, "Xs", "xs.typify"] + ([mode] if mode else []),
        ["check", XD, "Xs", "xs.typify"],
        stderr=BLANK_PREFIX.encode(),
        exit=2,
    )

# ---------------------------------------------------------------------------
# An accepted text with a note (10): standard error is empty in all three modes.

NOTE_LOOSE = raw(
    "#lang typify\n",
    "\n",
    "declare:Empty\n",
    "  file:empty.typify  \n",
    '  is   :   none = "a" except "a"\n',
    "\n",
    "\n",
    "end!\n",
)
NOTE_CANON = raw(
    "#lang typify\n",
    "\n",
    "declare : Empty\n",
    "file : empty.typify\n",
    'is : none = "a" except "a"\n',
    "\n",
    "end!\n",
)
NN = "empty-language.typify"
case(
    "n01-note-plain",
    "an accepted text with a note: the form on stdout, stderr empty",
    ["10"],
    {NN: NOTE_LOOSE},
    ["fmt", NN],
    ["check", NN],
    stdout=NOTE_CANON,
)
case(
    "n02-note-check-loose",
    "`--check` on such a text: its line, stderr empty",
    ["10", "9 point 3"],
    {NN: NOTE_LOOSE},
    ["fmt", NN, "--check"],
    ["check", NN],
    stdout=f"{NN}:3{NOT_CANONICAL}".encode(),
    exit=1,
)
case(
    "n03-note-check-canonical",
    "`--check` on such a text in its form: no output at all",
    ["10", "9 point 3"],
    {NN: NOTE_CANON},
    ["fmt", NN, "--check"],
    ["check", NN],
)
case(
    "n04-note-write",
    "`--write` on such a text: stdout and stderr empty",
    ["10", "9 point 6"],
    {NN: NOTE_LOOSE},
    ["fmt", NN, "--write"],
    ["check", NN],
    after={NN: NOTE_CANON},
)
case(
    "n05-note-plain-canonical",
    "such a text in its form is printed as it is, stderr empty",
    ["10", "3.4 L2"],
    {NN: NOTE_CANON},
    ["fmt", NN],
    ["check", NN],
    stdout=NOTE_CANON,
)

# ---------------------------------------------------------------------------
# The command line (9 point 4): the usage line, and each way to reach it.

D = "catalog-language.typify"
for cid, title, args in [
    ("u01-usage-no-argument", "`fmt` with nothing after it", ["fmt"]),
    (
        "u02-usage-two-arguments",
        "`fmt DECLARATIONS LANGUAGE`: two arguments",
        ["fmt", D, "Catalog"],
    ),
    (
        "u03-usage-four-arguments",
        "`fmt` with four arguments",
        ["fmt", D, "Catalog", "catalog.typify", "extra"],
    ),
    (
        "u04-usage-write-and-check",
        "`--write` together with `--check`",
        ["fmt", D, "--write", "--check"],
    ),
    (
        "u05-usage-check-and-write",
        "`--check` together with `--write`",
        ["fmt", D, "--check", "--write"],
    ),
    (
        "u06-usage-write-check-program",
        "`--write` with `--check`, program form",
        ["fmt", D, "Catalog", "catalog.typify", "--write", "--check"],
    ),
    ("u07-usage-json", "`--json` has no projection of `fmt`", ["fmt", D, "--json"]),
    ("u08-usage-sarif", "`--sarif` has no projection of `fmt`", ["fmt", D, "--sarif"]),
    ("u09-usage-lsp", "`--lsp` has no projection of `fmt`", ["fmt", D, "--lsp"]),
    (
        "u10-usage-github-annotations",
        "`--github-annotations` has no projection of `fmt`",
        ["fmt", D, "--github-annotations"],
    ),
    (
        "u11-usage-json-program",
        "`--json` with the program form",
        ["fmt", D, "Catalog", "catalog.typify", "--json"],
    ),
]:
    case(
        cid,
        title + ": the usage line of `fmt`, exit 2",
        ["9 point 4"],
        {D: CAT, "catalog.typify": b"item : book = Algebra\n"},
        args,
        None,
        premise="none",
        stderr=USAGE.encode(),
        exit=2,
    )

for cid, title, args in [
    (
        "u12-usage-before-check-json",
        "`--json` with a declarations file that check refuses",
        ["fmt", "bad-language.typify", "--json"],
    ),
    (
        "u13-usage-before-check-write-check",
        "`--write --check` with a program that check refuses",
        ["fmt", "bad-language.typify", "Shapes", "shapes.typify", "--write", "--check"],
    ),
    (
        "u14-usage-before-check-two-arguments",
        "two arguments with a refused declarations file",
        ["fmt", "bad-language.typify", "Shapes"],
    ),
]:
    case(
        cid,
        title + ": the usage line comes before check's answer (12 test 1)",
        ["12 test 1", "9 point 4"],
        {
            "bad-language.typify": raw(
                "#lang typify\n",
                "declare : Shapes\n",
                "file : shapes.typify\n",
                "  not a line  \n",
                "end!\n",
            ),
            "shapes.typify": b"ready!\n",
        },
        args,
        None,
        premise="none",
        stderr=USAGE.encode(),
        exit=2,
    )


def build_self(cases_dir):
    """The cases about the self-declaration, which needs its bytes."""
    path = os.path.join(HERE, "fixtures", "self-declaration.txt")
    with open(path, "rb") as handle:
        data = handle.read()
    crlf = data.replace(b"\n", b"\r\n")
    self_case(
        "s01-self-declaration-plain",
        "the reader's self-declaration is its own canonical form",
        ["3.4 L5"],
        {SELF: data},
        ["fmt", SELF],
        stdout=data,
    )
    self_case(
        "s02-self-declaration-check",
        "`--check` on the self-declaration: no output, exit 0",
        ["3.4 L5", "9 point 3"],
        {SELF: data},
        ["fmt", SELF, "--check"],
    )
    self_case(
        "s03-self-declaration-write",
        "`--write` on the self-declaration writes nothing",
        ["3.4 L5", "9 point 6"],
        {SELF: data},
        ["fmt", SELF, "--write"],
        readonly=[SELF],
    )
    for cid, title, text_, mode in [
        (
            "s04-self-declaration-crlf",
            "a CR LF copy is not the reader's own text byte for byte: refused",
            crlf,
            None,
        ),
        (
            "s05-self-declaration-no-last-newline",
            "a copy without its last newline is refused too",
            data[:-1],
            None,
        ),
        (
            "s06-self-declaration-crlf-check",
            "the same CR LF copy with `--check`: as check",
            crlf,
            "--check",
        ),
        (
            "s07-self-declaration-crlf-write",
            "the same CR LF copy with `--write`: refused, the file keeps its bytes",
            crlf,
            "--write",
        ),
    ]:
        self_case(
            cid,
            title,
            ["3.4 L5", "3.3 refused", "9 point 5"],
            {SELF: text_},
            ["fmt", SELF] + ([mode] if mode else []),
            premise="refused",
            as_check=True,
        )


# ---------------------------------------------------------------------------


TOKENS = {b"\xff": ("{{FF}}", "ff"), b"\xfe": ("{{FE}}", "fe")}


def tokenized(data, used):
    """Bytes that are no UTF-8 become tokens: the repository's publication check
    refuses a file that cannot be read as UTF-8. The gate puts the bytes back."""
    for byte, (token, value) in TOKENS.items():
        if byte in data:
            data = data.replace(byte, token.encode())
            used[token] = value
    data.decode("utf-8")
    return data


def write_cases(out):
    if not any(c["id"].startswith("s01-") for c in CASES):
        build_self(out)
    os.makedirs(out, exist_ok=True)
    seen = set()
    for c in CASES:
        assert c["id"] not in seen, c["id"]
        seen.add(c["id"])
        base = os.path.join(out, c["id"])
        os.makedirs(base)
        used = {}

        def put(name, data):
            target = os.path.join(base, name)
            os.makedirs(os.path.dirname(target), exist_ok=True)
            with open(target, "wb") as handle:
                handle.write(tokenized(data, used))

        for name, data in c["files"].items():
            put(name + ".in", data)
        expect = {"exit": c["exit"]}
        if c["stdout"]:
            expect["stdout"] = "expect.stdout"
            put("expect.stdout", c["stdout"])
        if c["stderr"]:
            expect["stderr"] = "expect.stderr"
            put("expect.stderr", c["stderr"])
        if c["as_check"]:
            expect["as_check"] = True
        if c["after"]:
            expect["files"] = {}
            for name, data in c["after"].items():
                label = "expect.after." + name.replace("/", "_") + ".in"
                expect["files"][name] = label
                put(label, data)
        meta = {
            "title": c["title"],
            "cites": c["cites"],
            "args": c["args"],
            "premise": c["premise"],
            "expect": expect,
        }
        if c["check"]:
            meta["check_args"] = c["check"]
        if c["readonly"]:
            meta["readonly"] = c["readonly"]
        if used:
            meta["bytes"] = used
        with open(os.path.join(base, "case.json"), "w", encoding="utf-8") as handle:
            json.dump(meta, handle, indent=2, ensure_ascii=False)
            handle.write("\n")
    return len(CASES)


if __name__ == "__main__":
    target = sys.argv[1] if len(sys.argv) > 1 else os.path.join(HERE, "cases")
    print(write_cases(target), "cases written to", target)
