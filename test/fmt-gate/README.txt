Typify `fmt` gate
=================

A second derivation of `typify fmt`: cases and a reference formatter written
from the text of the formatter's definition alone (sections 1 to 12), and one
gate program that says, for any reader, whether its `fmt` does what that text
says.

Run it
------

    python3 test/fmt-gate/gate.py /path/to/reader

The one argument is the path of a reader. The gate treats it as a black box: it
runs `fmt`, `check` and `emit` and compares what comes out. It prints one line
per case, then the count of cases, then the count of the M5 comparison, and
exits 0 only when every case holds and every compared text is equal.
`--only TEXT` runs the cases whose name holds TEXT, `--no-m5` skips the
comparison, `--jobs N` sets how many readers run at once. The gate writes only
into scratch directories. It makes no compiled file.

What stands here
----------------

    ref_fmt.py                the reference formatter: Python, standard library
                              only, about 120 lines. It maps an accepted text to
                              its canonical form (section 3.2, with 9.1, 9.2 and
                              9.7).
    cases/                    one directory per case: the files the run reads,
                              the run, and the expectation with the numbers of
                              the rules it comes from.
    make_cases.py             writes cases/. Every expectation in it is written
                              by hand from the text, with the number of the rule
                              or ruling beside it.
    gate.py                   the gate.
    fixtures/self-declaration.txt
                              the reader's self-declaration of the 1.0.0 release
                              (7,345 bytes, 229 lines), for law L5.
    standin/typify            a stand-in reader. It answers `fmt` with the
                              reference and the command surface of section 9, and
                              passes every other command to a real reader.
                              FMT_GATE_FAULT puts one fault into `fmt`.
    test_gate.py              the gate's own test: the reference against every
                              case, the laws on the corpus, the stand-in against
                              each fault, and a reader without the command.

A case
------

`case.json` holds the arguments after the reader (`args`), the reader's own
`check` that tells what the input is (`check_args`, with `premise` `accepted`,
`refused` or `failed`), the files to make read-only (`readonly`), and the
expectation (`expect`): the exit status, the files that hold the expected
standard output and standard error, the expected bytes of each input file
afterwards, and `as_check` where the text says that a refused text gets
`check`'s answer. A case whose premise does not hold on the reader under test
fails with that reason, since the input is then no longer the text the case is
about.

The run happens in a scratch copy of the case's input files, so a path in a
message is the path in `args`. An input file is stored with a trailing `.in`,
which the copy drops: the repository's publication check takes a `.typify` file
only in the corpus and in the ask gate's declarations. For the same reason a
byte that is no valid UTF-8 is stored as a token such as {{FF}}; `bytes` in
`case.json` names the value of each token, and the gate puts the byte back in
the input and in the expectation.

The comparison with the corpus (mark M5)
----------------------------------------

For every Typify text of test/corpus/ and of the ask gate's declarations that
the reader's `check` accepts, the gate makes the variants that law L3 names
(CR LF, a missing last newline, longer runs of blank lines where the text has
a blank line, blank lines at the ends, outer blanks, tight, tab-spaced and wide
spacing around the first separator, and all of these together; no blank line is
made where the text has none, since the formatter keeps the author's blank
lines, section 11). A variant counts only when the reader reads it as
it reads the original: `check --json` gives the same terms for a program,
`emit` the same grammar for each language of a declarations file. A variant
that reads otherwise is left out and counted. The reader's `fmt` and the
reference must agree byte for byte on each text and on each variant that
counts. The frozen files are only read: each text is copied to a scratch
directory first.

The gate's own test
-------------------

    FMT_GATE_BASE_READER=/path/to/reader python3 test/fmt-gate/test_gate.py

The stand-in reader needs a real reader for the commands that are not `fmt`.
The test runs the stand-in with the reference through the gate, then the same
stand-in with each of eight faults, then a stand-in that answers `fmt` as a
reader without the command does.
