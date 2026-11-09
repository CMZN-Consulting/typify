#!/usr/bin/env python3
# Language: Python 3.15.0 (standard library only)
# Typify v1 Stage-Three Reference Oracle for `ask` and Gate Generator.
# Derived solely from the Typify v1 definition text (blocks through 2026-11-09T17:41:38Z).

import argparse
import json
import re
import sys
from pathlib import Path

VERB_CLASS_RE = re.compile(r"^[A-Za-z0-9][A-Za-z0-9-]*$")
VERB_ALPHABET = (
    [chr(c) for c in range(ord("A"), ord("Z") + 1)]
    + [chr(c) for c in range(ord("a"), ord("z") + 1)]
    + [chr(c) for c in range(ord("0"), ord("9") + 1)]
    + ["-"]
)

CONSTRUCTOR_LINE_VERBS = {
    "writes",
    "reads",
    "guard",
    "effect",
    "stage",
    "scope",
    "fuel",
    "fact",
}

WITNESS_WORDS = [
    "alias-view",
    "canonical-path",
    "cawxyz",
    "declarx",
    "foreign-op",
    "ledgex",
    "letter",
    "ready!",
    "relative-path",
    "taxonomx",
    "unbind",
]


def osa_distance(a: str, b: str) -> int:
    """Optimal String Alignment distance over Unicode code points (14:59:52Z item 7).
    Insertion, deletion, substitution, or adjacent transposition costs 1; no substring edited twice.
    Case is significant.
    """
    m, n = len(a), len(b)
    dp = [[0] * (n + 1) for _ in range(m + 1)]
    for i in range(m + 1):
        dp[i][0] = i
    for j in range(n + 1):
        dp[0][j] = j
    for i in range(1, m + 1):
        for j in range(1, n + 1):
            cost = 0 if a[i - 1] == b[j - 1] else 1
            val = min(
                dp[i - 1][j] + 1,
                dp[i][j - 1] + 1,
                dp[i - 1][j - 1] + cost,
            )
            if i > 1 and j > 1 and a[i - 1] == b[j - 2] and a[i - 2] == b[j - 1]:
                val = min(val, dp[i - 2][j - 2] + 1)
            dp[i][j] = val
    return dp[m][n]


def edit_bound(word_len: int) -> int:
    """Distance bound max(n, 3) // 3 capped at 2 (14:59:52Z item 8)."""
    return min(2, max(word_len, 3) // 3)


def hyphen_parts(word: str) -> list[str]:
    """Non-empty runs between hyphens; a word without a hyphen is one part (14:59:52Z item 9)."""
    return [p for p in word.split("-") if p != ""]


def extract_placeholders(text: str) -> list[str]:
    """Extract <name> placeholders in order of first appearance."""
    seen = []
    for match in re.finditer(r"<([A-Za-z0-9_-]+)>", text):
        name = match.group(1)
        if name not in seen:
            seen.append(name)
    return seen


def extract_form_verb(production: str) -> str | None:
    """Extract literal verb of a colon or bang form (06:05:00Z item 3, 15:20:48Z item 5)."""
    if ":" in production:
        left = production.split(":", 1)[0].strip()
        if left and "<" not in left and ">" not in left and " " not in left:
            return left
        return None
    if production.endswith("!") and "->" not in production:
        left = production[:-1].strip()
        if left and "<" not in left and ">" not in left and " " not in left:
            return left
        return None
    return None


def make_meta_body() -> dict:
    """Construct the pinned question form entry of the reserved meta body Typify (15:14:08Z item 1)."""
    return {
        "name": "Typify",
        "file_line": None,
        "classes": {
            "verb": "is : verb = /[A-Za-z0-9][A-Za-z0-9-]*/",
        },
        "constructors": {
            "question": {
                "decl_line": "primitive : question : verb",
                "sig_classes": ["verb"],
                "ctor_lines": ["effect : question = external"],
                "rule_lines": [],
                "ensure_lines": [],
                "unkeyed_count": None,
                "keyed_count": None,
                "note_lines": [],
            }
        },
        "forms": [
            {
                "verb": "?",
                "form_line": "form : ? : <verb>",
                "does_line": "does : question <verb>",
                "ctor": "question",
                "does_placeholders": ["verb"],
                "examples": ["example : ? : form"],
                "defaults": [],
                "restricts": False,
            }
        ],
        "cells": {},
        "joins": {},
    }


def split_lines_with_offsets(raw_text: str) -> list[tuple[int, int, str]]:
    """Split text on LF or CRLF only (D9), returning (1-based line, start byteOffset, content)."""
    result: list[tuple[int, int, str]] = []
    lineno = 1
    byte_offset = 0
    i = 0
    n = len(raw_text)
    line_start = 0
    while i < n:
        ch = raw_text[i]
        if ch == "\n":
            content = raw_text[line_start:i]
            result.append((lineno, byte_offset, content))
            byte_offset += len(content.encode("utf-8")) + 1
            lineno += 1
            i += 1
            line_start = i
        elif ch == "\r" and i + 1 < n and raw_text[i + 1] == "\n":
            content = raw_text[line_start:i]
            result.append((lineno, byte_offset, content))
            byte_offset += len(content.encode("utf-8")) + 2
            lineno += 1
            i += 2
            line_start = i
        else:
            i += 1
    if line_start < n or n == 0:
        content = raw_text[line_start:n]
        if content or not result:
            result.append((lineno, byte_offset, content))
    return result


def whole_line_range(lineno: int, line_byte_start: int, raw_line: str) -> dict:
    """Compute D2 whole-line range from first to last non-blank code point."""
    leading = len(raw_line) - len(raw_line.lstrip(" \t"))
    trailing_stripped = raw_line.rstrip(" \t")
    start_col = leading + 1
    end_col = len(trailing_stripped) + 1
    start_byte = line_byte_start + len(raw_line[:leading].encode("utf-8"))
    end_byte = line_byte_start + len(trailing_stripped.encode("utf-8"))
    return {
        "start": {"line": lineno, "column": start_col, "byteOffset": start_byte},
        "end": {"line": lineno, "column": end_col, "byteOffset": end_byte},
    }


def parse_declarations(path: Path, display_file: str | None = None) -> tuple[dict[str, dict], list[dict]]:
    """Parse bodies of a Typify v1 declaration file and collect any E_QUESTION diagnostics (17:41:38Z item 4)."""
    file_label = display_file if display_file is not None else str(path)
    raw_text = path.read_text(encoding="utf-8")
    lines_with_offsets = split_lines_with_offsets(raw_text)
    bodies: dict[str, dict] = {}
    diagnostics: list[dict] = []
    current: dict | None = None
    comment_prefix = "#"

    for idx, (lineno, byte_start, raw) in enumerate(lines_with_offsets):
        line = raw.strip(" \t")
        if idx == 0 and line == "#lang typify":
            continue
        if not line or (comment_prefix and line.startswith(comment_prefix)):
            continue

        if ":" in line:
            v_check, a_check = [p.strip(" \t") for p in line.split(":", 1)]
            if v_check == "?":
                # Question line in a declarations file (17:41:38Z item 4)
                if not VERB_CLASS_RE.match(a_check):
                    diagnostics.append(
                        {
                            "code": "E_QUESTION",
                            "file": file_label,
                            "range": whole_line_range(lineno, byte_start, raw),
                            "data": {"actual": line},
                        }
                    )
                continue

        if current is None:
            if line.startswith("declare :"):
                name = line.split(":", 1)[1].strip()
                current = {
                    "name": name,
                    "file_line": None,
                    "classes": {},
                    "constructors": {},
                    "forms": [],
                    "cells": {},
                    "joins": {},
                }
                comment_prefix = "#"
            continue

        if line == "end!":
            bodies[current["name"]] = current
            current = None
            comment_prefix = "#"
            continue

        if line == "restricts!":
            if current["forms"]:
                current["forms"][-1]["restricts"] = True
            continue

        if ":" not in line:
            continue

        verb, arg = [p.strip() for p in line.split(":", 1)]
        norm_line = f"{verb} : {arg}"

        if verb == "comment":
            comment_prefix = arg
        elif verb == "file":
            current["file_line"] = norm_line
        elif verb == "is":
            cname = arg.split("=", 1)[0].strip()
            current["classes"][cname] = norm_line
        elif verb in ("term", "primitive"):
            if ":" in arg:
                cname, sig = [p.strip() for p in arg.split(":", 1)]
                sig_classes = [c for c in sig.split() if c]
            else:
                cname = arg.strip()
                sig_classes = []
            current["constructors"][cname] = {
                "decl_line": norm_line,
                "sig_classes": sig_classes,
                "ctor_lines": [],
                "rule_lines": [],
                "ensure_lines": [],
                "unkeyed_count": None,
                "keyed_count": None,
                "note_lines": [],
            }
        elif verb == "form":
            fverb = extract_form_verb(arg)
            current["forms"].append(
                {
                    "verb": fverb,
                    "form_line": norm_line,
                    "does_line": None,
                    "ctor": None,
                    "does_placeholders": [],
                    "examples": [],
                    "defaults": [],
                    "restricts": False,
                }
            )
        elif verb == "does":
            if current["forms"]:
                ctor = arg.split()[0] if arg.split() else ""
                current["forms"][-1]["does_line"] = norm_line
                current["forms"][-1]["ctor"] = ctor
                current["forms"][-1]["does_placeholders"] = extract_placeholders(arg)
        elif verb == "example":
            if current["forms"]:
                current["forms"][-1]["examples"].append(norm_line)
        elif verb == "default":
            if current["forms"]:
                current["forms"][-1]["defaults"].append(norm_line)
        elif verb == "count":
            lhs, rhs = [p.strip() for p in arg.split("=", 1)]
            if " by " in rhs:
                if not rhs.endswith(" by 0"):
                    current["constructors"][lhs]["keyed_count"] = norm_line
                else:
                    current["constructors"][lhs]["unkeyed_count"] = f"count : {lhs} = {rhs.split(' by ')[0].strip()}"
            else:
                current["constructors"][lhs]["unkeyed_count"] = norm_line
        elif verb == "rule":
            lhs = arg.split("->", 1)[0].strip()
            ctor = lhs.split()[0]
            if ctor in current["constructors"]:
                current["constructors"][ctor]["rule_lines"].append(norm_line)
        elif verb == "ensure":
            ctor = arg.split(":", 1)[0].strip()
            if ctor in current["constructors"]:
                current["constructors"][ctor]["ensure_lines"].append(norm_line)
        elif verb == "note":
            ctor = arg.split("=", 1)[0].strip()
            if ctor in current["constructors"]:
                current["constructors"][ctor]["note_lines"].append(norm_line)
        elif verb == "cell":
            cname, law = [p.strip() for p in arg.split("=", 1)]
            table = None
            if law.startswith("join "):
                table = law.split()[1]
            current["cells"][cname] = {"cell_line": norm_line, "table": table}
        elif verb == "join":
            tname = arg.split(":", 1)[0].strip()
            current["joins"].setdefault(tname, []).append(norm_line)
        elif verb in CONSTRUCTOR_LINE_VERBS:
            if ":" in arg:
                target = arg.split(":", 1)[0].strip()
            elif "=" in arg:
                target = arg.split("=", 1)[0].strip()
            else:
                target = arg.strip()
            if target in current["constructors"]:
                current["constructors"][target]["ctor_lines"].append(norm_line)

    bodies["Typify"] = make_meta_body()
    return bodies, diagnostics


def body_candidates(body: dict) -> list[str]:
    """Candidates for `near`: verbs of the asked body's forms and constructors of its
    `term` and `primitive` lines, each once (14:59:52Z item 6, 15:20:48Z item 5, 17:41:38Z item 3).
    Classes and cells are never candidates. Returned in code-point order.
    """
    cands = set()
    for f in body["forms"]:
        if f["verb"]:
            cands.add(f["verb"])
    for cname in body["constructors"]:
        cands.add(cname)
    return sorted(cands)


def body_declared_words(body: dict) -> set[str]:
    """Every word whose status in the asked body is `declared` (15:20:48Z item 2, 17:41:38Z item 3):
    form verbs, term/primitive constructors, classes, and cells.
    """
    declared = set(body_candidates(body))
    declared.update(body["classes"].keys())
    declared.update(body["cells"].keys())
    return declared


def compute_near(word: str, candidates: list[str], bound_mode: str = "item8", list_mode: str = "item9", parts_fallback: bool = True) -> list[str]:
    """Compute `near` list under the pinned rules (items 7, 8, 9) or rival variants."""
    scored: list[tuple[int, str]] = []
    for c in candidates:
        dist = osa_distance(word, c)
        if bound_mode == "item8":
            b = edit_bound(len(word))
        elif bound_mode == "alt_shorter":
            b = edit_bound(min(len(word), len(c)))
        else:
            raise ValueError(f"Unknown bound_mode: {bound_mode}")
        if dist <= b:
            scored.append((dist, c))

    if scored:
        if list_mode == "item9":
            min_d = min(d for d, _ in scored)
            return sorted(c for d, c in scored if d == min_d)
        elif list_mode == "alt_all_within_bound":
            scored.sort(key=lambda pair: (pair[0], pair[1]))
            return [c for _, c in scored]
        else:
            raise ValueError(f"Unknown list_mode: {list_mode}")

    if not parts_fallback:
        return []

    w_parts = set(hyphen_parts(word))
    shared = [c for c in candidates if set(hyphen_parts(c)) & w_parts]
    return sorted(shared)


def answer_declared(body: dict, word: str) -> list[str]:
    """Build the `? : ...` lines for a word declared in `body` (15:14:08Z items 1-4, 15:20:48Z item 4, 17:41:38Z items 1-3)."""
    selected_forms = []
    for f in body["forms"]:
        if f["verb"] == word or f["ctor"] == word:
            selected_forms.append(f)

    selected_ctors = []
    if word in body["constructors"]:
        selected_ctors.append(word)
    for f in selected_forms:
        if f["ctor"] and f["ctor"] in body["constructors"] and f["ctor"] not in selected_ctors:
            selected_ctors.append(f["ctor"])

    out: list[str] = []

    if selected_forms or selected_ctors:
        if selected_forms:
            for idx, f in enumerate(selected_forms):
                out.append(f"? : {f['form_line']}")
                if idx == 0 and body["file_line"]:
                    out.append(f"? : {body['file_line']}")
                if f["does_line"]:
                    out.append(f"? : {f['does_line']}")
                if f["restricts"]:
                    out.append("? : restricts!")
        else:
            # Formless constructor (15:20:48Z item 4)
            cname = selected_ctors[0]
            cinfo = body["constructors"][cname]
            out.append(f"? : {cinfo['decl_line']}")
            if body["file_line"]:
                out.append(f"? : {body['file_line']}")

        for cname in selected_ctors:
            cinfo = body["constructors"][cname]
            if selected_forms:
                out.append(f"? : {cinfo['decl_line']}")
                # Placeholders named in `does` lines building this constructor, each once within this constructor's group (17:41:38Z item 2)
                named_classes = []
                for f in selected_forms:
                    if f["ctor"] == cname:
                        for ph in f["does_placeholders"]:
                            if ph not in named_classes:
                                named_classes.append(ph)
                for ph in named_classes:
                    if ph in body["classes"]:
                        out.append(f"? : {body['classes'][ph]}")
            else:
                # Signature classes in signature order (15:20:48Z item 4)
                seen_sig = []
                for sc in cinfo["sig_classes"]:
                    if sc not in seen_sig:
                        seen_sig.append(sc)
                        if sc in body["classes"]:
                            out.append(f"? : {body['classes'][sc]}")

            for cline in cinfo["ctor_lines"]:
                out.append(f"? : {cline}")
            for rline in cinfo["rule_lines"]:
                out.append(f"? : {rline}")
            for eline in cinfo["ensure_lines"]:
                out.append(f"? : {eline}")

        for f in selected_forms:
            for ex in f["examples"]:
                out.append(f"? : {ex}")

        for cname in selected_ctors:
            cinfo = body["constructors"][cname]
            unkeyed = cinfo["unkeyed_count"] or f"count : {cname} = *"
            out.append(f"? : {unkeyed}")
            if cinfo["keyed_count"]:
                out.append(f"? : {cinfo['keyed_count']}")

            defaults = []
            for f in body["forms"]:
                if f["ctor"] == cname:
                    for dline in f["defaults"]:
                        if dline not in defaults:
                            defaults.append(dline)

            mult = unkeyed.split("=", 1)[1].strip()
            if defaults:
                missing_kind = "default"
            elif mult in ("1", "1..*"):
                missing_kind = "refused"
            else:
                missing_kind = "empty"
            out.append(f"? : missing : {missing_kind}")
            for dline in defaults:
                out.append(f"? : {dline}")

        for cname in selected_ctors:
            for nline in body["constructors"][cname]["note_lines"]:
                out.append(f"? : {nline}")

    # Union with class and cell answers when also declared as a class or cell, omitting any line already printed (17:41:38Z item 1)
    if word in body["classes"]:
        is_line = f"? : {body['classes'][word]}"
        if is_line not in out:
            out.append(is_line)

    if word in body["cells"]:
        cell_info = body["cells"][word]
        cell_line = f"? : {cell_info['cell_line']}"
        if cell_line not in out:
            out.append(cell_line)
        if cell_info["table"] and cell_info["table"] in body["joins"]:
            for jline in body["joins"][cell_info["table"]]:
                jl = f"? : {jline}"
                if jl not in out:
                    out.append(jl)

    if not out:
        raise ValueError(f"Word {word!r} not found in body {body['name']!r}")
    return out


def ask_word(bodies: dict[str, dict], language: str, word: str, decl_diagnostics: list[dict] | None = None) -> tuple[int, dict]:
    """Execute `typify ask <declarations> <Language> <word> --json` per blocks through 17:41:38Z."""
    if decl_diagnostics:
        return 1, {
            "ok": False,
            "diagnostics": decl_diagnostics,
        }

    if language not in bodies:
        return 2, {
            "ok": False,
            "diagnostics": [
                {
                    "code": "E_USAGE",
                    "file": "",
                    "range": None,
                    "data": {"actual": language},
                }
            ],
        }

    body = bodies[language]
    declared = body_declared_words(body)

    if word in declared:
        lines = answer_declared(body, word)
        return 0, {
            "word": word,
            "status": "declared",
            "lines": lines,
            "near": [],
            "accepts": [],
            "diagnostics": [],
        }

    if not VERB_CLASS_RE.match(word):
        return 2, {
            "ok": False,
            "diagnostics": [
                {
                    "code": "E_USAGE",
                    "file": "",
                    "range": None,
                    "data": {"actual": word},
                }
            ],
        }

    candidates = body_candidates(body)
    near = compute_near(word, candidates)
    lines = [f"? : {word} = undeclared"] + [f"? : near : {c}" for c in near]
    return 0, {
        "word": word,
        "status": "undeclared",
        "lines": lines,
        "near": near,
        "accepts": [],
        "diagnostics": [],
    }


def verify_round_trip(obj: dict) -> None:
    """Verify the round-trip law of 14:59:52Z item 14 on an answer object."""
    lines = obj["lines"]
    for ln in lines:
        if not ln.startswith("? : "):
            raise AssertionError(f"Line does not start with '? : ': {ln!r}")
    if obj["status"] == "undeclared":
        rebuilt = [f"? : {obj['word']} = {obj['status']}"] + [f"? : near : {c}" for c in obj["near"]]
        if rebuilt != lines:
            raise AssertionError(f"Round-trip mismatch for {obj['word']!r}: {rebuilt!r} != {lines!r}")


def one_edits(word: str) -> set[str]:
    """All words one edit (insert, delete, substitute from VERB_ALPHABET, or adjacent exchange) from `word`."""
    res: set[str] = set()
    n = len(word)
    # Deletions
    for i in range(n):
        res.add(word[:i] + word[i + 1 :])
    # Adjacent exchanges
    for i in range(n - 1):
        res.add(word[:i] + word[i + 1] + word[i] + word[i + 2 :])
    # Substitutions
    for i in range(n):
        for ch in VERB_ALPHABET:
            if ch != word[i]:
                res.add(word[:i] + ch + word[i + 1 :])
    # Insertions
    for i in range(n + 1):
        for ch in VERB_ALPHABET:
            res.add(word[:i] + ch + word[i:])
    return res


def two_deletions(word: str) -> set[str]:
    """All words formed by deleting two code points from `word`."""
    res: set[str] = set()
    n = len(word)
    for i in range(n):
        w1 = word[:i] + word[i + 1 :]
        for j in range(len(w1)):
            res.add(w1[:j] + w1[j + 1 :])
    return res


def two_exchanges(word: str) -> set[str]:
    """All words formed by two adjacent exchanges in succession at any positions (15:20:48Z item 2)."""
    res: set[str] = set()
    n = len(word)
    for i in range(n - 1):
        w1 = word[:i] + word[i + 1] + word[i] + word[i + 2 :]
        for j in range(len(w1) - 1):
            w2 = w1[:j] + w1[j + 1] + w1[j] + w1[j + 2 :]
            res.add(w2)
    return res


def generate_typo_set(body: dict) -> list[str]:
    """Generate the typo set for `body` (14:59:52Z item 14, 15:20:48Z item 2)."""
    candidates = body_candidates(body)
    declared = body_declared_words(body)
    raw: set[str] = set()
    for c in candidates:
        raw.update(one_edits(c))
        if len(c) >= 6:
            raw.update(two_deletions(c))
            raw.update(two_exchanges(c))
    filtered = [w for w in raw if w not in declared and VERB_CLASS_RE.match(w) is not None]
    return sorted(filtered)


def generate_unknown_set(body: dict) -> list[str]:
    """Generate the unknown-word set for `body` (14:59:52Z item 14, 15:20:48Z item 2):
    For each hyphenated candidate: `_` for `-`, no separator, parts in reverse order, and each part.
    Declared words leave the set; `_` for `-` stays (refused with exit status 2).
    """
    candidates = body_candidates(body)
    declared = body_declared_words(body)
    raw: set[str] = set()
    for c in candidates:
        if "-" in c:
            parts = hyphen_parts(c)
            raw.add(c.replace("-", "_"))
            raw.add("".join(parts))
            raw.add("-".join(reversed(parts)))
            for p in parts:
                raw.add(p)
    filtered = [w for w in raw if w not in declared]
    return sorted(filtered)


def build_gate_entry(decl_path: Path, rel_path: str, language: str, witness_words: list[str]) -> dict:
    """Build one declaration entry for `gate.json` per 15:14:08Z item 6."""
    bodies, decl_diags = parse_declarations(decl_path, display_file=rel_path)
    if decl_diags:
        raise ValueError(f"Gate declaration {rel_path} has diagnostics: {decl_diags}")
    body = bodies[language]
    candidates = body_candidates(body)
    typo_words = generate_typo_set(body)
    unknown_words = generate_unknown_set(body)
    witness_sorted = sorted(set(witness_words))

    words_out = []
    for set_name, word_list in (
        ("typo", typo_words),
        ("unknown", unknown_words),
        ("witness", witness_sorted),
    ):
        for w in word_list:
            code, obj = ask_word(bodies, language, w, decl_diags)
            if code == 0:
                verify_round_trip(obj)
            words_out.append(
                {
                    "word": w,
                    "set": set_name,
                    "exit_code": code,
                    "stdout_json": obj,
                }
            )

    return {
        "path": rel_path,
        "language": language,
        "candidates": candidates,
        "sizes": {
            "typo": len(typo_words),
            "unknown": len(unknown_words),
            "witness": len(witness_sorted),
        },
        "words": words_out,
    }


def run_prefix_sweep(decl_path: Path, language: str) -> dict:
    """Run the prefix sweep and lost-extension measurement (14:59:52Z item 15, 15:14:08Z item 9, 15:20:48Z item 1)."""
    bodies, _ = parse_declarations(decl_path)
    body = bodies[language]
    candidates = body_candidates(body)
    declared = body_declared_words(body)
    prefixes = ["un", "re", "de", "no", "dis"]

    antonym_hits = []
    prefix_trials = []
    for c in candidates:
        for pfx in prefixes:
            w = pfx + c
            near_item8 = compute_near(w, candidates, bound_mode="item8")
            near_alt = compute_near(w, candidates, bound_mode="alt_shorter")
            is_hit = near_item8 == [c]
            rec = {
                "candidate": c,
                "prefix": pfx,
                "word": w,
                "near_item8": near_item8,
                "near_alt": near_alt,
                "antonym_hit": is_hit,
            }
            prefix_trials.append(rec)
            if is_hit:
                antonym_hits.append(rec)

    lost_extensions = []
    lost_extension_by_candidate: dict[str, int] = {}
    for c in candidates:
        if len(c) in (4, 5):
            count_for_c = 0
            for ch1 in VERB_ALPHABET:
                for ch2 in VERB_ALPHABET:
                    w = c + ch1 + ch2
                    if w in declared or not VERB_CLASS_RE.match(w):
                        continue
                    dist = osa_distance(w, c)
                    b_item8 = edit_bound(len(w))
                    b_alt = edit_bound(min(len(w), len(c)))
                    if dist <= b_item8 and dist > b_alt:
                        count_for_c += 1
                        lost_extensions.append(
                            {
                                "candidate": c,
                                "word": w,
                                "distance": dist,
                                "bound_item8": b_item8,
                                "bound_alt": b_alt,
                            }
                        )
            lost_extension_by_candidate[c] = count_for_c

    return {
        "language": language,
        "candidates": candidates,
        "antonym_hit_count": len(antonym_hits),
        "prefix_trial_count": len(prefix_trials),
        "antonym_hits": antonym_hits,
        "prefix_trials": prefix_trials,
        "lost_extension_count": len(lost_extensions),
        "lost_extension_by_candidate": lost_extension_by_candidate,
        "lost_extensions_sample_first_20": lost_extensions[:20],
    }


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Typify v1 Stage-Three Reference Oracle for `ask` and Gate Generator."
    )
    parser.add_argument(
        "--generate-gate",
        action="store_true",
        help="Generate gate.json from declarations/ask-gate.typify",
    )
    parser.add_argument(
        "--prefix-sweep",
        action="store_true",
        help="Run the prefix sweep and lost-extension measurement over declarations/ask-gate.typify",
    )
    parser.add_argument(
        "--decl",
        default="declarations/ask-gate.typify",
        help="Relative path to gate declaration file (for --generate-gate / --prefix-sweep)",
    )
    parser.add_argument(
        "--lang",
        default="AskGate",
        help="Language name (for --generate-gate / --prefix-sweep)",
    )
    parser.add_argument(
        "--out",
        default="gate.json",
        help="Output path for gate.json (for --generate-gate)",
    )
    parser.add_argument("positional", nargs="*", help="<declarations> <Language> <word>")

    args = parser.parse_args()

    if args.generate_gate:
        base_dir = Path(__file__).resolve().parent.parent
        decl_path = (base_dir / args.decl).resolve()
        entry = build_gate_entry(decl_path, args.decl, args.lang, WITNESS_WORDS)
        gate_obj = {"declarations": [entry]}
        out_path = (base_dir / args.out).resolve()
        out_path.write_text(json.dumps(gate_obj, indent=2) + "\n", encoding="utf-8")
        return 0

    if args.prefix_sweep:
        base_dir = Path(__file__).resolve().parent.parent
        decl_path = (base_dir / args.decl).resolve()
        report = run_prefix_sweep(decl_path, args.lang)
        sys.stdout.write(json.dumps(report, indent=2) + "\n")
        return 0

    if len(args.positional) != 3:
        err = {
            "ok": False,
            "diagnostics": [
                {
                    "code": "E_USAGE",
                    "file": "",
                    "range": None,
                    "data": {"actual": args.positional[-1] if args.positional else ""},
                }
            ],
        }
        sys.stdout.write(json.dumps(err, indent=2) + "\n")
        return 2

    decl_file, language, word = args.positional
    decl_path = Path(decl_file)
    if not decl_path.is_file():
        err = {
            "ok": False,
            "diagnostics": [
                {
                    "code": "E_FILE",
                    "file": decl_file,
                    "range": None,
                    "data": {"path": decl_file},
                }
            ],
        }
        sys.stdout.write(json.dumps(err, indent=2) + "\n")
        return 2

    bodies, decl_diags = parse_declarations(decl_path, display_file=decl_file)
    code, obj = ask_word(bodies, language, word, decl_diags)
    sys.stdout.write(json.dumps(obj, indent=2) + "\n")
    return code


if __name__ == "__main__":
    sys.exit(main())
