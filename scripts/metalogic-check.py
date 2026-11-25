#!/usr/bin/env python3
import argparse
import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import urllib.parse
from pathlib import Path

EXPECTED_TOOLCHAIN = "leanprover/lean4:v4.36.0"
EXPECTED_LEAN_VERSION_SUBSTR = "Lean (version 4.36.0,"
REPO_SLUG = "CMZN-Consulting/typify"
WIKI_SECTION_WELCOME = "Metalogic.md"
WIKI_LAW_PREFIX = "Metalogic-"

ALLOWED_AXIOMS = [
    "Classical.choice",
    "Quot.sound",
    "propext",
]

FROZEN_LAWS = [
    {
        "id": "formality",
        "wikiPage": "Metalogic-Formality.md",
        "wikiLinkTarget": "Metalogic-Formality",
        "statementPath": "lean/TypifyMetalogic/FormalityStatement.lean",
        "statementModule": "TypifyMetalogic.FormalityStatement",
        "statementConst": "TypifyMetalogic.FormalityLaw",
        "proofPath": "lean/TypifyMetalogic/FormalityProof.lean",
        "proofModule": "TypifyMetalogic.FormalityProof",
        "proofConst": "TypifyMetalogic.formality_law",
    },
    {
        "id": "extension",
        "wikiPage": "Metalogic-Extension.md",
        "wikiLinkTarget": "Metalogic-Extension",
        "statementPath": "lean/TypifyMetalogic/ExtensionStatement.lean",
        "statementModule": "TypifyMetalogic.ExtensionStatement",
        "statementConst": "TypifyMetalogic.ExtensionLaw",
        "proofPath": "lean/TypifyMetalogic/ExtensionProof.lean",
        "proofModule": "TypifyMetalogic.ExtensionProof",
        "proofConst": "TypifyMetalogic.extension_law",
    },
    {
        "id": "ontology",
        "wikiPage": "Metalogic-Ontology.md",
        "wikiLinkTarget": "Metalogic-Ontology",
        "statementPath": "lean/TypifyMetalogic/OntologyStatement.lean",
        "statementModule": "TypifyMetalogic.OntologyStatement",
        "statementConst": "TypifyMetalogic.OntologyLaw",
        "proofPath": "lean/TypifyMetalogic/OntologyProof.lean",
        "proofModule": "TypifyMetalogic.OntologyProof",
        "proofConst": "TypifyMetalogic.ontology_law",
    },
]

FORBIDDEN_LEAN_WORDS = {
    "sorry",
    "admit",
    "axiom",
    "opaque",
    "unsafe",
    "partial",
    "implemented_by",
    "extern",
    "native_decide",
    "trustCompiler",
}


class GateFailure(Exception):
    def __init__(self, code: str, detail: str):
        super().__init__(f"{code}: {detail}")
        self.code = code
        self.detail = detail


def sha256_bytes(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def is_external_or_repo_link(url: str) -> bool:
    parsed = urllib.parse.urlsplit(url)
    if parsed.scheme or parsed.netloc or url.startswith("/"):
        return True
    first_segment = parsed.path.split("/", 1)[0].lower()
    host = first_segment.split(":", 1)[0]
    return host == "github.com" or host.endswith(".github.com")


def scan_lean_formatting(rel_path: str, text: str) -> None:
    if "\r" in text:
        raise GateFailure("lean-crlf", f"{rel_path} contains CR characters")
    n = len(text)
    i = 0
    in_str = False
    while i < n:
        ch = text[i]
        if in_str:
            if ch == "\\":
                i += 2
                continue
            if ch == '"':
                in_str = False
            i += 1
            continue
        if ch == '"':
            in_str = True
            i += 1
            continue
        if ch == "'" and i + 2 < n:
            if text[i + 1] == "\\":
                end = text.find("'", i + 3)
                if end != -1:
                    i = end + 1
                    continue
            elif text[i + 2] == "'":
                i += 3
                continue
        if i + 1 < n and (
            (ch == "-" and text[i + 1] == "-") or (ch == "/" and text[i + 1] == "-")
        ):
            raise GateFailure(
                "lean-comment", f"{rel_path} contains a comment at offset {i}"
            )
        i += 1


def scan_lean_keywords(rel_path: str, text: str) -> None:
    n = len(text)
    i = 0
    in_str = False
    while i < n:
        ch = text[i]
        if in_str:
            if ch == "\\":
                i += 2
                continue
            if ch == '"':
                in_str = False
            i += 1
            continue
        if ch == '"':
            in_str = True
            i += 1
            continue
        if ch == "'" and i + 2 < n:
            if text[i + 1] == "\\":
                end = text.find("'", i + 3)
                if end != -1:
                    i = end + 1
                    continue
            elif text[i + 2] == "'":
                i += 3
                continue
        if ch.isalpha() or ch == "_":
            j = i + 1
            while j < n and (text[j].isalnum() or text[j] in {"_", "'"}):
                j += 1
            word = text[i:j]
            if word in FORBIDDEN_LEAN_WORDS:
                raise GateFailure(
                    "forbidden-lean-keyword",
                    f"{rel_path} contains forbidden token '{word}'",
                )
            i = j
            continue
        i += 1


def list_repo_files(
    repo_dir: Path, git_ref: str | None, allow_uncommitted: bool
) -> list[str]:
    if allow_uncommitted:
        out = []
        for p in sorted(repo_dir.rglob("*")):
            if p.is_symlink() or p.is_dir():
                continue
            rel = p.relative_to(repo_dir).as_posix()
            if (
                rel.startswith(".git/")
                or "/.lake/" in f"/{rel}"
                or "/compiled/" in f"/{rel}"
                or "/node_modules/" in f"/{rel}"
            ):
                continue
            out.append(rel)
        return sorted(out)
    ref = git_ref or "HEAD"
    status = subprocess.run(
        ["git", "-C", str(repo_dir), "status", "--porcelain"],
        check=True,
        capture_output=True,
        text=True,
    )
    if git_ref is None and status.stdout.strip():
        raise GateFailure("dirty-worktree", f"{repo_dir} has uncommitted changes")
    proc = subprocess.run(
        ["git", "-C", str(repo_dir), "ls-tree", "-r", "--name-only", ref],
        check=True,
        capture_output=True,
        text=True,
    )
    return sorted(line for line in proc.stdout.splitlines() if line)


def read_repo_blob(
    repo_dir: Path, rel_path: str, git_ref: str | None, allow_uncommitted: bool
) -> bytes:
    if allow_uncommitted:
        p = repo_dir / rel_path
        if not p.is_file():
            raise GateFailure("missing-file", f"{repo_dir}/{rel_path} does not exist")
        return p.read_bytes()
    ref = git_ref or "HEAD"
    proc = subprocess.run(
        ["git", "-C", str(repo_dir), "cat-file", "blob", f"{ref}:{rel_path}"],
        capture_output=True,
    )
    if proc.returncode != 0:
        raise GateFailure("missing-git-blob", f"{ref}:{rel_path} missing in {repo_dir}")
    return proc.stdout


def resolve_git_commit_and_tree(repo_dir: Path, git_ref: str | None) -> tuple[str, str]:
    ref = git_ref or "HEAD"
    c = subprocess.run(
        ["git", "-C", str(repo_dir), "rev-parse", "--verify", f"{ref}^{{commit}}"],
        check=True,
        capture_output=True,
        text=True,
    ).stdout.strip()
    t = subprocess.run(
        ["git", "-C", str(repo_dir), "rev-parse", "--verify", f"{ref}^{{tree}}"],
        check=True,
        capture_output=True,
        text=True,
    ).stdout.strip()
    return c, t


LEAN_INSPECTOR_TEMPLATE = r"""
import Lean
__IMPORTS__

open Lean

inductive SortAst where
  | base (tex : String)
  | var (name : String)
  | arrow (dom : SortAst) (cod : SortAst)
  | propSort
  | typeSort
  deriving Repr, BEq, DecidableEq

inductive TermAst where
  | var (name : String)
  | sym (tex : String) (args : List TermAst)
  deriving Repr

inductive FormulaAst where
  | pred (tex : String) (args : List TermAst)
  | varPred (name : String) (args : List TermAst)
  | eq (lhs : TermAst) (rhs : TermAst)
  | notF (inner : FormulaAst)
  | andF (lhs : FormulaAst) (rhs : FormulaAst)
  | iffF (lhs : FormulaAst) (rhs : FormulaAst)
  | impF (lhs : FormulaAst) (rhs : FormulaAst)
  | forallF (v : String) (s : SortAst) (body : FormulaAst)
  | existsF (v : String) (s : SortAst) (body : FormulaAst)
  deriving Repr

def renderSort : SortAst → String
  | SortAst.base t => t
  | SortAst.var v => v
  | SortAst.propSort => "\\mathsf{Prop}"
  | SortAst.typeSort => "\\mathsf{Type}"
  | SortAst.arrow (SortAst.arrow a b) c =>
    "\\left(" ++ renderSort (SortAst.arrow a b) ++ "\\right) \\to " ++ renderSort c
  | SortAst.arrow a b => renderSort a ++ " \\to " ++ renderSort b

def joinComma : List String → String
  | [] => ""
  | [x] => x
  | x :: xs => x ++ ", " ++ joinComma xs

def renderTerm : TermAst → String
  | TermAst.var v => v
  | TermAst.sym t [] => t
  | TermAst.sym t args => t ++ "(" ++ joinComma (args.map renderTerm) ++ ")"

def fPrec : FormulaAst → Nat
  | FormulaAst.forallF _ _ _ => 1
  | FormulaAst.existsF _ _ _ => 1
  | FormulaAst.impF _ _ => 2
  | FormulaAst.iffF _ _ => 3
  | FormulaAst.andF _ _ => 4
  | FormulaAst.notF _ => 5
  | FormulaAst.eq _ _ => 6
  | FormulaAst.pred _ _ => 6
  | FormulaAst.varPred _ _ => 6

def wrapIf (need : Bool) (s : String) : String :=
  if need then "\\left(" ++ s ++ "\\right)" else s

def renderFormula : FormulaAst → String
  | FormulaAst.pred t [] => t
  | FormulaAst.pred t args => t ++ "(" ++ joinComma (args.map renderTerm) ++ ")"
  | FormulaAst.varPred v [] => v
  | FormulaAst.varPred v args => v ++ "(" ++ joinComma (args.map renderTerm) ++ ")"
  | FormulaAst.eq l r => renderTerm l ++ " = " ++ renderTerm r
  | FormulaAst.notF inner =>
    let sub := renderFormula inner
    "\\lnot " ++ wrapIf (fPrec inner < 5) sub
  | FormulaAst.andF l r =>
    let sl := wrapIf (fPrec l < 4) (renderFormula l)
    let sr := wrapIf (fPrec r < 4) (renderFormula r)
    sl ++ " \\land " ++ sr
  | FormulaAst.iffF l r =>
    let sl := wrapIf (fPrec l <= 3) (renderFormula l)
    let sr := wrapIf (fPrec r <= 3) (renderFormula r)
    sl ++ " \\iff " ++ sr
  | FormulaAst.impF l r =>
    let sl := wrapIf (fPrec l <= 2) (renderFormula l)
    let sr := wrapIf (fPrec r < 2) (renderFormula r)
    sl ++ " \\implies " ++ sr
  | FormulaAst.forallF v s body =>
    "\\forall " ++ v ++ " : " ++ renderSort s ++ ",\\; " ++ renderFormula body
  | FormulaAst.existsF v s body =>
    "\\exists " ++ v ++ " : " ++ renderSort s ++ ",\\; " ++ renderFormula body

def baseSortTable : List (Name × String) :=
  [ (`TypifyMetalogic.Decl, "\\mathsf{Decl}"),
    (`TypifyMetalogic.SourceLine, "\\mathsf{Line}"),
    (`TypifyMetalogic.SourceProgram, "\\mathsf{Prog}"),
    (`TypifyMetalogic.FactTerm, "\\mathsf{Term}"),
    (`TypifyMetalogic.TermList, "\\mathsf{TermList}"),
    (`TypifyMetalogic.DeclData, "\\mathsf{DeclData}"),
    (`TypifyMetalogic.AssertionList, "\\mathsf{AssertList}"),
    (`TypifyMetalogic.IndName, "\\mathsf{Name}") ]

def termSymTable : List (Name × Nat × Nat × String) :=
  [ (`TypifyMetalogic.witnessBaseDecl, 0, 0, "\\mathsf{d}_{\\mathsf{base}}"),
    (`TypifyMetalogic.ontologyDecl, 0, 0, "\\mathsf{d}_{\\mathsf{ont}}"),
    (`TypifyMetalogic.witnessKnowsTerm, 0, 0, "\\mathsf{t}_{\\mathsf{knows}}"),
    (`TypifyMetalogic.nameAda, 0, 0, "\\mathsf{ada}"),
    (`TypifyMetalogic.nameBen, 0, 0, "\\mathsf{ben}"),
    (`TypifyMetalogic.recognizeLine, 0, 2, "\\mathsf{recognize}"),
    (`TypifyMetalogic.compileMeta, 0, 1, "\\mathsf{compileMeta}"),
    (`TypifyMetalogic.appendDeclData, 0, 2, "\\mathsf{extend}"),
    (`TypifyMetalogic.readProgram, 0, 2, "\\mathsf{readProg}"),
    (`TypifyMetalogic.encodeAssertions, 0, 1, "\\mathsf{encode}"),
    (`TypifyMetalogic.decodeTerms, 0, 1, "\\mathsf{decode}"),
    (`TypifyMetalogic.singleBinary, 0, 2, "\\mathsf{singleRel}"),
    (`Option.some, 1, 1, "\\mathsf{some}"),
    (`Option.none, 1, 0, "\\mathsf{none}") ]

def predSymTable : List (Name × Nat × String) :=
  [ (`TypifyMetalogic.WellFormedDecl, 1, "\\mathsf{WFDecl}"),
    (`TypifyMetalogic.FragmentLine, 1, "\\mathsf{FragLine}"),
    (`TypifyMetalogic.DerivesLine, 3, "\\mathsf{Derives}"),
    (`TypifyMetalogic.AcceptsLine, 2, "\\mathsf{Accepts}"),
    (`TypifyMetalogic.FreshExtension, 2, "\\mathsf{FreshExt}"),
    (`TypifyMetalogic.AdmissibleAssertions, 1, "\\mathsf{Admissible}"),
    (`Nonempty, 1, "\\mathsf{Nonempty}") ]

def lookupSortBase (n : Name) : Option String :=
  match baseSortTable.find? (fun p => p.1 == n) with
  | some (_, s) => some s
  | none => none

def lookupTermSym (n : Name) : Option (Nat × Nat × String) :=
  match termSymTable.find? (fun p => p.1 == n) with
  | some (_, dropN, arity, s) => some (dropN, arity, s)
  | none => none

def lookupPredSym (n : Name) : Option (Nat × String) :=
  match predSymTable.find? (fun p => p.1 == n) with
  | some (_, arity, s) => some (arity, s)
  | none => none

def sanitizeBinderBase (n : Name) : String :=
  let raw := match n with
    | Name.str _ s => s
    | _ => "x"
  let base := ((raw.splitOn "._").headD "x")
  if base == "t₁" then "t_{1}"
  else if base == "t₂" then "t_{2}"
  else if base == "dBase" then "d_{\\mathsf{base}}"
  else if base == "dExt" then "d_{\\mathsf{ext}}"
  else if base == "val" then "\\mathsf{val}"
  else base

def freshBinder (n : Name) (ctx : List (String × Bool)) : String :=
  let used := ctx.map Prod.fst
  let b := sanitizeBinderBase n
  if !(used.contains b) then b
  else
    let rec pick (k : Nat) : Nat → String
      | 0 => b ++ "_{99}"
      | fuel + 1 =>
        let cand := b ++ "_{" ++ toString k ++ "}"
        if !(used.contains cand) then cand else pick (k + 1) fuel
    pick 1 50

def getAppSpine (e : Expr) : Expr × List Expr :=
  let rec go (cur : Expr) (acc : List Expr) : Expr × List Expr :=
    match cur with
    | Expr.app f a => go f (a :: acc)
    | _ => (cur, acc)
  go e []

partial def reflectSort (ctx : List (String × Bool)) (e : Expr) : Except String SortAst := do
  match e with
  | Expr.sort (Level.zero) => pure SortAst.propSort
  | Expr.sort (Level.succ Level.zero) => pure SortAst.typeSort
  | Expr.const n _ =>
    match lookupSortBase n with
    | some tex => pure (SortAst.base tex)
    | none => throw s!"unmapped sort constant: {n}"
  | Expr.bvar idx =>
    match ctx[idx]? with
    | some (v, true) => pure (SortAst.var v)
    | some (v, false) => throw s!"non-type binder {v} used as sort"
    | none => throw s!"unbound sort bvar #{idx}"
  | Expr.forallE _ dom cod _ =>
    if cod.hasLooseBVar 0 then
      throw "dependent function sort unsupported"
    let ds ← reflectSort ctx dom
    let cs ← reflectSort (("_", false) :: ctx) cod
    pure (SortAst.arrow ds cs)
  | _ => throw s!"unsupported sort expr: {repr e}"

partial def reflectTerm (ctx : List (String × Bool)) (e : Expr) : Except String TermAst := do
  let (hd, args) := getAppSpine e
  match hd with
  | Expr.bvar idx =>
    match ctx[idx]? with
    | some (v, _) =>
      let rargs ← args.mapM (reflectTerm ctx)
      if rargs.isEmpty then pure (TermAst.var v)
      else pure (TermAst.sym v rargs)
    | none => throw s!"unbound term bvar #{idx}"
  | Expr.const n _ =>
    match lookupTermSym n with
    | some (dropN, arity, tex) =>
      if args.length != dropN + arity then
        throw s!"term arity mismatch on {n}: got {args.length}, expected {dropN + arity}"
      let kept := args.drop dropN
      let rargs ← kept.mapM (reflectTerm ctx)
      pure (TermAst.sym tex rargs)
    | none =>
      match lookupSortBase n with
      | some tex =>
        if args.isEmpty then pure (TermAst.sym tex [])
        else throw s!"unexpected arguments on sort constant {n}"
      | none => throw s!"unmapped term constant: {n}"
  | _ => throw s!"unsupported term head: {repr hd}"

partial def reflectFormula (ctx : List (String × Bool)) (e : Expr) : Except String FormulaAst := do
  match e with
  | Expr.forallE name dom body _ =>
    match reflectSort ctx dom with
    | Except.ok s =>
      let v := freshBinder name ctx
      let isTy := s == SortAst.typeSort
      let b ← reflectFormula ((v, isTy) :: ctx) body
      pure (FormulaAst.forallF v s b)
    | Except.error _ =>
      if body.hasLooseBVar 0 then
        throw s!"dependent proof binder unsupported: {name}"
      let lhs ← reflectFormula ctx dom
      let rhs ← reflectFormula (("_", false) :: ctx) body
      pure (FormulaAst.impF lhs rhs)
  | _ =>
    let (hd, args) := getAppSpine e
    match hd with
    | Expr.const `And _ =>
      if h : args.length = 2 then
        let l ← reflectFormula ctx args[0]
        let r ← reflectFormula ctx args[1]
        pure (FormulaAst.andF l r)
      else throw "And arity != 2"
    | Expr.const `Iff _ =>
      if h : args.length = 2 then
        let l ← reflectFormula ctx args[0]
        let r ← reflectFormula ctx args[1]
        pure (FormulaAst.iffF l r)
      else throw "Iff arity != 2"
    | Expr.const `Not _ =>
      if h : args.length = 1 then
        let sub ← reflectFormula ctx args[0]
        pure (FormulaAst.notF sub)
      else throw "Not arity != 1"
    | Expr.const `Eq _ =>
      if h : args.length = 3 then
        let l ← reflectTerm ctx args[1]
        let r ← reflectTerm ctx args[2]
        pure (FormulaAst.eq l r)
      else throw "Eq arity != 3"
    | Expr.const `Exists _ =>
      if h : args.length = 2 then
        match args[1] with
        | Expr.lam name dom body _ =>
          let s ← reflectSort ctx dom
          let v := freshBinder name ctx
          let isTy := s == SortAst.typeSort
          let b ← reflectFormula ((v, isTy) :: ctx) body
          pure (FormulaAst.existsF v s b)
        | other => throw s!"Exists argument is not lambda: {repr other}"
      else throw "Exists arity != 2"
    | Expr.const `TypifyMetalogic.SatisfiesSource _ =>
      if h : args.length = 5 then
        let rargs ← args.mapM (reflectTerm ctx)
        pure (FormulaAst.pred "\\mathsf{SatSrc}" rargs)
      else throw "SatisfiesSource arity != 5"
    | Expr.const `TypifyMetalogic.SatisfiesOutput _ =>
      if h : args.length = 5 then
        let rargs ← args.mapM (reflectTerm ctx)
        pure (FormulaAst.pred "\\mathsf{SatOut}" rargs)
      else throw "SatisfiesOutput arity != 5"
    | Expr.const n _ =>
      match lookupPredSym n with
      | some (arity, tex) =>
        if args.length != arity then
          throw s!"predicate arity mismatch on {n}: got {args.length}, expected {arity}"
        let rargs ← args.mapM (reflectTerm ctx)
        pure (FormulaAst.pred tex rargs)
      | none => throw s!"unmapped predicate constant: {n}"
    | Expr.bvar idx =>
      match ctx[idx]? with
      | some (v, _) =>
        let rargs ← args.mapM (reflectTerm ctx)
        pure (FormulaAst.varPred v rargs)
      | none => throw s!"unbound predicate bvar #{idx}"
    | _ => throw s!"unsupported formula expr: {repr e}"

def runRendererSelfTests : Except String Nat := do
  let p := FormulaAst.varPred "P" [TermAst.var "x"]
  let q := FormulaAst.varPred "Q" [TermAst.var "x"]
  let r := FormulaAst.varPred "R" [TermAst.var "x"]
  let leftAssoc := renderFormula (FormulaAst.impF (FormulaAst.impF p q) r)
  let rightAssoc := renderFormula (FormulaAst.impF p (FormulaAst.impF q r))
  if leftAssoc == rightAssoc then
    throw "implication associativity collision"
  if renderFormula (FormulaAst.impF p q) == renderFormula (FormulaAst.impF q p) then
    throw "implication direction collision"
  let relAB := renderFormula (FormulaAst.varPred "R" [TermAst.var "a", TermAst.var "b"])
  let relBA := renderFormula (FormulaAst.varPred "R" [TermAst.var "b", TermAst.var "a"])
  if relAB == relBA then
    throw "binary relation argument order collision"
  let eqAB := renderFormula (FormulaAst.eq (TermAst.var "a") (TermAst.var "b"))
  let eqBA := renderFormula (FormulaAst.eq (TermAst.var "b") (TermAst.var "a"))
  if eqAB == eqBA then
    throw "equality argument order collision"
  let qy := FormulaAst.varPred "Q" [TermAst.var "y"]
  let scopedLeft := renderFormula (FormulaAst.andF (FormulaAst.forallF "x" (SortAst.base "\\mathsf{Decl}") p) qy)
  let scopedWide := renderFormula (FormulaAst.forallF "x" (SortAst.base "\\mathsf{Decl}") (FormulaAst.andF p qy))
  if scopedLeft == scopedWide then
    throw "quantifier scope vs conjunction collision"
  let shadowExpr :=
    Expr.forallE `x (Expr.const `TypifyMetalogic.Decl [])
      (Expr.forallE `x (Expr.const `TypifyMetalogic.Decl [])
        (Expr.app (Expr.app (Expr.app (Expr.const `Eq [Level.succ Level.zero])
          (Expr.const `TypifyMetalogic.Decl [])) (Expr.bvar 1)) (Expr.bvar 0))
        BinderInfo.default)
      BinderInfo.default
  let shadowAst ← reflectFormula [] shadowExpr
  let shadowTex := renderFormula shadowAst
  if shadowTex != "\\forall x : \\mathsf{Decl},\\; \\forall x_{1} : \\mathsf{Decl},\\; x = x_{1}" then
    throw s!"unexpected shadowed binder rendering: {shadowTex}"
  let unmappedExpr := Expr.const `UnmappedSymbol []
  match reflectFormula [] unmappedExpr with
  | Except.ok _ => throw "unmapped symbol was not rejected"
  | Except.error _ => pure ()
  pure 7

def jsonEsc (s : String) : String :=
  let rec go : List Char → String
    | [] => ""
    | '"' :: cs => "\\\"" ++ go cs
    | '\\' :: cs => "\\\\" ++ go cs
    | '\n' :: cs => "\\n" ++ go cs
    | '\t' :: cs => "\\t" ++ go cs
    | c :: cs => String.singleton c ++ go cs
  "\"" ++ go s.toList ++ "\""

def jsonStrList (xs : List String) : String :=
  "[" ++ joinComma (xs.map jsonEsc) ++ "]"

def evalConformanceJson : IO String := do
  let raw ← IO.FS.readFile "__CONFORMANCE_JSON__"
  let parsed ← IO.ofExcept (Json.parse raw)
  let declsJson ← IO.ofExcept (parsed.getObjValAs? (Array Json) "declarations")
  let progsJson ← IO.ofExcept (parsed.getObjValAs? (Array Json) "programs")
  let mut declMap : List (String × Option TypifyMetalogic.Decl × String) := []
  let mut declResults : List String := []
  for dj in declsJson do
    let did ← IO.ofExcept (dj.getObjValAs? String "id")
    let lang ← IO.ofExcept (dj.getObjValAs? String "language")
    let mlinesJson ← IO.ofExcept (dj.getObjValAs? (Array Json) "metaLines")
    let mut mlines : List TypifyMetalogic.MetaLine := []
    for mj in mlinesJson do
      let kind ← IO.ofExcept (mj.getObjValAs? String "kind")
      let ml ← match kind with
        | "isClass" => do
          let nm ← IO.ofExcept (mj.getObjValAs? String "name")
          let litsArr ← IO.ofExcept (mj.getObjValAs? (Array String) "literals")
          pure (TypifyMetalogic.MetaLine.isClass nm litsArr.toList)
        | "termUnary" => do
          let ct ← IO.ofExcept (mj.getObjValAs? String "ctor")
          let c1 ← IO.ofExcept (mj.getObjValAs? String "argClass")
          pure (TypifyMetalogic.MetaLine.termUnary ct c1)
        | "termBinary" => do
          let ct ← IO.ofExcept (mj.getObjValAs? String "ctor")
          let c1 ← IO.ofExcept (mj.getObjValAs? String "argClass1")
          let c2 ← IO.ofExcept (mj.getObjValAs? String "argClass2")
          pure (TypifyMetalogic.MetaLine.termBinary ct c1 c2)
        | "formUnary" => do
          let vb ← IO.ofExcept (mj.getObjValAs? String "verb")
          let sl ← IO.ofExcept (mj.getObjValAs? String "slot")
          pure (TypifyMetalogic.MetaLine.formUnary vb sl)
        | "doesUnary" => do
          let ct ← IO.ofExcept (mj.getObjValAs? String "ctor")
          let sl ← IO.ofExcept (mj.getObjValAs? String "slot")
          pure (TypifyMetalogic.MetaLine.doesUnary ct sl)
        | "formBinary" => do
          let vb ← IO.ofExcept (mj.getObjValAs? String "verb")
          let s1 ← IO.ofExcept (mj.getObjValAs? String "slot1")
          let sp ← IO.ofExcept (mj.getObjValAs? String "sep")
          let s2 ← IO.ofExcept (mj.getObjValAs? String "slot2")
          pure (TypifyMetalogic.MetaLine.formBinary vb s1 sp s2)
        | "doesBinary" => do
          let ct ← IO.ofExcept (mj.getObjValAs? String "ctor")
          let s1 ← IO.ofExcept (mj.getObjValAs? String "slot1")
          let s2 ← IO.ofExcept (mj.getObjValAs? String "slot2")
          pure (TypifyMetalogic.MetaLine.doesBinary ct s1 s2)
        | other => throw (IO.userError s!"unknown metaLine kind: {other}")
      mlines := mlines ++ [ml]
    let compiled := TypifyMetalogic.compileMeta mlines
    let rendered := TypifyMetalogic.renderTypifySource lang mlines
    declMap := declMap ++ [(did, compiled, rendered)]
    let okStr := if compiled.isSome then "true" else "false"
    declResults := declResults ++ [
      "{\"id\":" ++ jsonEsc did ++ ",\"valid\":" ++ okStr ++ ",\"source\":" ++ jsonEsc rendered ++ "}"
    ]
  let mut progResults : List String := []
  for pj in progsJson do
    let pid ← IO.ofExcept (pj.getObjValAs? String "id")
    let did ← IO.ofExcept (pj.getObjValAs? String "declarationId")
    let linesArr ← IO.ofExcept (pj.getObjValAs? (Array String) "lines")
    let compiledOpt := match declMap.find? (fun r => r.1 == did) with
      | some (_, c, _) => c
      | none => none
    match compiledOpt with
    | none => throw (IO.userError s!"program {pid} references invalid declaration {did}")
    | some d =>
      match TypifyMetalogic.readProgram d linesArr.toList with
      | none =>
        progResults := progResults ++ [
          "{\"id\":" ++ jsonEsc pid ++ ",\"accepted\":false,\"terms\":null}"
        ]
      | some ts =>
        let tStrs := ts.map (fun ft => jsonStrList (ft.ctor :: ft.args))
        progResults := progResults ++ [
          "{\"id\":" ++ jsonEsc pid ++ ",\"accepted\":true,\"terms\":[" ++ joinComma tStrs ++ "]}"
        ]
  pure ("{\"declarations\":[" ++ joinComma declResults ++ "],\"programs\":[" ++ joinComma progResults ++ "]}")

def collectAxiomsIO (env : Environment) (cname : Name) : IO (Array Name) := do
  let ctx : Core.Context := { fileName := "<audit>", fileMap := default }
  let st : Core.State := { env := env }
  let (axs, _) ← Core.CoreM.toIO (Lean.collectAxioms cname) ctx st
  pure axs

def runAudit : IO Unit := do
  initSearchPath (← findSysroot) [System.FilePath.mk ".lake/build/lib/lean"]
  let targetMods : Array Name := #[__MODULES_ARRAY__]
  let imports := targetMods.map (fun m => { module := m : Import })
  let env ← importModules imports {}
  let allowedAxioms : List Name := [`Classical.choice, `Quot.sound, `propext]
  let mut ownedCount : Nat := 0
  for (cname, cinfo) in env.constants.map₁.toList do
    match env.getModuleIdxFor? cname with
    | none => pure ()
    | some midx =>
      let modName := env.header.moduleNames[midx.toNat]!
      if targetMods.contains modName then
        ownedCount := ownedCount + 1
        if cinfo.isUnsafe && (!cname.isInternal) && (!(toString cname).endsWith "._impl") then
          throw (IO.userError s!"unsafe declaration forbidden: {cname} in {modName}")
        match cinfo with
        | ConstantInfo.axiomInfo _ =>
          throw (IO.userError s!"user-declared axiom forbidden: {cname} in {modName}")
        | ConstantInfo.opaqueInfo _ =>
          throw (IO.userError s!"opaque declaration forbidden: {cname} in {modName}")
        | _ => pure ()
        let axs ← collectAxiomsIO env cname
        for ax in axs.toList do
          if ax == `sorryAx then
            throw (IO.userError s!"sorryAx dependency found in {cname} ({modName})")
          if !(allowedAxioms.contains ax) then
            throw (IO.userError s!"disallowed axiom {ax} in {cname} ({modName})")

  let selfTests ← match runRendererSelfTests with
    | Except.ok k => pure k
    | Except.error msg => throw (IO.userError s!"renderer self-test failed: {msg}")

  let laws : List (String × Name × Name × Name × Name) := [
    __LAW_TUPLES__
  ]
  let mut lawEntries : List String := []
  for (lid, stmtMod, stmtConst, proofMod, proofConst) in laws do
    let stmtIdx ← match env.getModuleIdxFor? stmtConst with
      | some idx => pure idx
      | none => throw (IO.userError s!"missing statement constant {stmtConst}")
    let actualStmtMod := env.header.moduleNames[stmtIdx.toNat]!
    if actualStmtMod != stmtMod then
      throw (IO.userError s!"statement {stmtConst} owned by {actualStmtMod}, expected {stmtMod}")
    let stmtBody ← match env.find? stmtConst with
      | some (ConstantInfo.defnInfo v) =>
        if v.type != Expr.sort Level.zero then
          throw (IO.userError s!"statement {stmtConst} does not have type Prop")
        pure v.value
      | _ => throw (IO.userError s!"statement {stmtConst} is not a def")

    let proofIdx ← match env.getModuleIdxFor? proofConst with
      | some idx => pure idx
      | none => throw (IO.userError s!"missing proof theorem {proofConst}")
    let actualProofMod := env.header.moduleNames[proofIdx.toNat]!
    if actualProofMod != proofMod then
      throw (IO.userError s!"proof {proofConst} owned by {actualProofMod}, expected {proofMod}")
    let proofModData := env.header.moduleData[proofIdx.toNat]!
    let directImports := proofModData.imports.toList.map Import.module
    if !(directImports.contains stmtMod) then
      throw (IO.userError s!"proof module {proofMod} does not directly import {stmtMod}")
    match env.find? proofConst with
    | some (ConstantInfo.thmInfo tv) =>
      if tv.type != Expr.const stmtConst [] then
        throw (IO.userError s!"theorem {proofConst} has type {repr tv.type}, expected {stmtConst}")
    | _ => throw (IO.userError s!"{proofConst} is not a theorem")

    let axs ← collectAxiomsIO env proofConst
    let axNames := (axs.toList.map toString).mergeSort (· < ·)
    let ast ← match reflectFormula [] stmtBody with
      | Except.ok f => pure f
      | Except.error err => throw (IO.userError s!"formula reflection failed on {stmtConst}: {err}")
    let tex := renderFormula ast
    lawEntries := lawEntries ++ [
      "{\"id\":" ++ jsonEsc lid ++
      ",\"statementModule\":" ++ jsonEsc (toString stmtMod) ++
      ",\"statementConst\":" ++ jsonEsc (toString stmtConst) ++
      ",\"proofModule\":" ++ jsonEsc (toString proofMod) ++
      ",\"proofConst\":" ++ jsonEsc (toString proofConst) ++
      ",\"axioms\":" ++ jsonStrList axNames ++
      ",\"tex\":" ++ jsonEsc tex ++ "}"
    ]

  let confJson ← evalConformanceJson
  let out :=
    "{\"ownedDeclarationsChecked\":" ++ toString ownedCount ++
    ",\"rendererSelfTests\":" ++ toString selfTests ++
    ",\"laws\":[" ++ joinComma lawEntries ++ "]" ++
    ",\"conformance\":" ++ confJson ++ "}"
  IO.FS.writeFile "__OUT_JSON__" out

#eval runAudit
"""


def find_lean_bin(explicit_bin: str | None) -> tuple[Path, Path]:
    if explicit_bin:
        b = Path(explicit_bin)
        return b / "lean", b / "lake"
    lean_exe = shutil.which("lean")
    lake_exe = shutil.which("lake")
    if lean_exe and lake_exe:
        return Path(lean_exe), Path(lake_exe)
    raise GateFailure(
        "missing-lean-toolchain", "lean and lake not found on PATH or --lean-bin"
    )


def run_typify_reader(
    repo_dir: Path, reader_bin: str | None, args: list[str], cwd: Path
) -> subprocess.CompletedProcess:
    env = dict(os.environ)
    if reader_bin:
        cmd = [reader_bin, *args]
    else:
        racket_exe = shutil.which("racket")
        if not racket_exe:
            raise GateFailure(
                "missing-racket", "neither --reader-bin nor racket on PATH"
            )
        cmd = [racket_exe, str(repo_dir / "src/main.rkt"), *args]
    return subprocess.run(
        cmd, cwd=str(cwd), env=env, capture_output=True, text=True, check=False
    )


def check_source_side(
    repo_dir: Path,
    source_ref: str | None,
    allow_uncommitted: bool,
    lean_bin: str | None,
    reader_bin: str | None,
) -> dict:
    lean_exe, lake_exe = find_lean_bin(lean_bin)
    ver_proc = subprocess.run(
        [str(lean_exe), "--version"], check=True, capture_output=True, text=True
    )
    ver_line = ver_proc.stdout.splitlines()[0].strip()
    if EXPECTED_LEAN_VERSION_SUBSTR not in ver_line:
        raise GateFailure(
            "lean-version-mismatch",
            f"expected '{EXPECTED_LEAN_VERSION_SUBSTR}', got '{ver_line}'",
        )

    tracked_files = list_repo_files(repo_dir, source_ref, allow_uncommitted)
    lean_files = [f for f in tracked_files if f.endswith(".lean")]

    toolchain_text = (
        read_repo_blob(repo_dir, "lean/lean-toolchain", source_ref, allow_uncommitted)
        .decode("utf-8")
        .strip()
    )
    if toolchain_text != EXPECTED_TOOLCHAIN:
        raise GateFailure(
            "toolchain-file-mismatch",
            f"lean-toolchain is '{toolchain_text}', expected '{EXPECTED_TOOLCHAIN}'",
        )

    lakefile_text = read_repo_blob(
        repo_dir, "lean/lakefile.toml", source_ref, allow_uncommitted
    ).decode("utf-8")

    expected_paths = []
    for law in FROZEN_LAWS:
        expected_paths.append(law["statementPath"])
        expected_paths.append(law["proofPath"])

    if len(set(expected_paths)) != 2 * len(FROZEN_LAWS):
        raise GateFailure(
            "duplicate-registered-lean-path", "registered law paths reuse a file"
        )

    lean_contents: dict[str, str] = {}
    all_modules: list[str] = []
    for rel in lean_files:
        if not rel.startswith("lean/"):
            raise GateFailure(
                "stray-lean-file", f"tracked Lean file outside lean/: {rel}"
            )
        sub = rel[len("lean/") :]
        mod_name = sub[: -len(".lean")].replace("/", ".")
        all_modules.append(mod_name)
        raw = read_repo_blob(repo_dir, rel, source_ref, allow_uncommitted).decode(
            "utf-8"
        )
        scan_lean_formatting(rel, raw)
        lean_contents[rel] = raw

    with tempfile.TemporaryDirectory(prefix="metalogic-lean-build-") as tmp_str:
        tmp = Path(tmp_str)
        lean_work = tmp / "lean"
        lean_work.mkdir(parents=True)
        for cfg in (
            "lean/lean-toolchain",
            "lean/lakefile.toml",
            "lean/lake-manifest.json",
        ):
            if cfg in tracked_files:
                (lean_work / Path(cfg).name).write_bytes(
                    read_repo_blob(repo_dir, cfg, source_ref, allow_uncommitted)
                )
        for rel, content in lean_contents.items():
            dest = tmp / rel
            dest.parent.mkdir(parents=True, exist_ok=True)
            dest.write_text(content, encoding="utf-8")

        env = dict(os.environ)
        env["PATH"] = f"{lean_exe.parent}:{env.get('PATH', '')}"

        if not all_modules:
            raise GateFailure("no-lean-modules", "zero tracked .lean files found")

        build_proc = subprocess.run(
            [str(lake_exe), "build", *all_modules],
            cwd=str(lean_work),
            env=env,
            capture_output=True,
            text=True,
        )
        if build_proc.returncode != 0:
            raise GateFailure(
                "lake-build-failed",
                f"lake build failed:\n{build_proc.stdout}\n{build_proc.stderr}",
            )

        if not set(expected_paths).issubset(set(lean_files)):
            raise GateFailure(
                "lean-inventory-mismatch",
                f"tracked .lean files {sorted(lean_files)} missing required files from {sorted(expected_paths)}",
            )

        conf_bytes = read_repo_blob(
            repo_dir, "test/metalogic-conformance.json", source_ref, allow_uncommitted
        )
        conf_path = tmp / "conformance.json"
        conf_path.write_bytes(conf_bytes)
        out_json_path = tmp / "audit-out.json"

        imports_block = "\n".join(f"import {m}" for m in sorted(set(all_modules)))
        mods_array = ", ".join(f"`{m}" for m in sorted(set(all_modules)))
        law_tuples = ",\n    ".join(
            f'("{l["id"]}", `{l["statementModule"]}, `{l["statementConst"]}, `{l["proofModule"]}, `{l["proofConst"]})'
            for l in FROZEN_LAWS
        )
        probe_code = (
            LEAN_INSPECTOR_TEMPLATE.replace("__IMPORTS__", imports_block)
            .replace("__MODULES_ARRAY__", mods_array)
            .replace("__LAW_TUPLES__", law_tuples)
            .replace("__CONFORMANCE_JSON__", conf_path.as_posix())
            .replace("__OUT_JSON__", out_json_path.as_posix())
        )
        probe_path = tmp / "InspectorProbe.lean"
        probe_path.write_text(probe_code, encoding="utf-8")

        probe_proc = subprocess.run(
            [str(lake_exe), "env", str(lean_exe), str(probe_path)],
            cwd=str(lean_work),
            env=env,
            capture_output=True,
            text=True,
        )
        if probe_proc.returncode != 0 or not out_json_path.is_file():
            raise GateFailure(
                "kernel-audit-failed",
                f"Lean kernel inspector failed:\n{probe_proc.stdout}\n{probe_proc.stderr}",
            )

        for rel, raw in lean_contents.items():
            scan_lean_keywords(rel, raw)

        if len(lean_files) != 2 * len(FROZEN_LAWS) or sorted(lean_files) != sorted(
            expected_paths
        ):
            raise GateFailure(
                "lean-inventory-mismatch",
                f"tracked .lean files {sorted(lean_files)} != expected 2N={2 * len(FROZEN_LAWS)} files {sorted(expected_paths)}",
            )

        for law in FROZEN_LAWS:
            for mkey in ("statementModule", "proofModule"):
                if f'"{law[mkey]}"' not in lakefile_text:
                    raise GateFailure(
                        "lakefile-target-missing",
                        f"{law[mkey]} missing from lakefile.toml",
                    )

        audit_data = json.loads(out_json_path.read_text(encoding="utf-8"))

        conf_spec = json.loads(conf_bytes.decode("utf-8"))
        lean_decls = {d["id"]: d for d in audit_data["conformance"]["declarations"]}
        lean_progs = {p["id"]: p for p in audit_data["conformance"]["programs"]}
        decl_spec_by_id = {d["id"]: d for d in conf_spec["declarations"]}

        conformance_checked = 0
        racket_dir = tmp / "racket-conformance"
        racket_dir.mkdir()

        for dspec in conf_spec["declarations"]:
            did = dspec["id"]
            ld = lean_decls[did]
            if ld["valid"] != dspec["expectValid"]:
                raise GateFailure(
                    "lean-decl-verdict-mismatch",
                    f"{did}: Lean valid={ld['valid']} != {dspec['expectValid']}",
                )
            dfile = racket_dir / f"{did}.typify"
            dfile.write_text(ld["source"], encoding="utf-8")
            proc = run_typify_reader(
                repo_dir, reader_bin, ["check", str(dfile), "--json"], racket_dir
            )
            rdoc = json.loads(proc.stdout)
            rok = bool(rdoc.get("ok") is True and proc.returncode == 0)
            if rok != dspec["expectValid"]:
                raise GateFailure(
                    "reader-decl-verdict-mismatch",
                    f"{did}: reader ok={rok} != {dspec['expectValid']}",
                )
            if not dspec["expectValid"]:
                codes = [diag.get("code") for diag in rdoc.get("diagnostics", [])]
                if dspec["expectedDiagnosticCode"] not in codes:
                    raise GateFailure(
                        "reader-decl-code-mismatch",
                        f"{did}: expected {dspec['expectedDiagnosticCode']}, got {codes}",
                    )
            conformance_checked += 1

        for pspec in conf_spec["programs"]:
            pid = pspec["id"]
            did = pspec["declarationId"]
            dspec = decl_spec_by_id[did]
            lp = lean_progs[pid]
            if lp["accepted"] != pspec["expectAccepted"]:
                raise GateFailure(
                    "lean-prog-verdict-mismatch",
                    f"{pid}: Lean accepted={lp['accepted']}",
                )
            dfile = racket_dir / f"{did}.typify"
            pfile = racket_dir / f"{pid}.prog"
            pfile.write_text("\n".join(pspec["lines"]) + "\n", encoding="utf-8")
            proc = run_typify_reader(
                repo_dir,
                reader_bin,
                ["check", str(dfile), dspec["language"], str(pfile), "--json"],
                racket_dir,
            )
            rdoc = json.loads(proc.stdout)
            rok = bool(rdoc.get("ok") is True and proc.returncode == 0)
            if rok != pspec["expectAccepted"]:
                raise GateFailure(
                    "reader-prog-verdict-mismatch",
                    f"{pid}: reader ok={rok} != {pspec['expectAccepted']}",
                )
            if pspec["expectAccepted"]:
                if (
                    lp["terms"] != pspec["expectedTerms"]
                    or rdoc.get("terms") != pspec["expectedTerms"]
                ):
                    raise GateFailure(
                        "cross-reader-terms-mismatch",
                        f"{pid}: Lean={lp['terms']} Reader={rdoc.get('terms')} Expected={pspec['expectedTerms']}",
                    )
            else:
                codes = [diag.get("code") for diag in rdoc.get("diagnostics", [])]
                if pspec["expectedDiagnosticCode"] not in codes:
                    raise GateFailure(
                        "reader-prog-code-mismatch",
                        f"{pid}: expected {pspec['expectedDiagnosticCode']}, got {codes}",
                    )
            conformance_checked += 1

    src_digests = {
        rel: sha256_bytes(lean_contents[rel].encode("utf-8"))
        for rel in sorted(lean_contents)
    }
    result = {
        "leanVersion": ver_line,
        "toolchainPin": EXPECTED_TOOLCHAIN,
        "allowedAxioms": ALLOWED_AXIOMS,
        "trackedLeanFiles": src_digests,
        "ownedDeclarationsChecked": audit_data["ownedDeclarationsChecked"],
        "rendererSelfTests": audit_data["rendererSelfTests"],
        "conformanceCasesVerified": conformance_checked,
        "laws": audit_data["laws"],
    }
    if not allow_uncommitted:
        source_sha, source_tree = resolve_git_commit_and_tree(repo_dir, source_ref)
        result["sourceCommit"] = source_sha
        result["sourceTree"] = source_tree
    return result


def extract_markdown_links(md_text: str) -> list[tuple[str, str]]:
    out = []
    in_fence = False
    for line in md_text.split("\n"):
        stripped = line.lstrip()
        if stripped.startswith("```"):
            in_fence = not in_fence
            continue
        if in_fence:
            continue
        for m in re.finditer(r"(?<!!)\[([^\]]+)\]\(([^)]+)\)", line):
            out.append((m.group(1), m.group(2)))
    return out


def extract_display_math_blocks(md_text: str) -> list[str]:
    blocks = []
    lines = md_text.split("\n")
    i = 0
    in_fence = False
    while i < len(lines):
        s = lines[i].lstrip()
        if s.startswith("```"):
            in_fence = not in_fence
            i += 1
            continue
        if not in_fence and lines[i].strip() == "$$":
            j = i + 1
            body = []
            while j < len(lines) and lines[j].strip() != "$$":
                body.append(lines[j])
                j += 1
            if j >= len(lines):
                raise GateFailure(
                    "unclosed-display-math", "unclosed $$ display-math block"
                )
            blocks.append("\n".join(body).strip())
            i = j + 1
            continue
        i += 1
    return blocks


def check_full_gate(
    repo_dir: Path,
    wiki_dir: Path,
    source_ref: str | None,
    wiki_ref: str | None,
    expected_source_sha: str | None,
    allow_uncommitted: bool,
    lean_bin: str | None,
    reader_bin: str | None,
    allow_placeholder_sha: bool = False,
) -> dict:
    src_result = check_source_side(
        repo_dir, source_ref, allow_uncommitted, lean_bin, reader_bin
    )
    if expected_source_sha:
        commit_for_links = expected_source_sha
        source_sha, source_tree = (
            expected_source_sha,
            "working-copy"
            if allow_uncommitted
            else resolve_git_commit_and_tree(repo_dir, source_ref)[1],
        )
        if not allow_uncommitted:
            actual_sha, source_tree = resolve_git_commit_and_tree(repo_dir, source_ref)
            if actual_sha != expected_source_sha:
                raise GateFailure(
                    "source-sha-mismatch", f"HEAD {actual_sha} != {expected_source_sha}"
                )
    else:
        source_sha, source_tree = (
            ("working-copy", "working-copy")
            if allow_uncommitted
            else resolve_git_commit_and_tree(repo_dir, source_ref)
        )
        commit_for_links = source_sha

    if commit_for_links == "0" * 40 and not allow_placeholder_sha:
        raise GateFailure(
            "placeholder-source-sha", "0{40} placeholder SHA is forbidden in full gate"
        )
    if not allow_uncommitted and (
        not re.fullmatch(r"[0-9a-f]{40}", commit_for_links)
        or commit_for_links == "0" * 40
    ):
        raise GateFailure(
            "placeholder-source-sha",
            f"Invalid source commit SHA for permanent links: {commit_for_links}",
        )

    wiki_sha, wiki_tree = (
        ("working-copy", "working-copy")
        if allow_uncommitted
        else resolve_git_commit_and_tree(wiki_dir, wiki_ref)
    )
    wiki_files = list_repo_files(wiki_dir, wiki_ref, allow_uncommitted)
    tracked_source_files = set(list_repo_files(repo_dir, source_ref, allow_uncommitted))

    section_pages = [
        f
        for f in wiki_files
        if Path(f).name == WIKI_SECTION_WELCOME
        or (Path(f).name.startswith(WIKI_LAW_PREFIX) and f.endswith(".md"))
    ]
    if WIKI_SECTION_WELCOME not in section_pages:
        raise GateFailure(
            "missing-welcome-page", f"{WIKI_SECTION_WELCOME} missing from wiki"
        )

    discovered_law_pages = [p for p in section_pages if p != WIKI_SECTION_WELCOME]
    expected_law_pages = [l["wikiPage"] for l in FROZEN_LAWS]

    if len(section_pages) != len(FROZEN_LAWS) + 1:
        raise GateFailure(
            "wiki-section-count-mismatch",
            f"found {len(section_pages)} Metalogic pages ({section_pages}), expected {len(FROZEN_LAWS) + 1}",
        )
    if len(set(expected_law_pages)) != len(FROZEN_LAWS):
        raise GateFailure("reused-wiki-page", "registered laws reuse a wiki page")
    if sorted(discovered_law_pages) != sorted(expected_law_pages):
        raise GateFailure(
            "wiki-law-inventory-mismatch",
            f"discovered law pages {sorted(discovered_law_pages)} != registered {sorted(expected_law_pages)}",
        )

    for nav_page in ("Home.md", "Language.md"):
        nav_text = read_repo_blob(
            wiki_dir, nav_page, wiki_ref, allow_uncommitted
        ).decode("utf-8")
        nav_targets = [t.split("#", 1)[0] for _, t in extract_markdown_links(nav_text)]
        if "Metalogic" not in nav_targets:
            raise GateFailure(
                "section-not-discoverable", f"{nav_page} does not link to Metalogic"
            )

    welcome_bytes = read_repo_blob(
        wiki_dir, WIKI_SECTION_WELCOME, wiki_ref, allow_uncommitted
    )
    welcome_text = welcome_bytes.decode("utf-8")
    if "\r" in welcome_text:
        raise GateFailure("wiki-crlf", f"{WIKI_SECTION_WELCOME} contains CR")
    if not welcome_text.startswith("As of v1.0.0\n"):
        raise GateFailure(
            "wiki-stamp-missing",
            f"{WIKI_SECTION_WELCOME} missing 'As of v1.0.0' header",
        )

    welcome_links = extract_markdown_links(welcome_text)
    law_link_targets = [
        t.split("#", 1)[0].rstrip("/").split("/")[-1]
        for _, t in welcome_links
        if t.split("#", 1)[0].rstrip("/").split("/")[-1].startswith("Metalogic-")
    ]
    expected_order = [l["wikiLinkTarget"] for l in FROZEN_LAWS]
    if law_link_targets != expected_order:
        raise GateFailure(
            "welcome-index-mismatch",
            f"Metalogic.md law links {law_link_targets} != expected reading order {expected_order}",
        )

    tex_by_id = {entry["id"]: entry["tex"] for entry in src_result["laws"]}
    verified_pages = {}

    for law in FROZEN_LAWS:
        lid = law["id"]
        page_name = law["wikiPage"]
        raw_page = read_repo_blob(wiki_dir, page_name, wiki_ref, allow_uncommitted)
        page_text = raw_page.decode("utf-8")
        if "\r" in page_text:
            raise GateFailure("wiki-crlf", f"{page_name} contains CR")
        if not page_text.startswith("As of v1.0.0\n"):
            raise GateFailure(
                "wiki-stamp-missing", f"{page_name} missing 'As of v1.0.0' header"
            )

        math_blocks = extract_display_math_blocks(page_text)
        if len(math_blocks) != 1:
            raise GateFailure(
                "display-math-count",
                f"{page_name} has {len(math_blocks)} display-math blocks, expected 1",
            )
        expected_tex = tex_by_id[lid]
        if math_blocks[0] != expected_tex:
            raise GateFailure(
                "formula-correspondence-mismatch",
                f"{page_name} display-math differs from reflected Lean Prop {law['statementConst']}:\n"
                f"  page: {math_blocks[0]}\n  lean: {expected_tex}",
            )

        links = extract_markdown_links(page_text)
        ext_links = [(lbl, url) for lbl, url in links if is_external_or_repo_link(url)]
        if len(ext_links) != 2:
            raise GateFailure(
                "source-link-count",
                f"{page_name} must have exact 2 source links, found {len(ext_links)}: {ext_links}",
            )
        expected_stmt_url = f"https://github.com/{REPO_SLUG}/blob/{commit_for_links}/{law['statementPath']}"
        expected_proof_url = (
            f"https://github.com/{REPO_SLUG}/blob/{commit_for_links}/{law['proofPath']}"
        )
        if ext_links != [
            ("Statement", expected_stmt_url),
            ("Proof", expected_proof_url),
        ]:
            raise GateFailure(
                "source-link-mismatch",
                f"{page_name} links {ext_links} != [('Statement', '{expected_stmt_url}'), ('Proof', '{expected_proof_url}')]",
            )
        if (
            law["statementPath"] not in tracked_source_files
            or law["proofPath"] not in tracked_source_files
        ):
            raise GateFailure(
                "link-escapes-inventory",
                f"{page_name} link target missing from Git inventory",
            )

        verified_pages[page_name] = sha256_bytes(raw_page)

    verified_pages[WIKI_SECTION_WELCOME] = sha256_bytes(welcome_bytes)
    return {
        "schema": "typify-metalogic-gate-v1",
        "pass": True,
        "sourceCommit": source_sha,
        "sourceTree": source_tree,
        "wikiCommit": wiki_sha,
        "wikiTree": wiki_tree,
        "lineEndingPolicy": "LF-only",
        "sectionPages": verified_pages,
        "lean": src_result,
    }


def copy_clean_tree(
    src: Path, dst: Path, files: list[str], ref: str | None, allow_uncommitted: bool
) -> None:
    for rel in files:
        target = dst / rel
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes(read_repo_blob(src, rel, ref, allow_uncommitted))


def run_controls(
    repo_dir: Path,
    wiki_dir: Path,
    source_ref: str | None,
    wiki_ref: str | None,
    expected_source_sha: str | None,
    allow_uncommitted: bool,
    lean_bin: str | None,
    reader_bin: str | None,
    allow_placeholder_sha: bool = False,
) -> dict:
    src_files = list_repo_files(repo_dir, source_ref, allow_uncommitted)
    wiki_files = list_repo_files(wiki_dir, wiki_ref, allow_uncommitted)
    link_sha = expected_source_sha or (
        "working-copy"
        if allow_uncommitted
        else resolve_git_commit_and_tree(repo_dir, source_ref)[0]
    )

    results = []

    green = check_full_gate(
        repo_dir,
        wiki_dir,
        source_ref,
        wiki_ref,
        link_sha,
        allow_uncommitted,
        lean_bin,
        reader_bin,
        allow_placeholder_sha=allow_placeholder_sha,
    )
    results.append(
        {
            "control": "unmodified-pair-green",
            "expected": "green",
            "actual": "green",
            "pass": True,
        }
    )

    def expect_red(name: str, expected_codes: set[str], mutator) -> None:
        with tempfile.TemporaryDirectory(prefix=f"metalogic-ctrl-{name}-") as tmp_str:
            tmp = Path(tmp_str)
            s_copy = tmp / "repo"
            w_copy = tmp / "wiki"
            copy_clean_tree(repo_dir, s_copy, src_files, source_ref, allow_uncommitted)
            copy_clean_tree(wiki_dir, w_copy, wiki_files, wiki_ref, allow_uncommitted)
            mutator(s_copy, w_copy)
            try:
                check_full_gate(
                    s_copy,
                    w_copy,
                    None,
                    None,
                    link_sha,
                    True,
                    lean_bin,
                    reader_bin,
                    allow_placeholder_sha=allow_placeholder_sha,
                )
            except GateFailure as exc:
                ok = exc.code in expected_codes
                results.append(
                    {
                        "control": name,
                        "expected": "red",
                        "actual": "red",
                        "code": exc.code,
                        "pass": ok,
                    }
                )
                if not ok:
                    raise GateFailure(
                        "unexpected-control-code",
                        f"control {name} failed with {exc.code} ({exc.detail}), expected one of {expected_codes}",
                    )
                return
            raise GateFailure(
                "control-passed-unexpectedly",
                f"negative control {name} unexpectedly passed",
            )

    def mut_quantifier(_s: Path, w: Path) -> None:
        p = w / "Metalogic-Formality.md"
        txt = p.read_text(encoding="utf-8")
        assert "\\forall d : \\mathsf{Decl}" in txt
        p.write_text(
            txt.replace(
                "\\forall d : \\mathsf{Decl}", "\\exists d : \\mathsf{Decl}", 1
            ),
            encoding="utf-8",
        )

    def mut_reversed_rel(_s: Path, w: Path) -> None:
        p = w / "Metalogic-Ontology.md"
        txt = p.read_text(encoding="utf-8")
        target = "R(\\mathsf{val}(\\mathsf{ada}), \\mathsf{val}(\\mathsf{ben}))"
        repl = "R(\\mathsf{val}(\\mathsf{ben}), \\mathsf{val}(\\mathsf{ada}))"
        assert target in txt
        p.write_text(txt.replace(target, repl, 1), encoding="utf-8")

    def mut_swapped_proof(s: Path, _w: Path) -> None:
        p = s / "lean/TypifyMetalogic/ExtensionProof.lean"
        txt = p.read_text(encoding="utf-8")
        p.write_text(
            txt.replace(
                "theorem extension_law : ExtensionLaw := by",
                "theorem extension_law : FormalityLaw := formality_law\ntheorem extension_law_unused : ExtensionLaw := by",
                1,
            ),
            encoding="utf-8",
        )

    def mut_orphan_lean(s: Path, _w: Path) -> None:
        extra = s / "lean/TypifyMetalogic/OrphanHelper.lean"
        extra.write_text(
            "namespace TypifyMetalogic\ndef orphanVal : Nat := 1\nend TypifyMetalogic\n",
            encoding="utf-8",
        )

    def mut_unimported_compile_err(s: Path, _w: Path) -> None:
        extra = s / "lean/TypifyMetalogic/UnimportedBroken.lean"
        extra.write_text(
            'namespace TypifyMetalogic\ndef brokenVal : Nat := "not-a-nat"\nend TypifyMetalogic\n',
            encoding="utf-8",
        )

    def mut_unimported_sorry(s: Path, _w: Path) -> None:
        extra = s / "lean/TypifyMetalogic/UnimportedSorry.lean"
        extra.write_text(
            "namespace TypifyMetalogic\ntheorem unimported_claim : False := by sorry\nend TypifyMetalogic\n",
            encoding="utf-8",
        )

    def mut_orphan_wiki(_s: Path, w: Path) -> None:
        extra = w / "Metalogic-Orphan.md"
        extra.write_text("As of v1.0.0\n\nUnindexed page.\n", encoding="utf-8")

    def mut_removed_triple(s: Path, w: Path) -> None:
        (s / "lean/TypifyMetalogic/OntologyStatement.lean").unlink()
        (s / "lean/TypifyMetalogic/OntologyProof.lean").unlink()
        lf = s / "lean/lakefile.toml"
        lf.write_text(
            "\n".join(
                ln
                for ln in lf.read_text(encoding="utf-8").splitlines()
                if "Ontology" not in ln
            )
            + "\n",
            encoding="utf-8",
        )
        (w / "Metalogic-Ontology.md").unlink()
        wm = w / "Metalogic.md"
        wm.write_text(
            "\n".join(
                ln
                for ln in wm.read_text(encoding="utf-8").splitlines()
                if "Metalogic-Ontology" not in ln
            )
            + "\n",
            encoding="utf-8",
        )

    def mut_proof_sorry(s: Path, _w: Path) -> None:
        p = s / "lean/TypifyMetalogic/OntologyProof.lean"
        p.write_text(
            "import TypifyMetalogic.OntologyStatement\nnamespace TypifyMetalogic\ntheorem ontology_law : OntologyLaw := by sorry\nend TypifyMetalogic\n",
            encoding="utf-8",
        )

    def mut_unused_helper_sorry(s: Path, _w: Path) -> None:
        p = s / "lean/TypifyMetalogic/FormalityProof.lean"
        txt = p.read_text(encoding="utf-8")
        p.write_text(
            txt.replace(
                "end TypifyMetalogic",
                "theorem unused_helper_claim : True := by sorry\n\nend TypifyMetalogic",
            ),
            encoding="utf-8",
        )

    def mut_wiki_link_placeholder(_s: Path, w: Path) -> None:
        p = w / "Metalogic-Formality.md"
        txt = p.read_text(encoding="utf-8")
        bad_sha = "f" * 40 if link_sha == "0" * 40 else "0" * 40
        assert f"/blob/{link_sha}/" in txt
        p.write_text(
            txt.replace(f"/blob/{link_sha}/", f"/blob/{bad_sha}/", 1), encoding="utf-8"
        )

    expect_red(
        "changed-visible-quantifier",
        {"formula-correspondence-mismatch"},
        mut_quantifier,
    )
    expect_red(
        "reversed-binary-relation",
        {"formula-correspondence-mismatch"},
        mut_reversed_rel,
    )
    expect_red("swapped-proof-theorem-type", {"kernel-audit-failed"}, mut_swapped_proof)
    expect_red("orphan-lean-file", {"lean-inventory-mismatch"}, mut_orphan_lean)
    expect_red(
        "unimported-module-compile-error",
        {"lake-build-failed"},
        mut_unimported_compile_err,
    )
    expect_red("unimported-module-sorry", {"kernel-audit-failed"}, mut_unimported_sorry)
    expect_red(
        "orphan-wiki-page",
        {"wiki-section-count-mismatch", "wiki-law-inventory-mismatch"},
        mut_orphan_wiki,
    )
    expect_red(
        "removed-required-triple",
        {"lean-inventory-mismatch", "wiki-section-count-mismatch"},
        mut_removed_triple,
    )
    expect_red("proof-sorry-placeholder", {"kernel-audit-failed"}, mut_proof_sorry)
    expect_red(
        "unused-helper-sorry-placeholder",
        {"kernel-audit-failed"},
        mut_unused_helper_sorry,
    )
    expect_red(
        "wiki-link-placeholder-sha", {"source-link-mismatch"}, mut_wiki_link_placeholder
    )

    return {
        "schema": "typify-metalogic-controls-v1",
        "pass": all(r["pass"] for r in results),
        "greenGate": green,
        "controls": results,
    }


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument(
        "--mode", choices=["render-tex", "ci", "full", "controls"], required=True
    )
    ap.add_argument("--repo-dir", default=".")
    ap.add_argument("--wiki-dir")
    ap.add_argument("--source-ref")
    ap.add_argument("--wiki-ref")
    ap.add_argument("--expected-source-sha")
    ap.add_argument("--allow-uncommitted", action="store_true")
    ap.add_argument("--allow-placeholder-sha", action="store_true")
    ap.add_argument("--lean-bin")
    ap.add_argument("--reader-bin")
    ap.add_argument("--output")
    args = ap.parse_args()

    repo_dir = Path(args.repo_dir).resolve()
    try:
        if args.mode in ("render-tex", "ci"):
            res = check_source_side(
                repo_dir,
                args.source_ref,
                args.allow_uncommitted
                if args.mode == "render-tex"
                else args.allow_uncommitted,
                args.lean_bin,
                args.reader_bin,
            )
            payload = {
                "schema": "typify-lean-zero-sorry-v1",
                "pass": True,
                **res,
            }
        elif args.mode == "full":
            if not args.wiki_dir:
                raise GateFailure(
                    "missing-wiki-dir", "--wiki-dir is required for --mode full"
                )
            payload = check_full_gate(
                repo_dir,
                Path(args.wiki_dir).resolve(),
                args.source_ref,
                args.wiki_ref,
                args.expected_source_sha,
                args.allow_uncommitted,
                args.lean_bin,
                args.reader_bin,
                allow_placeholder_sha=args.allow_placeholder_sha,
            )
        else:
            if not args.wiki_dir:
                raise GateFailure(
                    "missing-wiki-dir", "--wiki-dir is required for --mode controls"
                )
            payload = run_controls(
                repo_dir,
                Path(args.wiki_dir).resolve(),
                args.source_ref,
                args.wiki_ref,
                args.expected_source_sha,
                args.allow_uncommitted,
                args.lean_bin,
                args.reader_bin,
                allow_placeholder_sha=args.allow_placeholder_sha,
            )
    except GateFailure as exc:
        err_obj = {"pass": False, "errorCode": exc.code, "detail": exc.detail}
        if args.output:
            Path(args.output).write_text(
                json.dumps(err_obj, indent=2, sort_keys=True) + "\n", encoding="utf-8"
            )
        print(json.dumps(err_obj, sort_keys=True), file=sys.stderr)
        return 1

    out_text = json.dumps(payload, indent=2, sort_keys=True) + "\n"
    if args.output:
        Path(args.output).write_text(out_text, encoding="utf-8")
    print(out_text, end="")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
