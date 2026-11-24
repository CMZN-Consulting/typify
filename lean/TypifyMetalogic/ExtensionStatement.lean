import TypifyMetalogic.FormalityStatement

namespace TypifyMetalogic

def SourceProgram : Type := List SourceLine

def TermList : Type := List FactTerm

inductive MetaLine where
  | isClass (className : String) (literals : List String)
  | termUnary (ctor : String) (argClass : String)
  | termBinary (ctor : String) (argClass1 : String) (argClass2 : String)
  | formUnary (verb : String) (slot : String)
  | doesUnary (ctor : String) (slot : String)
  | formBinary (verb : String) (slot1 : String) (sep : String) (slot2 : String)
  | doesBinary (ctor : String) (slot1 : String) (slot2 : String)
  deriving DecidableEq, Repr

def DeclData : Type := List MetaLine

def appendDeclData (base ext : DeclData) : DeclData :=
  List.append base ext

def parseMetaClauses : List MetaLine → Option (List ClassDecl × List TermDecl × List FormDecl)
  | [] => some ([], [], [])
  | MetaLine.isClass cname lits :: rest =>
    match parseMetaClauses rest with
    | some (cs, ts, fs) => some (ClassDecl.mk cname lits :: cs, ts, fs)
    | none => none
  | MetaLine.termUnary ctor c1 :: rest =>
    match parseMetaClauses rest with
    | some (cs, ts, fs) => some (cs, TermDecl.mk ctor [c1] :: ts, fs)
    | none => none
  | MetaLine.termBinary ctor c1 c2 :: rest =>
    match parseMetaClauses rest with
    | some (cs, ts, fs) => some (cs, TermDecl.mk ctor [c1, c2] :: ts, fs)
    | none => none
  | MetaLine.formUnary verb slot :: MetaLine.doesUnary ctor dslot :: rest =>
    match parseMetaClauses rest with
    | some (cs, ts, fs) => some (cs, ts, FormDecl.unary verb slot ctor dslot :: fs)
    | none => none
  | MetaLine.formBinary verb s1 sep s2 :: MetaLine.doesBinary ctor d1 d2 :: rest =>
    match parseMetaClauses rest with
    | some (cs, ts, fs) => some (cs, ts, FormDecl.binary verb s1 sep s2 ctor d1 d2 :: fs)
    | none => none
  | _ => none

def compileMeta (entries : DeclData) : Option Decl :=
  match parseMetaClauses entries with
  | none => none
  | some (cs, ts, fs) =>
    let d : Decl := { classes := cs, terms := ts, forms := fs }
    if wellFormedDeclBool d then some d else none

def metaClassNames : List MetaLine → List String
  | [] => []
  | MetaLine.isClass cname _ :: rest => cname :: metaClassNames rest
  | _ :: rest => metaClassNames rest

def metaTermCtors : List MetaLine → List String
  | [] => []
  | MetaLine.termUnary ctor _ :: rest => ctor :: metaTermCtors rest
  | MetaLine.termBinary ctor _ _ :: rest => ctor :: metaTermCtors rest
  | _ :: rest => metaTermCtors rest

def metaFormVerbs : List MetaLine → List String
  | [] => []
  | MetaLine.formUnary verb _ :: rest => verb :: metaFormVerbs rest
  | MetaLine.formBinary verb _ _ _ :: rest => verb :: metaFormVerbs rest
  | _ :: rest => metaFormVerbs rest

def disjointStrings (xs ys : List String) : Bool :=
  xs.all (fun x => !(ys.contains x))

def freshExtensionBool (base ext : DeclData) : Bool :=
  (parseMetaClauses ext).isSome &&
  disjointStrings (metaClassNames ext) (metaClassNames base) &&
  disjointStrings (metaTermCtors ext) (metaTermCtors base) &&
  disjointStrings (metaFormVerbs ext) (metaFormVerbs base)

def FreshExtension (base ext : DeclData) : Prop :=
  freshExtensionBool base ext = true

def readProgram (d : Decl) : SourceProgram → Option TermList
  | [] => some []
  | line :: rest =>
    if isBlankOrCommentLine line then
      readProgram d rest
    else if isFragmentLineBool line then
      match recognizeLine d line, readProgram d rest with
      | some t, some ts => some (t :: ts)
      | _, _ => none
    else
      none

def joinLiterals : List String → String
  | [] => ""
  | [x] => "\"" ++ x ++ "\""
  | x :: xs => "\"" ++ x ++ "\" | " ++ joinLiterals xs

def renderMetaLine : MetaLine → String
  | MetaLine.isClass cname lits => "is : " ++ cname ++ " = " ++ joinLiterals lits
  | MetaLine.termUnary ctor c1 => "term : " ++ ctor ++ " : " ++ c1
  | MetaLine.termBinary ctor c1 c2 => "term : " ++ ctor ++ " : " ++ c1 ++ " " ++ c2
  | MetaLine.formUnary verb slot => "form : " ++ verb ++ " : <" ++ slot ++ ">"
  | MetaLine.doesUnary ctor slot => "does : " ++ ctor ++ " <" ++ slot ++ ">"
  | MetaLine.formBinary verb s1 sep s2 =>
    "form : " ++ verb ++ " : <" ++ s1 ++ ">" ++ sep ++ "<" ++ s2 ++ ">"
  | MetaLine.doesBinary ctor s1 s2 => "does : " ++ ctor ++ " <" ++ s1 ++ "> <" ++ s2 ++ ">"

def renderMetaLines : List MetaLine → String
  | [] => ""
  | m :: ms => renderMetaLine m ++ "\n" ++ renderMetaLines ms

def renderTypifySource (langName : String) (entries : DeclData) : String :=
  "#lang typify\ndeclare : " ++ langName ++ "\nversion : 1\n" ++
    renderMetaLines entries ++ "end!\n"

def witnessBaseData : DeclData :=
  [ MetaLine.isClass "person" ["ada", "ben"],
    MetaLine.isClass "peer" ["ada", "ben"],
    MetaLine.termUnary "member" "person",
    MetaLine.formUnary "member" "person",
    MetaLine.doesUnary "member" "person" ]

def witnessKnowsData : DeclData :=
  [ MetaLine.termBinary "knows" "person" "peer",
    MetaLine.formBinary "knows" "person" " = " "peer",
    MetaLine.doesBinary "knows" "person" "peer" ]

def witnessKnowsLine : SourceLine :=
  "knows : ada = ben"

def witnessKnowsTerm : FactTerm :=
  FactTerm.mk "knows" ["ada", "ben"]

def ExtensionLaw : Prop :=
  (∀ (base ext : DeclData) (dBase dExt : Decl) (prog : SourceProgram) (terms : TermList),
    compileMeta base = Option.some dBase →
    compileMeta (appendDeclData base ext) = Option.some dExt →
    FreshExtension base ext →
    readProgram dBase prog = Option.some terms →
    readProgram dExt prog = Option.some terms) ∧
  (∃ (base ext : DeclData) (dBase dExt : Decl) (line : SourceLine),
    compileMeta base = Option.some dBase ∧
    compileMeta (appendDeclData base ext) = Option.some dExt ∧
    FreshExtension base ext ∧
    recognizeLine dBase line = Option.none ∧
    recognizeLine dExt line = Option.some witnessKnowsTerm)

end TypifyMetalogic
