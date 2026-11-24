namespace TypifyMetalogic

def SourceLine : Type := String
  deriving DecidableEq, Repr

def IndName : Type := String
  deriving DecidableEq, Repr

structure ClassDecl where
  name : String
  literals : List String
  deriving DecidableEq, Repr

structure TermDecl where
  ctor : String
  paramClasses : List String
  deriving DecidableEq, Repr

inductive FormDecl where
  | unary (verb : String) (slot : String) (ctor : String) (doesSlot : String)
  | binary (verb : String) (slot1 : String) (sep : String) (slot2 : String)
      (ctor : String) (doesSlot1 : String) (doesSlot2 : String)
  deriving DecidableEq, Repr

def FormDecl.verb : FormDecl → String
  | FormDecl.unary v _ _ _ => v
  | FormDecl.binary v _ _ _ _ _ _ => v

structure Decl where
  classes : List ClassDecl
  terms : List TermDecl
  forms : List FormDecl
  deriving DecidableEq, Repr

structure FactTerm where
  ctor : String
  args : List String
  deriving DecidableEq, Repr

def isBlank (c : Char) : Bool :=
  (c == ' ') || (c == '\t')

def isPrintableAscii (c : Char) : Bool :=
  decide (32 ≤ c.toNat) && decide (c.toNat < 127)

def isLegalChar (c : Char) : Bool :=
  (c == '\t') || isPrintableAscii c

def trimLeftChars (cs : List Char) : List Char :=
  cs.dropWhile isBlank

def trimChars (cs : List Char) : List Char :=
  (trimLeftChars (trimLeftChars cs).reverse).reverse

def trimStr (s : String) : String :=
  String.ofList (trimChars s.toList)

def isCommentChars : List Char → Bool
  | '#' :: _ => true
  | _ => false

def isQuestionChars : List Char → Bool
  | '?' :: _ => true
  | _ => false

def startsNonBlank : List Char → Bool
  | [] => false
  | c :: _ => !isBlank c

def isFragmentLineBool (s : SourceLine) : Bool :=
  let cs := String.toList s
  let trimmed := trimChars cs
  cs.all isLegalChar && (trimmed != []) && (!isCommentChars trimmed) && (!isQuestionChars trimmed)

def FragmentLine (s : SourceLine) : Prop :=
  isFragmentLineBool s = true

def isBlankOrCommentLine (s : SourceLine) : Bool :=
  let cs := String.toList s
  let trimmed := trimChars cs
  cs.all isLegalChar && ((trimmed == []) || isCommentChars trimmed)

def reservedNames : List String :=
  ["letter", "canonical-path", "relative-path", "Typify",
   "name", "constructor", "verb", "value", "text", "number",
   "ordering", "order-mode", "prefix", "production", "term",
   "pattern", "law", "width", "limit", "scope-mode", "multiplicity",
   "parent", "join", "split", "ancestors", "suffix", "equal",
   "concat", "inherited"]

def isAlphaAscii (c : Char) : Bool :=
  (decide ('A'.toNat ≤ c.toNat) && decide (c.toNat ≤ 'Z'.toNat)) ||
  (decide ('a'.toNat ≤ c.toNat) && decide (c.toNat ≤ 'z'.toNat))

def isDigitAscii (c : Char) : Bool :=
  decide ('0'.toNat ≤ c.toNat) && decide (c.toNat ≤ '9'.toNat)

def isNameToken (s : String) : Bool :=
  match s.toList with
  | [] => false
  | c :: cs =>
    isAlphaAscii c &&
    cs.all (fun x => isAlphaAscii x || isDigitAscii x || (x == '_') || (x == '-')) &&
    !(reservedNames.contains s)

def isVerbToken (s : String) : Bool :=
  match s.toList with
  | [] => false
  | c :: cs =>
    (isAlphaAscii c || isDigitAscii c) &&
    cs.all (fun x => isAlphaAscii x || isDigitAscii x || (x == '-')) &&
    (s != "?")

def isLegalToken (s : String) : Bool :=
  let cs := s.toList
  (cs != []) &&
  cs.all (fun c => isPrintableAscii c && (c != '"') && (c != '\\')) &&
  startsNonBlank cs &&
  startsNonBlank cs.reverse &&
  (!isCommentChars cs)

def isLegalSeparator (sep : String) : Bool :=
  let cs := sep.toList
  (cs != []) &&
  cs.all (fun c => isPrintableAscii c && (c != '<') && (c != '>'))

def noDupKeys {α : Type} (key : α → String) : List α → Bool
  | [] => true
  | x :: xs => (!(xs.any (fun y => key y == key x))) && noDupKeys key xs

def findByKey {α : Type} (key : α → String) : List α → String → Option α
  | [], _ => none
  | x :: xs, k => if key x == k then some x else findByKey key xs k

def lookupClass (d : Decl) (cname : String) : Option ClassDecl :=
  findByKey ClassDecl.name d.classes cname

def lookupTerm (d : Decl) (ctor : String) : Option TermDecl :=
  findByKey TermDecl.ctor d.terms ctor

def lookupForm (d : Decl) (verb : String) : Option FormDecl :=
  findByKey FormDecl.verb d.forms verb

def startsWithChars : List Char → List Char → Bool
  | [], _ => true
  | _ :: _, [] => false
  | p :: ps, x :: xs => (p == x) && startsWithChars ps xs

def noEarlySepMatch (sep : List Char) : List Char → Bool
  | [] => true
  | c :: u' => (!startsWithChars sep (c :: (u' ++ sep))) && noEarlySepMatch sep u'

def splitFirstColon : List Char → Option (List Char × List Char)
  | [] => none
  | c :: cs =>
    if c == ':' then
      some ([], cs)
    else
      match splitFirstColon cs with
      | some (lhs, rhs) => some (c :: lhs, rhs)
      | none => none

def splitColonLine (s : SourceLine) : Option (String × String) :=
  if isFragmentLineBool s then
    match splitFirstColon (trimChars (String.toList s)) with
    | some (lhsRaw, rhsRaw) =>
      let verb := String.ofList (trimChars lhsRaw)
      let rhs := String.ofList (trimChars rhsRaw)
      if verb == "" then none else some (verb, rhs)
    | none => none
  else
    none

def splitAtFirstSep (sep : List Char) : List Char → Option (List Char × List Char)
  | [] => none
  | x :: xs =>
    if startsWithChars sep (x :: xs) then
      some ([], (x :: xs).drop sep.length)
    else
      match splitAtFirstSep sep xs with
      | some (lhs, rhs) => some (x :: lhs, rhs)
      | none => none

def wellFormedClass (cls : ClassDecl) : Bool :=
  isNameToken cls.name &&
  (cls.literals != []) &&
  noDupKeys id cls.literals &&
  cls.literals.all isLegalToken

def wellFormedTerm (d : Decl) (td : TermDecl) : Bool :=
  isNameToken td.ctor &&
  ((td.paramClasses.length == 1) || (td.paramClasses.length == 2)) &&
  td.paramClasses.all (fun cname => (lookupClass d cname).isSome)

def wellFormedForm (d : Decl) : FormDecl → Bool
  | FormDecl.unary verb slot ctor doesSlot =>
    isVerbToken verb &&
    isNameToken slot &&
    isNameToken ctor &&
    (doesSlot == slot) &&
    (lookupClass d slot).isSome &&
    (lookupTerm d ctor == some (TermDecl.mk ctor [doesSlot]))
  | FormDecl.binary verb slot1 sep slot2 ctor does1 does2 =>
    isVerbToken verb &&
    isNameToken slot1 &&
    isNameToken slot2 &&
    (slot1 != slot2) &&
    isLegalSeparator sep &&
    isNameToken ctor &&
    (((does1 == slot1) && (does2 == slot2)) || ((does1 == slot2) && (does2 == slot1))) &&
    (lookupTerm d ctor == some (TermDecl.mk ctor [does1, does2])) &&
    match lookupClass d slot1, lookupClass d slot2 with
    | some cls1, some _ =>
      cls1.literals.all (fun u => noEarlySepMatch sep.toList u.toList)
    | _, _ => false

def wellFormedDeclBool (d : Decl) : Bool :=
  noDupKeys ClassDecl.name d.classes &&
  noDupKeys TermDecl.ctor d.terms &&
  noDupKeys FormDecl.verb d.forms &&
  d.classes.all wellFormedClass &&
  d.terms.all (wellFormedTerm d) &&
  d.forms.all (wellFormedForm d)

def WellFormedDecl (d : Decl) : Prop :=
  wellFormedDeclBool d = true

def lookupBinding (bindings : List (String × String)) (slot : String) : Option String :=
  match findByKey Prod.fst bindings slot with
  | some (_, v) => some v
  | none => none

def recognizeLine (d : Decl) (line : SourceLine) : Option FactTerm :=
  match splitColonLine line with
  | none => none
  | some (verb, rhs) =>
    match lookupForm d verb with
    | none => none
    | some (FormDecl.unary _ slot ctor doesSlot) =>
      match lookupClass d slot, lookupTerm d ctor with
      | some cls, some td =>
        if cls.literals.contains rhs && (td.paramClasses == [doesSlot]) then
          match lookupBinding [(slot, rhs)] doesSlot with
          | some v1 => some (FactTerm.mk ctor [v1])
          | none => none
        else
          none
      | _, _ => none
    | some (FormDecl.binary _ slot1 sep slot2 ctor does1 does2) =>
      match splitAtFirstSep sep.toList rhs.toList with
      | none => none
      | some (lhsChars, rhsChars) =>
        let tok1 := String.ofList lhsChars
        let tok2 := String.ofList rhsChars
        match lookupClass d slot1, lookupClass d slot2, lookupTerm d ctor with
        | some cls1, some cls2, some td =>
          if cls1.literals.contains tok1 &&
             cls2.literals.contains tok2 &&
             (td.paramClasses == [does1, does2]) then
            let bindings := [(slot1, tok1), (slot2, tok2)]
            match lookupBinding bindings does1, lookupBinding bindings does2 with
            | some v1, some v2 => some (FactTerm.mk ctor [v1, v2])
            | _, _ => none
          else
            none
        | _, _, _ => none

def acceptsLine (d : Decl) (line : SourceLine) : Bool :=
  (recognizeLine d line).isSome

def AcceptsLine (d : Decl) (line : SourceLine) : Prop :=
  acceptsLine d line = true

inductive HasColonShape : SourceLine → String → String → Prop where
  | intro (line : SourceLine) (lhsRaw rhsRaw : List Char) (verb rhs : String) :
      FragmentLine line →
      trimChars (String.toList line) = lhsRaw ++ (':' :: rhsRaw) →
      (∀ c, c ∈ lhsRaw → c ≠ ':') →
      verb = String.ofList (trimChars lhsRaw) →
      verb ≠ "" →
      rhs = String.ofList (trimChars rhsRaw) →
      HasColonShape line verb rhs

inductive CapturesForm : FormDecl → String → List (String × String) → Prop where
  | unary (verb slot ctor doesSlot rhs : String) :
      CapturesForm (FormDecl.unary verb slot ctor doesSlot) rhs [(slot, rhs)]
  | binary (verb slot1 sep slot2 ctor does1 does2 tok1 tok2 : String) :
      CapturesForm (FormDecl.binary verb slot1 sep slot2 ctor does1 does2)
        (tok1 ++ sep ++ tok2) [(slot1, tok1), (slot2, tok2)]

inductive ValidCaptures (d : Decl) : List (String × String) → Prop where
  | nil : ValidCaptures d []
  | cons (slot tok : String) (cls : ClassDecl) (rest : List (String × String)) :
      cls ∈ d.classes →
      cls.name = slot →
      tok ∈ cls.literals →
      ValidCaptures d rest →
      ValidCaptures d ((slot, tok) :: rest)

inductive BuildsFactTerm (d : Decl) : FormDecl → List (String × String) → FactTerm → Prop where
  | unary (verb slot ctor doesSlot v1 : String) :
      TermDecl.mk ctor [doesSlot] ∈ d.terms →
      (doesSlot, v1) ∈ bindings →
      BuildsFactTerm d (FormDecl.unary verb slot ctor doesSlot) bindings (FactTerm.mk ctor [v1])
  | binary (verb slot1 sep slot2 ctor does1 does2 v1 v2 : String) :
      TermDecl.mk ctor [does1, does2] ∈ d.terms →
      (does1, v1) ∈ bindings →
      (does2, v2) ∈ bindings →
      BuildsFactTerm d (FormDecl.binary verb slot1 sep slot2 ctor does1 does2)
        bindings (FactTerm.mk ctor [v1, v2])

inductive DerivesLine (d : Decl) (line : SourceLine) (t : FactTerm) : Prop where
  | intro (verb rhs : String) (form : FormDecl) (bindings : List (String × String)) :
      HasColonShape line verb rhs →
      form ∈ d.forms →
      form.verb = verb →
      CapturesForm form rhs bindings →
      ValidCaptures d bindings →
      BuildsFactTerm d form bindings t →
      DerivesLine d line t

def witnessBaseDecl : Decl :=
  { classes :=
      [ { name := "person", literals := ["ada", "ben"] },
        { name := "peer", literals := ["ada", "ben"] } ],
    terms :=
      [ { ctor := "member", paramClasses := ["person"] } ],
    forms :=
      [ FormDecl.unary "member" "person" "member" "person" ] }

def witnessExtDecl : Decl :=
  { classes :=
      [ { name := "person", literals := ["ada", "ben"] },
        { name := "peer", literals := ["ada", "ben"] } ],
    terms :=
      [ { ctor := "member", paramClasses := ["person"] },
        { ctor := "knows", paramClasses := ["person", "peer"] } ],
    forms :=
      [ FormDecl.unary "member" "person" "member" "person",
        FormDecl.binary "knows" "person" " = " "peer" "knows" "person" "peer" ] }

def FormalityLaw : Prop :=
  WellFormedDecl witnessBaseDecl ∧
  ∀ (d : Decl) (line : SourceLine),
    WellFormedDecl d →
    FragmentLine line →
    ((∀ (t : FactTerm), recognizeLine d line = Option.some t ↔ DerivesLine d line t) ∧
     (∀ (t₁ t₂ : FactTerm), DerivesLine d line t₁ → DerivesLine d line t₂ → t₁ = t₂) ∧
     (AcceptsLine d line ↔ ∃ (t : FactTerm), DerivesLine d line t))

end TypifyMetalogic
