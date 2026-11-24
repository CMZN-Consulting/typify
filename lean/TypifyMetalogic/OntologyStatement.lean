import TypifyMetalogic.ExtensionStatement

namespace TypifyMetalogic

inductive Assertion where
  | unary (subject : IndName)
  | binary (subject : IndName) (object : IndName)
  deriving DecidableEq, Repr

def AssertionList : Type := List Assertion

def nameAda : IndName := "ada"

def nameBen : IndName := "ben"

def ontologyDecl : Decl :=
  witnessExtDecl

def isAdmissibleName (x : IndName) : Bool :=
  (x == nameAda) || (x == nameBen)

def isAdmissibleAssertion : Assertion → Bool
  | Assertion.unary x => isAdmissibleName x
  | Assertion.binary x y => isAdmissibleName x && isAdmissibleName y

def admissibleAssertionsBool (assertions : AssertionList) : Bool :=
  assertions.all isAdmissibleAssertion

def AdmissibleAssertions (assertions : AssertionList) : Prop :=
  admissibleAssertionsBool assertions = true

def encodeAssertion : Assertion → SourceLine
  | Assertion.unary x => String.append "member : " x
  | Assertion.binary x y => String.append "knows : " (String.append x (String.append " = " y))

def encodeAssertions (assertions : AssertionList) : SourceProgram :=
  assertions.map encodeAssertion

def decodeTerm : FactTerm → Option Assertion
  | FactTerm.mk ctor [x] =>
    if ctor == "member" then some (Assertion.unary x) else none
  | FactTerm.mk ctor [x, y] =>
    if ctor == "knows" then some (Assertion.binary x y) else none
  | _ => none

def decodeTerms : TermList → Option AssertionList
  | [] => some []
  | t :: ts =>
    match decodeTerm t, decodeTerms ts with
    | some a, some as => some (a :: as)
    | _, _ => none

def HoldsAssertion (D : Type) (val : IndName → D) (P : D → Prop) (R : D → D → Prop) :
    Assertion → Prop
  | Assertion.unary x => P (val x)
  | Assertion.binary x y => R (val x) (val y)

def SatisfiesSource (D : Type) (val : IndName → D) (P : D → Prop) (R : D → D → Prop) :
    AssertionList → Prop
  | [] => True
  | a :: rest => HoldsAssertion D val P R a ∧ SatisfiesSource D val P R rest

def HoldsFactTerm (D : Type) (val : IndName → D) (P : D → Prop) (R : D → D → Prop) :
    FactTerm → Prop
  | FactTerm.mk ctor [x] => ctor = "member" ∧ P (val x)
  | FactTerm.mk ctor [x, y] => ctor = "knows" ∧ R (val x) (val y)
  | _ => False

def SatisfiesTermList (D : Type) (val : IndName → D) (P : D → Prop) (R : D → D → Prop) :
    TermList → Prop
  | [] => True
  | t :: ts => HoldsFactTerm D val P R t ∧ SatisfiesTermList D val P R ts

def SatisfiesOutput (D : Type) (val : IndName → D) (P : D → Prop) (R : D → D → Prop) :
    Option TermList → Prop
  | none => False
  | some ts => SatisfiesTermList D val P R ts

def singleBinary (x y : IndName) : AssertionList :=
  [Assertion.binary x y]

def OntologyLaw : Prop :=
  WellFormedDecl ontologyDecl ∧
  (∀ (assertions : AssertionList),
    AdmissibleAssertions assertions →
    ∃ (terms : TermList),
      readProgram ontologyDecl (encodeAssertions assertions) = Option.some terms ∧
      decodeTerms terms = Option.some assertions) ∧
  (∀ (D : Type),
    Nonempty D →
    ∀ (val : IndName → D) (P : D → Prop) (R : D → D → Prop) (assertions : AssertionList),
      AdmissibleAssertions assertions →
      (SatisfiesSource D val P R assertions ↔
       SatisfiesOutput D val P R (readProgram ontologyDecl (encodeAssertions assertions)))) ∧
  (∃ (D : Type),
    Nonempty D ∧
    ∃ (val : IndName → D) (P : D → Prop) (R : D → D → Prop),
      R (val nameAda) (val nameBen) ∧
      ¬ R (val nameBen) (val nameAda) ∧
      SatisfiesOutput D val P R
        (readProgram ontologyDecl (encodeAssertions (singleBinary nameAda nameBen))) ∧
      ¬ SatisfiesOutput D val P R
        (readProgram ontologyDecl (encodeAssertions (singleBinary nameBen nameAda))))

end TypifyMetalogic
