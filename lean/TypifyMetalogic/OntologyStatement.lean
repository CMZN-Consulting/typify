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

structure Vocabulary where
  subjectClass : String
  objectClass : String
  names : List String
  classes : List String
  relations : List String

def relationSeparator : String := " = "

def isPlainName (s : String) : Bool :=
  match s.toList with
  | [] => false
  | c :: cs =>
    (isAlphaAscii c || isDigitAscii c) &&
    cs.all (fun x => isAlphaAscii x || isDigitAscii x || (x == '_') || (x == '-'))

def isPredicateSymbol (v : Vocabulary) (s : String) : Bool :=
  isNameToken s && isVerbToken s && (s != v.subjectClass) && (s != v.objectClass)

def wellFormedVocabularyBool (v : Vocabulary) : Bool :=
  isNameToken v.subjectClass &&
  isNameToken v.objectClass &&
  (v.subjectClass != v.objectClass) &&
  (v.names != []) &&
  noDupKeys id v.names &&
  v.names.all isPlainName &&
  noDupKeys id (v.classes ++ v.relations) &&
  (v.classes ++ v.relations).all (isPredicateSymbol v)

def WellFormedVocabulary (v : Vocabulary) : Prop :=
  wellFormedVocabularyBool v = true

def vocabularyDecl (v : Vocabulary) : Decl :=
  { classes :=
      [ { name := v.subjectClass, literals := v.names },
        { name := v.objectClass, literals := v.names } ],
    terms :=
      v.classes.map (fun p => TermDecl.mk p [v.subjectClass]) ++
      v.relations.map (fun r => TermDecl.mk r [v.subjectClass, v.objectClass]),
    forms :=
      v.classes.map (fun p => FormDecl.unary p v.subjectClass p v.subjectClass) ++
      v.relations.map (fun r =>
        FormDecl.binary r v.subjectClass relationSeparator v.objectClass r
          v.subjectClass v.objectClass) }

def witnessVocabulary : Vocabulary :=
  { subjectClass := "person", objectClass := "peer",
    names := ["ada", "ben"], classes := ["member"], relations := ["knows"] }

inductive VocabAssertion where
  | unary (cls : String) (subject : String)
  | binary (rel : String) (subject : String) (object : String)
  deriving DecidableEq, Repr

def isAdmissibleIn (v : Vocabulary) : VocabAssertion → Bool
  | VocabAssertion.unary p x => v.classes.contains p && v.names.contains x
  | VocabAssertion.binary r x y =>
    v.relations.contains r && v.names.contains x && v.names.contains y

def AdmissibleIn (v : Vocabulary) (assertions : List VocabAssertion) : Prop :=
  assertions.all (isAdmissibleIn v) = true

def encodeVocabAssertion : VocabAssertion → SourceLine
  | VocabAssertion.unary p x => String.append p (String.append " : " x)
  | VocabAssertion.binary r x y =>
    String.append r (String.append " : " (String.append x (String.append relationSeparator y)))

def encodeVocabAssertions (assertions : List VocabAssertion) : SourceProgram :=
  assertions.map encodeVocabAssertion

def decodeVocabTerm (v : Vocabulary) : FactTerm → Option VocabAssertion
  | FactTerm.mk ctor [x] =>
    if v.classes.contains ctor then some (VocabAssertion.unary ctor x) else none
  | FactTerm.mk ctor [x, y] =>
    if v.relations.contains ctor then some (VocabAssertion.binary ctor x y) else none
  | _ => none

def decodeVocabTerms (v : Vocabulary) : TermList → Option (List VocabAssertion)
  | [] => some []
  | t :: ts =>
    match decodeVocabTerm v t, decodeVocabTerms v ts with
    | some a, some rest => some (a :: rest)
    | _, _ => none

def HoldsVocabAssertion (D : Type) (val : String → D)
    (P : String → D → Prop) (R : String → D → D → Prop) : VocabAssertion → Prop
  | VocabAssertion.unary p x => P p (val x)
  | VocabAssertion.binary r x y => R r (val x) (val y)

def SatisfiesVocabSource (D : Type) (val : String → D)
    (P : String → D → Prop) (R : String → D → D → Prop) : List VocabAssertion → Prop
  | [] => True
  | a :: rest => HoldsVocabAssertion D val P R a ∧ SatisfiesVocabSource D val P R rest

def HoldsVocabTerm (v : Vocabulary) (D : Type) (val : String → D)
    (P : String → D → Prop) (R : String → D → D → Prop) : FactTerm → Prop
  | FactTerm.mk ctor [x] => ctor ∈ v.classes ∧ P ctor (val x)
  | FactTerm.mk ctor [x, y] => ctor ∈ v.relations ∧ R ctor (val x) (val y)
  | _ => False

def SatisfiesVocabTerms (v : Vocabulary) (D : Type) (val : String → D)
    (P : String → D → Prop) (R : String → D → D → Prop) : TermList → Prop
  | [] => True
  | t :: ts => HoldsVocabTerm v D val P R t ∧ SatisfiesVocabTerms v D val P R ts

def SatisfiesVocabOutput (v : Vocabulary) (D : Type) (val : String → D)
    (P : String → D → Prop) (R : String → D → D → Prop) : Option TermList → Prop
  | none => False
  | some ts => SatisfiesVocabTerms v D val P R ts

def OntologyVocabularyLaw : Prop :=
  (WellFormedVocabulary witnessVocabulary ∧ vocabularyDecl witnessVocabulary = witnessExtDecl) ∧
  ∀ (v : Vocabulary),
    WellFormedVocabulary v →
    (WellFormedDecl (vocabularyDecl v) ∧
     (∀ (assertions : List VocabAssertion),
       AdmissibleIn v assertions →
       ∃ (terms : TermList),
         readProgram (vocabularyDecl v) (encodeVocabAssertions assertions) = Option.some terms ∧
         decodeVocabTerms v terms = Option.some assertions) ∧
     (∀ (prog : SourceProgram) (terms : TermList),
       readProgram (vocabularyDecl v) prog = Option.some terms →
       ∃ (assertions : List VocabAssertion),
         AdmissibleIn v assertions ∧ decodeVocabTerms v terms = Option.some assertions) ∧
     (∀ (D : Type),
       Nonempty D →
       ∀ (val : String → D) (P : String → D → Prop) (R : String → D → D → Prop)
         (assertions : List VocabAssertion),
         AdmissibleIn v assertions →
         (SatisfiesVocabSource D val P R assertions ↔
          SatisfiesVocabOutput v D val P R
            (readProgram (vocabularyDecl v) (encodeVocabAssertions assertions)))) ∧
     (∀ (r x y : String),
       r ∈ v.relations → x ∈ v.names → y ∈ v.names → x ≠ y →
       ∃ (D : Type),
         Nonempty D ∧
         ∃ (val : String → D) (P : String → D → Prop) (R : String → D → D → Prop),
           SatisfiesVocabOutput v D val P R
             (readProgram (vocabularyDecl v)
               (encodeVocabAssertions [VocabAssertion.binary r x y])) ∧
           ¬ SatisfiesVocabOutput v D val P R
             (readProgram (vocabularyDecl v)
               (encodeVocabAssertions [VocabAssertion.binary r y x]))))

end TypifyMetalogic
