import TypifyMetalogic.OntologyStatement
import TypifyMetalogic.FormalityProof

namespace TypifyMetalogic

def assertionFactTerm : Assertion → FactTerm
  | Assertion.unary x => FactTerm.mk "member" [x]
  | Assertion.binary x y => FactTerm.mk "knows" [x, y]

theorem isAdmissibleName_cases (x : IndName) (h : isAdmissibleName x = true) :
    x = "ada" ∨ x = "ben" := by
  unfold isAdmissibleName nameAda nameBen at h
  rcases Bool.or_eq_true_iff.mp h with ha | hb
  · exact Or.inl (beq_iff_eq.mp ha)
  · exact Or.inr (beq_iff_eq.mp hb)

theorem encodeAssertion_not_blank (a : Assertion) (ha : isAdmissibleAssertion a = true) :
    isBlankOrCommentLine (encodeAssertion a) = false := by
  cases a with
  | unary x =>
    rcases isAdmissibleName_cases x ha with rfl | rfl <;> decide
  | binary x y =>
    rcases Bool.and_eq_true_iff.mp ha with ⟨hx, hy⟩
    rcases isAdmissibleName_cases x hx with rfl | rfl <;>
    rcases isAdmissibleName_cases y hy with rfl | rfl <;>
    decide

theorem encodeAssertion_fragment (a : Assertion) (ha : isAdmissibleAssertion a = true) :
    isFragmentLineBool (encodeAssertion a) = true := by
  cases a with
  | unary x =>
    rcases isAdmissibleName_cases x ha with rfl | rfl <;> decide
  | binary x y =>
    rcases Bool.and_eq_true_iff.mp ha with ⟨hx, hy⟩
    rcases isAdmissibleName_cases x hx with rfl | rfl <;>
    rcases isAdmissibleName_cases y hy with rfl | rfl <;>
    decide

theorem recognize_encodeAssertion (a : Assertion) (ha : isAdmissibleAssertion a = true) :
    recognizeLine ontologyDecl (encodeAssertion a) = some (assertionFactTerm a) := by
  cases a with
  | unary x =>
    rcases isAdmissibleName_cases x ha with rfl | rfl <;> decide
  | binary x y =>
    rcases Bool.and_eq_true_iff.mp ha with ⟨hx, hy⟩
    rcases isAdmissibleName_cases x hx with rfl | rfl <;>
    rcases isAdmissibleName_cases y hy with rfl | rfl <;>
    decide

theorem decodeTerm_assertionFactTerm (a : Assertion) :
    decodeTerm (assertionFactTerm a) = some a := by
  cases a <;> rfl

theorem readProgram_encodeAssertions :
    ∀ (assertions : List Assertion),
      assertions.all isAdmissibleAssertion = true →
      readProgram ontologyDecl (encodeAssertions assertions) =
        some (assertions.map assertionFactTerm)
  | [], _ => rfl
  | a :: rest, hall => by
    simp [List.all] at hall
    rcases hall with ⟨ha, hrest⟩
    have hrestAll : rest.all isAdmissibleAssertion = true := List.all_eq_true.mpr hrest
    have hnb := encodeAssertion_not_blank a ha
    have hfl := encodeAssertion_fragment a ha
    have hrec := recognize_encodeAssertion a ha
    have ih := readProgram_encodeAssertions rest hrestAll
    unfold encodeAssertions at ih ⊢
    simp [List.map, readProgram, hnb, hfl, hrec, ih]
    rfl

theorem decodeTerms_map_assertionFactTerm :
    ∀ (assertions : List Assertion),
      decodeTerms (assertions.map assertionFactTerm) = some assertions
  | [] => rfl
  | a :: rest => by
    simp [List.map, decodeTerms, decodeTerm_assertionFactTerm a,
      decodeTerms_map_assertionFactTerm rest]
    rfl

theorem holdsAssertion_iff_holdsFactTerm
    (D : Type) (val : IndName → D) (P : D → Prop) (R : D → D → Prop) (a : Assertion) :
    HoldsAssertion D val P R a ↔ HoldsFactTerm D val P R (assertionFactTerm a) := by
  cases a with
  | unary x =>
    simp [HoldsAssertion, assertionFactTerm, HoldsFactTerm]
  | binary x y =>
    simp [HoldsAssertion, assertionFactTerm, HoldsFactTerm]

theorem satisfiesSource_iff_termList
    (D : Type) (val : IndName → D) (P : D → Prop) (R : D → D → Prop) :
    ∀ (assertions : List Assertion),
      SatisfiesSource D val P R assertions ↔
      SatisfiesTermList D val P R (assertions.map assertionFactTerm)
  | [] => Iff.rfl
  | a :: rest => by
    simp [SatisfiesSource, SatisfiesTermList,
      holdsAssertion_iff_holdsFactTerm D val P R a,
      satisfiesSource_iff_termList D val P R rest]

theorem ontology_law : OntologyLaw := by
  refine ⟨by unfold WellFormedDecl; decide, ?_, ?_, ?_⟩
  · intro assertions hadm
    unfold AdmissibleAssertions admissibleAssertionsBool at hadm
    refine ⟨assertions.map assertionFactTerm, ?_, ?_⟩
    · exact readProgram_encodeAssertions assertions hadm
    · exact decodeTerms_map_assertionFactTerm assertions
  · intro D _hdom val P R assertions hadm
    unfold AdmissibleAssertions admissibleAssertionsBool at hadm
    have hread := readProgram_encodeAssertions assertions hadm
    rw [hread]
    exact satisfiesSource_iff_termList D val P R assertions
  · refine ⟨Bool, ⟨true⟩, (fun s => s == nameAda), (fun _ => True),
      (fun u v => u = true ∧ v = false), ?_, ?_, ?_, ?_⟩
    · exact ⟨rfl, rfl⟩
    · intro h
      exact Bool.noConfusion h.1
    · have hread : readProgram ontologyDecl (encodeAssertions (singleBinary nameAda nameBen)) =
          some [FactTerm.mk "knows" [nameAda, nameBen]] := rfl
      rw [hread]
      exact ⟨⟨rfl, ⟨rfl, rfl⟩⟩, trivial⟩
    · have hread : readProgram ontologyDecl (encodeAssertions (singleBinary nameBen nameAda)) =
          some [FactTerm.mk "knows" [nameBen, nameAda]] := rfl
      rw [hread]
      intro h
      exact Bool.noConfusion h.1.2.1

end TypifyMetalogic
