import TypifyMetalogic.ExtensionStatement
import TypifyMetalogic.FormalityProof

namespace TypifyMetalogic

theorem parseMetaClauses_append :
    ∀ (base ext : List MetaLine)
      (cs1 : List ClassDecl) (ts1 : List TermDecl) (fs1 : List FormDecl)
      (cs2 : List ClassDecl) (ts2 : List TermDecl) (fs2 : List FormDecl),
      parseMetaClauses base = some (cs1, ts1, fs1) →
      parseMetaClauses ext = some (cs2, ts2, fs2) →
      parseMetaClauses (List.append base ext) = some (cs1 ++ cs2, ts1 ++ ts2, fs1 ++ fs2)
  | [], ext, cs1, ts1, fs1, cs2, ts2, fs2, hb, he => by
    simp [parseMetaClauses] at hb
    rcases hb with ⟨rfl, rfl, rfl⟩
    simpa using he
  | [MetaLine.formUnary _ _], _, _, _, _, _, _, _, hb, _ => by
    simp [parseMetaClauses] at hb
  | [MetaLine.formBinary _ _ _ _], _, _, _, _, _, _, _, hb, _ => by
    simp [parseMetaClauses] at hb
  | MetaLine.doesUnary _ _ :: _, _, _, _, _, _, _, _, hb, _ => by
    simp [parseMetaClauses] at hb
  | MetaLine.doesBinary _ _ _ :: _, _, _, _, _, _, _, _, hb, _ => by
    simp [parseMetaClauses] at hb
  | MetaLine.isClass cname lits :: rest, ext, cs1, ts1, fs1, cs2, ts2, fs2, hb, he => by
    unfold parseMetaClauses at hb
    cases hr : parseMetaClauses rest with
    | none =>
      rw [hr] at hb
      contradiction
    | some tr =>
      rw [hr] at hb
      rcases tr with ⟨c, t, f⟩
      simp at hb
      rcases hb with ⟨rfl, rfl, rfl⟩
      have ih := parseMetaClauses_append rest ext c t f cs2 ts2 fs2 hr he
      rw [List.append_eq] at ih
      simp [parseMetaClauses, ih]
  | MetaLine.termUnary ctor c1 :: rest, ext, cs1, ts1, fs1, cs2, ts2, fs2, hb, he => by
    unfold parseMetaClauses at hb
    cases hr : parseMetaClauses rest with
    | none =>
      rw [hr] at hb
      contradiction
    | some tr =>
      rw [hr] at hb
      rcases tr with ⟨c, t, f⟩
      simp at hb
      rcases hb with ⟨rfl, rfl, rfl⟩
      have ih := parseMetaClauses_append rest ext c t f cs2 ts2 fs2 hr he
      rw [List.append_eq] at ih
      simp [parseMetaClauses, ih]
  | MetaLine.termBinary ctor c1 c2 :: rest, ext, cs1, ts1, fs1, cs2, ts2, fs2, hb, he => by
    unfold parseMetaClauses at hb
    cases hr : parseMetaClauses rest with
    | none =>
      rw [hr] at hb
      contradiction
    | some tr =>
      rw [hr] at hb
      rcases tr with ⟨c, t, f⟩
      simp at hb
      rcases hb with ⟨rfl, rfl, rfl⟩
      have ih := parseMetaClauses_append rest ext c t f cs2 ts2 fs2 hr he
      rw [List.append_eq] at ih
      simp [parseMetaClauses, ih]
  | MetaLine.formUnary v s :: m2 :: rest, ext, cs1, ts1, fs1, cs2, ts2, fs2, hb, he => by
    cases m2 with
    | doesUnary ctor dslot =>
      unfold parseMetaClauses at hb
      cases hr : parseMetaClauses rest with
      | none =>
        rw [hr] at hb
        contradiction
      | some tr =>
        rw [hr] at hb
        rcases tr with ⟨c, t, f⟩
        simp at hb
        rcases hb with ⟨rfl, rfl, rfl⟩
        have ih := parseMetaClauses_append rest ext c t f cs2 ts2 fs2 hr he
        rw [List.append_eq] at ih
        simp [parseMetaClauses, ih]
    | _ => simp [parseMetaClauses] at hb
  | MetaLine.formBinary v s1 sep s2 :: m2 :: rest, ext, cs1, ts1, fs1, cs2, ts2, fs2, hb, he => by
    cases m2 with
    | doesBinary ctor d1 d2 =>
      unfold parseMetaClauses at hb
      cases hr : parseMetaClauses rest with
      | none =>
        rw [hr] at hb
        contradiction
      | some tr =>
        rw [hr] at hb
        rcases tr with ⟨c, t, f⟩
        simp at hb
        rcases hb with ⟨rfl, rfl, rfl⟩
        have ih := parseMetaClauses_append rest ext c t f cs2 ts2 fs2 hr he
        rw [List.append_eq] at ih
        simp [parseMetaClauses, ih]
    | _ => simp [parseMetaClauses] at hb

theorem validCaptures_mono {d1 d2 : Decl}
    (hsub : ∀ c, c ∈ d1.classes → c ∈ d2.classes) :
    ∀ {bindings : List (String × String)},
      ValidCaptures d1 bindings → ValidCaptures d2 bindings
  | _, ValidCaptures.nil => ValidCaptures.nil
  | _, ValidCaptures.cons slot tok cls rest hcm hcn htm hv =>
    ValidCaptures.cons slot tok cls rest (hsub cls hcm) hcn htm (validCaptures_mono hsub hv)

theorem buildsFactTerm_mono {d1 d2 : Decl}
    (hsub : ∀ td, td ∈ d1.terms → td ∈ d2.terms)
    {form : FormDecl} {bindings : List (String × String)} {t : FactTerm} :
    BuildsFactTerm d1 form bindings t → BuildsFactTerm d2 form bindings t
  | BuildsFactTerm.unary verb slot ctor doesSlot v1 htm hb =>
    BuildsFactTerm.unary verb slot ctor doesSlot v1 (hsub _ htm) hb
  | BuildsFactTerm.binary verb s1 sep s2 ctor d1s d2s v1 v2 htm hb1 hb2 =>
    BuildsFactTerm.binary verb s1 sep s2 ctor d1s d2s v1 v2 (hsub _ htm) hb1 hb2

theorem derivesLine_mono {d1 d2 : Decl}
    (hc : ∀ c, c ∈ d1.classes → c ∈ d2.classes)
    (ht : ∀ td, td ∈ d1.terms → td ∈ d2.terms)
    (hf : ∀ fd, fd ∈ d1.forms → fd ∈ d2.forms)
    {line : SourceLine} {t : FactTerm} :
    DerivesLine d1 line t → DerivesLine d2 line t
  | DerivesLine.intro verb rhs form bindings hsh hfm hfv hcap hv hb =>
    DerivesLine.intro verb rhs form bindings hsh (hf form hfm) hfv hcap
      (validCaptures_mono hc hv) (buildsFactTerm_mono ht hb)

theorem readProgram_mono {d1 d2 : Decl}
    (hwf1 : WellFormedDecl d1)
    (hwf2 : WellFormedDecl d2)
    (hc : ∀ c, c ∈ d1.classes → c ∈ d2.classes)
    (ht : ∀ td, td ∈ d1.terms → td ∈ d2.terms)
    (hf : ∀ fd, fd ∈ d1.forms → fd ∈ d2.forms) :
    ∀ (prog : SourceProgram) (terms : TermList),
      readProgram d1 prog = some terms →
      readProgram d2 prog = some terms
  | [], terms, h => h
  | line :: rest, terms, h => by
    dsimp [readProgram] at h ⊢
    by_cases hbc : isBlankOrCommentLine line = true
    · rw [hbc] at h ⊢
      simp at h ⊢
      exact readProgram_mono hwf1 hwf2 hc ht hf rest terms h
    · have hbcf : isBlankOrCommentLine line = false := Bool.eq_false_iff.mpr hbc
      rw [hbcf] at h ⊢
      by_cases hfl : isFragmentLineBool line = true
      · rw [hfl] at h ⊢
        simp at h ⊢
        cases hr1 : recognizeLine d1 line with
        | none =>
          rw [hr1] at h
          contradiction
        | some t =>
          cases hp1 : readProgram d1 rest with
          | none =>
            rw [hr1, hp1] at h
            contradiction
          | some ts =>
            rw [hr1, hp1] at h
            injection h with hEq
            subst hEq
            have hd1 : DerivesLine d1 line t :=
              (recognizeLine_iff_derives d1 line hwf1 hfl t).mp hr1
            have hd2 : DerivesLine d2 line t :=
              derivesLine_mono hc ht hf hd1
            have hr2 : recognizeLine d2 line = some t :=
              (recognizeLine_iff_derives d2 line hwf2 hfl t).mpr hd2
            have hp2 : readProgram d2 rest = some ts :=
              readProgram_mono hwf1 hwf2 hc ht hf rest ts hp1
            rw [hr2, hp2]
      · have hflf : isFragmentLineBool line = false := Bool.eq_false_iff.mpr hfl
        rw [hflf] at h
        contradiction

theorem extension_law : ExtensionLaw := by
  constructor
  · intro base ext dBase dExt prog terms hcb hce hfresh hprog
    unfold compileMeta at hcb hce
    cases hpb : parseMetaClauses base with
    | none =>
      rw [hpb] at hcb
      contradiction
    | some tr1 =>
      rw [hpb] at hcb
      rcases tr1 with ⟨cs1, ts1, fs1⟩
      simp at hcb
      rcases hcb with ⟨hwf1, rfl⟩
      unfold FreshExtension freshExtensionBool at hfresh
      rcases Bool.and_eq_true_iff.mp hfresh with ⟨hf1, _⟩
      rcases Bool.and_eq_true_iff.mp hf1 with ⟨hf2, _⟩
      rcases Bool.and_eq_true_iff.mp hf2 with ⟨hextSome, _⟩
      cases hpe : parseMetaClauses ext with
      | none =>
        rw [hpe] at hextSome
        contradiction
      | some tr2 =>
        rcases tr2 with ⟨cs2, ts2, fs2⟩
        have happ := parseMetaClauses_append base ext cs1 ts1 fs1 cs2 ts2 fs2 hpb hpe
        unfold appendDeclData at hce
        rw [happ] at hce
        simp at hce
        rcases hce with ⟨hwf2, rfl⟩
        exact readProgram_mono hwf1 hwf2
          (fun c hm => List.mem_append_left cs2 hm)
          (fun td hm => List.mem_append_left ts2 hm)
          (fun fd hm => List.mem_append_left fs2 hm)
          prog terms hprog
  · refine ⟨witnessBaseData, witnessKnowsData, witnessBaseDecl, witnessExtDecl, witnessKnowsLine,
      by decide, by decide, by unfold FreshExtension; decide, by decide, by decide⟩

end TypifyMetalogic
