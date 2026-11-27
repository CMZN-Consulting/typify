import TypifyMetalogic.OntologyStatement
import TypifyMetalogic.FormalityProof

namespace TypifyMetalogic

def isWordChar (c : Char) : Bool :=
  isAlphaAscii c || isDigitAscii c || (c == '_') || (c == '-')

theorem wordChar_bounds (c : Char) (h : isWordChar c = true) :
    (65 ≤ c.toNat ∧ c.toNat ≤ 90) ∨
    (97 ≤ c.toNat ∧ c.toNat ≤ 122) ∨
    (48 ≤ c.toNat ∧ c.toNat ≤ 57) ∨
    c.toNat = 95 ∨
    c.toNat = 45 := by
  unfold isWordChar isAlphaAscii isDigitAscii at h
  rcases Bool.or_eq_true_iff.mp h with h123 | hdash
  · rcases Bool.or_eq_true_iff.mp h123 with h12 | hus
    · rcases Bool.or_eq_true_iff.mp h12 with hal | hdig
      · rcases Bool.or_eq_true_iff.mp hal with hup | hlo
        · rcases Bool.and_eq_true_iff.mp hup with ⟨h1, h2⟩
          have ha1 : 'A'.toNat = 65 := rfl
          have ha2 : 'Z'.toNat = 90 := rfl
          have b1 := of_decide_eq_true h1
          have b2 := of_decide_eq_true h2
          rw [ha1] at b1
          rw [ha2] at b2
          exact Or.inl ⟨b1, b2⟩
        · rcases Bool.and_eq_true_iff.mp hlo with ⟨h1, h2⟩
          have ha1 : 'a'.toNat = 97 := rfl
          have ha2 : 'z'.toNat = 122 := rfl
          have b1 := of_decide_eq_true h1
          have b2 := of_decide_eq_true h2
          rw [ha1] at b1
          rw [ha2] at b2
          exact Or.inr (Or.inl ⟨b1, b2⟩)
      · rcases Bool.and_eq_true_iff.mp hdig with ⟨h1, h2⟩
        have hd1 : '0'.toNat = 48 := rfl
        have hd2 : '9'.toNat = 57 := rfl
        have b1 := of_decide_eq_true h1
        have b2 := of_decide_eq_true h2
        rw [hd1] at b1
        rw [hd2] at b2
        exact Or.inr (Or.inr (Or.inl ⟨b1, b2⟩))
    · have heq : c = '_' := beq_iff_eq.mp hus
      subst heq
      exact Or.inr (Or.inr (Or.inr (Or.inl rfl)))
  · have heq : c = '-' := beq_iff_eq.mp hdash
    subst heq
    exact Or.inr (Or.inr (Or.inr (Or.inr rfl)))

theorem wordChar_printable (c : Char) (h : isWordChar c = true) :
    isPrintableAscii c = true := by
  unfold isPrintableAscii
  have hb := wordChar_bounds c h
  have h1 : 32 ≤ c.toNat := by omega
  have h2 : c.toNat < 127 := by omega
  exact Bool.and_eq_true_iff.mpr ⟨decide_eq_true h1, decide_eq_true h2⟩

theorem wordChar_legal (c : Char) (h : isWordChar c = true) :
    isLegalChar c = true := by
  unfold isLegalChar
  exact Bool.or_eq_true_iff.mpr (Or.inr (wordChar_printable c h))

theorem wordChar_ne_space (c : Char) (h : isWordChar c = true) : c ≠ ' ' := by
  intro heq
  subst heq
  have hb := wordChar_bounds ' ' h
  have hn : (' ').toNat = 32 := rfl
  rw [hn] at hb
  omega

theorem wordChar_ne_tab (c : Char) (h : isWordChar c = true) : c ≠ '\t' := by
  intro heq
  subst heq
  have hb := wordChar_bounds '\t' h
  have hn : ('\t').toNat = 9 := rfl
  rw [hn] at hb
  omega

theorem wordChar_ne_colon (c : Char) (h : isWordChar c = true) : c ≠ ':' := by
  intro heq
  subst heq
  have hb := wordChar_bounds ':' h
  have hn : (':').toNat = 58 := rfl
  rw [hn] at hb
  omega

theorem wordChar_ne_dquote (c : Char) (h : isWordChar c = true) : c ≠ '"' := by
  intro heq
  subst heq
  have hb := wordChar_bounds '"' h
  have hn : ('"').toNat = 34 := rfl
  rw [hn] at hb
  omega

theorem wordChar_ne_bslash (c : Char) (h : isWordChar c = true) : c ≠ '\\' := by
  intro heq
  subst heq
  have hb := wordChar_bounds '\\' h
  have hn : ('\\').toNat = 92 := rfl
  rw [hn] at hb
  omega

theorem wordChar_ne_hash (c : Char) (h : isWordChar c = true) : c ≠ '#' := by
  intro heq
  subst heq
  have hb := wordChar_bounds '#' h
  have hn : ('#').toNat = 35 := rfl
  rw [hn] at hb
  omega

theorem wordChar_ne_qmark (c : Char) (h : isWordChar c = true) : c ≠ '?' := by
  intro heq
  subst heq
  have hb := wordChar_bounds '?' h
  have hn : ('?').toNat = 63 := rfl
  rw [hn] at hb
  omega

theorem wordChar_not_blank (c : Char) (h : isWordChar c = true) :
    isBlank c = false := by
  unfold isBlank
  have hs : (c == ' ') = false := beq_eq_false_iff_ne.mpr (wordChar_ne_space c h)
  have ht : (c == '\t') = false := beq_eq_false_iff_ne.mpr (wordChar_ne_tab c h)
  rw [hs, ht]
  rfl

theorem all_mono {α : Type} (p q : α → Bool) (hpq : ∀ x, p x = true → q x = true) :
    ∀ (xs : List α), xs.all p = true → xs.all q = true
  | [], _ => rfl
  | x :: xs, h => by
    unfold List.all at h ⊢
    rcases Bool.and_eq_true_iff.mp h with ⟨hx, hxs⟩
    exact Bool.and_eq_true_iff.mpr ⟨hpq x hx, all_mono p q hpq xs hxs⟩

theorem all_append_true {α : Type} (p : α → Bool) :
    ∀ (xs ys : List α), xs.all p = true → ys.all p = true → (xs ++ ys).all p = true
  | [], _, _, hy => hy
  | x :: xs, ys, hx, hy => by
    unfold List.all at hx
    rcases Bool.and_eq_true_iff.mp hx with ⟨hpx, hxs⟩
    simp [hpx, all_append_true p xs ys hxs hy]

theorem all_map_true {α β : Type} (f : α → β) (q : β → Bool) :
    ∀ (xs : List α), (∀ x, x ∈ xs → q (f x) = true) → (xs.map f).all q = true
  | [], _ => rfl
  | x :: xs, h => by
    have hx : q (f x) = true := h x (List.Mem.head xs)
    have hxs : (xs.map f).all q = true :=
      all_map_true f q xs (fun y hy => h y (List.Mem.tail x hy))
    simp [List.map, hx, hxs]

theorem isPlainName_wordChars (s : String) (h : isPlainName s = true) :
    ∃ c cs, s.toList = c :: cs ∧ isWordChar c = true ∧ cs.all isWordChar = true := by
  unfold isPlainName at h
  cases hcs : s.toList with
  | nil =>
    rw [hcs] at h
    contradiction
  | cons c cs =>
    rw [hcs] at h
    dsimp only at h
    rcases Bool.and_eq_true_iff.mp h with ⟨hc, hrest⟩
    have hcw : isWordChar c = true := by
      unfold isWordChar
      exact Bool.or_eq_true_iff.mpr (Or.inl (Bool.or_eq_true_iff.mpr (Or.inl hc)))
    exact ⟨c, cs, rfl, hcw, hrest⟩

theorem isVerbToken_wordChars (s : String) (h : isVerbToken s = true) :
    ∃ c cs, s.toList = c :: cs ∧ isWordChar c = true ∧ cs.all isWordChar = true := by
  unfold isVerbToken at h
  cases hcs : s.toList with
  | nil =>
    rw [hcs] at h
    contradiction
  | cons c cs =>
    rw [hcs] at h
    dsimp only at h
    rcases Bool.and_eq_true_iff.mp h with ⟨h1, _⟩
    rcases Bool.and_eq_true_iff.mp h1 with ⟨hc, hrest⟩
    have hcw : isWordChar c = true := by
      unfold isWordChar
      exact Bool.or_eq_true_iff.mpr (Or.inl (Bool.or_eq_true_iff.mpr (Or.inl hc)))
    have hrestw : cs.all isWordChar = true := by
      refine all_mono (fun x => isAlphaAscii x || isDigitAscii x || (x == '-')) isWordChar ?_ cs hrest
      intro x hx
      unfold isWordChar
      rcases Bool.or_eq_true_iff.mp hx with had | hdash
      · exact Bool.or_eq_true_iff.mpr (Or.inl (Bool.or_eq_true_iff.mpr (Or.inl had)))
      · exact Bool.or_eq_true_iff.mpr (Or.inr hdash)
    exact ⟨c, cs, rfl, hcw, hrestw⟩

theorem reverse_starts_wordChar :
    ∀ (c : Char) (cs : List Char),
      isWordChar c = true →
      cs.all isWordChar = true →
      ∃ d ds, (c :: cs).reverse = d :: ds ∧ isWordChar d = true
  | c, [], hc, _ => ⟨c, [], rfl, hc⟩
  | c, c2 :: rest, _, hrest => by
    unfold List.all at hrest
    rcases Bool.and_eq_true_iff.mp hrest with ⟨hc2, htail⟩
    rcases reverse_starts_wordChar c2 rest hc2 htail with ⟨d, ds, hrev, hd⟩
    refine ⟨d, ds ++ [c], ?_, hd⟩
    rw [List.reverse_cons, hrev]
    rfl

theorem trimLeftChars_wordChar (c : Char) (cs : List Char) (hc : isWordChar c = true) :
    trimLeftChars (c :: cs) = c :: cs := by
  unfold trimLeftChars
  simp [List.dropWhile, wordChar_not_blank c hc]

theorem trimChars_framed (zs : List Char) (c d : Char) (ztail zrtail : List Char)
    (hz : zs = c :: ztail) (hr : zs.reverse = d :: zrtail)
    (hc : isWordChar c = true) (hd : isWordChar d = true) :
    trimChars zs = zs := by
  unfold trimChars
  have hl : trimLeftChars zs = zs := by
    rw [hz]
    exact trimLeftChars_wordChar c ztail hc
  rw [hl, hr, trimLeftChars_wordChar d zrtail hd, ← hr, List.reverse_reverse]

theorem trimChars_append_space (c : Char) (cs : List Char)
    (hc : isWordChar c = true) (hcs : cs.all isWordChar = true) :
    trimChars ((c :: cs) ++ [' ']) = c :: cs := by
  rcases reverse_starts_wordChar c cs hc hcs with ⟨d, ds, hrev, hd⟩
  unfold trimChars
  have hcons : (c :: cs) ++ [' '] = c :: (cs ++ [' ']) := rfl
  rw [hcons, trimLeftChars_wordChar c (cs ++ [' ']) hc]
  have hr : (c :: (cs ++ [' '])).reverse = ' ' :: (d :: ds) := by
    have h1 : ((c :: cs) ++ [' ']).reverse = [' '].reverse ++ (c :: cs).reverse :=
      @List.reverse_append _ (c :: cs) [' ']
    rw [hrev] at h1
    exact h1
  rw [hr]
  unfold trimLeftChars
  have hsp : isBlank ' ' = true := rfl
  simp [List.dropWhile, hsp, wordChar_not_blank d hd, ← hrev, List.reverse_reverse]

theorem trimChars_space_prepend (ys : List Char) (c d : Char) (ytail yrtail : List Char)
    (hy : ys = c :: ytail) (hyr : ys.reverse = d :: yrtail)
    (hc : isWordChar c = true) (hd : isWordChar d = true) :
    trimChars (' ' :: ys) = ys := by
  unfold trimChars
  have hl : trimLeftChars (' ' :: ys) = ys := by
    rw [hy]
    unfold trimLeftChars
    have hsp : isBlank ' ' = true := rfl
    simp [List.dropWhile, hsp, wordChar_not_blank c hc]
  rw [hl, hyr, trimLeftChars_wordChar d yrtail hd, ← hyr, List.reverse_reverse]

theorem noEarlySepMatch_wordChars :
    ∀ (cs : List Char),
      cs.all isWordChar = true →
      noEarlySepMatch relationSeparator.toList cs = true
  | [], _ => rfl
  | c :: u', h => by
    unfold List.all at h
    rcases Bool.and_eq_true_iff.mp h with ⟨hc, hu⟩
    have hsep : relationSeparator.toList = [' ', '=', ' '] := by decide
    rw [hsep]
    unfold noEarlySepMatch startsWithChars
    have hne : (' ' == c) = false := beq_eq_false_iff_ne.mpr (Ne.symm (wordChar_ne_space c hc))
    rw [hne]
    simp only [Bool.false_and, Bool.not_false, Bool.true_and]
    rw [← hsep]
    exact noEarlySepMatch_wordChars u' hu

theorem isPlainName_isLegalToken (s : String) (h : isPlainName s = true) :
    isLegalToken s = true := by
  rcases isPlainName_wordChars s h with ⟨c, cs, hcs, hc, hrest⟩
  rcases reverse_starts_wordChar c cs hc hrest with ⟨d, ds, hrev, hd⟩
  unfold isLegalToken
  rw [hcs]
  dsimp only
  have hne : ((c :: cs) != []) = true := rfl
  have hallw : (c :: cs).all isWordChar = true := by
    simp [List.all, hc, hrest]
  have halltok : (c :: cs).all (fun x => isPrintableAscii x && (x != '"') && (x != '\\')) = true := by
    refine all_mono isWordChar (fun x => isPrintableAscii x && (x != '"') && (x != '\\')) ?_ (c :: cs) hallw
    intro x hx
    have hp := wordChar_printable x hx
    have hq : (x != '"') = true := by
      change !(x == '"') = true
      rw [beq_eq_false_iff_ne.mpr (wordChar_ne_dquote x hx)]
      rfl
    have hb : (x != '\\') = true := by
      change !(x == '\\') = true
      rw [beq_eq_false_iff_ne.mpr (wordChar_ne_bslash x hx)]
      rfl
    rw [hp, hq, hb]
    rfl
  have hnb1 : startsNonBlank (c :: cs) = true := by
    unfold startsNonBlank
    dsimp only
    rw [wordChar_not_blank c hc]
    rfl
  have hnb2 : startsNonBlank (c :: cs).reverse = true := by
    rw [hrev]
    unfold startsNonBlank
    dsimp only
    rw [wordChar_not_blank d hd]
    rfl
  have hnc : (!isCommentChars (c :: cs)) = true := by
    cases hcc : isCommentChars (c :: cs) with
    | false => rfl
    | true =>
      unfold isCommentChars at hcc
      split at hcc
      · rename_i heq
        injection heq with hcHash
        exact absurd hcHash (wordChar_ne_hash c hc)
      · contradiction
  rw [hne, halltok, hnb1, hnb2, hnc]
  rfl

theorem encode_colon_line (vStr rStr : String)
    (vc : Char) (vcs : List Char) (hvList : vStr.toList = vc :: vcs)
    (hvc : isWordChar vc = true) (hvcs : vcs.all isWordChar = true)
    (rc rd : Char) (rtail rrtail : List Char)
    (hrList : rStr.toList = rc :: rtail) (hrRev : rStr.toList.reverse = rd :: rrtail)
    (hrc : isWordChar rc = true) (hrd : isWordChar rd = true)
    (hrLegal : rStr.toList.all isLegalChar = true) :
    let line := String.append vStr (String.append " : " rStr)
    isBlankOrCommentLine line = false ∧
    isFragmentLineBool line = true ∧
    splitColonLine line = some (vStr, rStr) := by
  intro line
  have hmid : (" : " : String).toList = [' ', ':', ' '] := by decide
  have hlineList : line.toList = (vc :: vcs) ++ [' ', ':', ' '] ++ rStr.toList := by
    have hdef : line = vStr ++ (" : " ++ rStr) := rfl
    rw [hdef, String.toList_append, String.toList_append, hvList, hmid, List.append_assoc]
  have hvLegal : (vc :: vcs).all isLegalChar = true := by
    have hvw : (vc :: vcs).all isWordChar = true := by simp [List.all, hvc, hvcs]
    exact all_mono isWordChar isLegalChar wordChar_legal (vc :: vcs) hvw
  have hmidLegal : ([' ', ':', ' '] : List Char).all isLegalChar = true := by decide
  have hallLegal : line.toList.all isLegalChar = true := by
    rw [hlineList]
    exact all_append_true isLegalChar ((vc :: vcs) ++ [' ', ':', ' ']) rStr.toList
      (all_append_true isLegalChar (vc :: vcs) [' ', ':', ' '] hvLegal hmidLegal) hrLegal
  have hlineCons : line.toList = vc :: (vcs ++ [' ', ':', ' '] ++ rStr.toList) := by
    rw [hlineList]
    simp [List.append_assoc]
  have hlineRev : line.toList.reverse = rd :: (rrtail ++ ([' ', ':', ' '].reverse ++ (vc :: vcs).reverse)) := by
    rw [hlineList, List.reverse_append, List.reverse_append, hrRev]
    simp
  have htrimLine : trimChars line.toList = line.toList :=
    trimChars_framed line.toList vc rd (vcs ++ [' ', ':', ' '] ++ rStr.toList)
      (rrtail ++ ([' ', ':', ' '].reverse ++ (vc :: vcs).reverse))
      hlineCons hlineRev hvc hrd
  have hnotComm : isCommentChars line.toList = false := by
    rw [hlineCons]
    cases hcc : isCommentChars (vc :: (vcs ++ [' ', ':', ' '] ++ rStr.toList)) with
    | false => rfl
    | true =>
      unfold isCommentChars at hcc
      split at hcc
      · rename_i heq
        injection heq with hcHash
        exact absurd hcHash (wordChar_ne_hash vc hvc)
      · contradiction
  have hnotQuest : isQuestionChars line.toList = false := by
    rw [hlineCons]
    cases hqc : isQuestionChars (vc :: (vcs ++ [' ', ':', ' '] ++ rStr.toList)) with
    | false => rfl
    | true =>
      unfold isQuestionChars at hqc
      split at hqc
      · rename_i heq
        injection heq with hcQ
        exact absurd hcQ (wordChar_ne_qmark vc hvc)
      · contradiction
  have hnonEmpty : (line.toList != []) = true := by
    rw [hlineCons]
    rfl
  have hblankEq : (line.toList == []) = false := by
    rw [hlineCons]
    rfl
  have hbc : isBlankOrCommentLine line = false := by
    unfold isBlankOrCommentLine
    dsimp only
    rw [htrimLine, hallLegal, hblankEq, hnotComm]
    rfl
  have hfl : isFragmentLineBool line = true := by
    unfold isFragmentLineBool
    dsimp only
    rw [htrimLine, hallLegal, hnonEmpty, hnotComm, hnotQuest]
    rfl
  refine ⟨hbc, hfl, ?_⟩
  unfold splitColonLine
  rw [hfl]
  dsimp only [ite_true]
  rw [htrimLine]
  have hsplitShape : line.toList = ((vc :: vcs) ++ [' ']) ++ (':' :: (' ' :: rStr.toList)) := by
    rw [hlineList]
    simp [List.append_assoc]
  have hnoColon : ∀ ch, ch ∈ (vc :: vcs) ++ [' '] → ch ≠ ':' := by
    intro ch hmem
    rcases List.mem_append.mp hmem with hleft | hright
    · have hvw : (vc :: vcs).all isWordChar = true := by simp [List.all, hvc, hvcs]
      exact wordChar_ne_colon ch (all_mem_prop isWordChar (vc :: vcs) ch hvw hleft)
    · have hsp : ch = ' ' := List.mem_singleton.mp hright
      subst hsp
      decide
  rw [hsplitShape, splitFirstColon_complete ((vc :: vcs) ++ [' ']) (' ' :: rStr.toList) hnoColon]
  dsimp only
  rw [trimChars_append_space vc vcs hvc hvcs,
      trimChars_space_prepend rStr.toList rc rd rtail rrtail hrList hrRev hrc hrd]
  rw [← hvList, String.ofList_toList, String.ofList_toList]
  have hvNeEmpty : (vStr == "") = false := by
    apply beq_eq_false_iff_ne.mpr
    intro heq
    subst heq
    simp at hvList
  rw [hvNeEmpty]
  rfl

theorem noDupKeys_eq_map {α : Type} (key : α → String) :
    ∀ (xs : List α), noDupKeys key xs = noDupKeys id (xs.map key)
  | [] => rfl
  | x :: xs => by
    unfold noDupKeys
    simp [List.map, noDupKeys_eq_map key xs]
    rfl

theorem map_id_fn {α : Type} (f : String → α) (key : α → String)
    (hkey : ∀ s, key (f s) = s) :
    ∀ (xs : List String), (xs.map f).map key = xs
  | [] => rfl
  | x :: xs => by
    simp [List.map, hkey x, map_id_fn f key hkey xs]

theorem map_ctor_vocabularyDecl (v : Vocabulary) :
    (vocabularyDecl v).terms.map TermDecl.ctor = v.classes ++ v.relations := by
  unfold vocabularyDecl
  dsimp only
  rw [List.map_append]
  rw [map_id_fn (fun p => TermDecl.mk p [v.subjectClass]) TermDecl.ctor (fun _ => rfl) v.classes]
  rw [map_id_fn (fun r => TermDecl.mk r [v.subjectClass, v.objectClass]) TermDecl.ctor (fun _ => rfl) v.relations]

theorem map_verb_vocabularyDecl (v : Vocabulary) :
    (vocabularyDecl v).forms.map FormDecl.verb = v.classes ++ v.relations := by
  unfold vocabularyDecl
  dsimp only
  rw [List.map_append]
  rw [map_id_fn (fun p => FormDecl.unary p v.subjectClass p v.subjectClass) FormDecl.verb (fun _ => rfl) v.classes]
  rw [map_id_fn (fun r => FormDecl.binary r v.subjectClass relationSeparator v.objectClass r v.subjectClass v.objectClass) FormDecl.verb (fun _ => rfl) v.relations]

theorem lookupClass_subject (v : Vocabulary) :
    lookupClass (vocabularyDecl v) v.subjectClass =
      some { name := v.subjectClass, literals := v.names } := by
  unfold lookupClass vocabularyDecl findByKey
  simp

theorem lookupClass_object (v : Vocabulary) (hne : (v.subjectClass != v.objectClass) = true) :
    lookupClass (vocabularyDecl v) v.objectClass =
      some { name := v.objectClass, literals := v.names } := by
  have hbeq : (v.subjectClass == v.objectClass) = false := by
    change !(v.subjectClass == v.objectClass) = true at hne
    cases h : (v.subjectClass == v.objectClass) with
    | false => rfl
    | true => rw [h] at hne; contradiction
  unfold lookupClass vocabularyDecl findByKey
  dsimp only
  rw [hbeq]
  simp [findByKey]

theorem vocabularyDecl_wellFormed (v : Vocabulary) (hwf : WellFormedVocabulary v) :
    WellFormedDecl (vocabularyDecl v) := by
  unfold WellFormedVocabulary wellFormedVocabularyBool at hwf
  rcases Bool.and_eq_true_iff.mp hwf with ⟨h1, hpredAll⟩
  rcases Bool.and_eq_true_iff.mp h1 with ⟨h2, hndSyms⟩
  rcases Bool.and_eq_true_iff.mp h2 with ⟨h3, hplainNames⟩
  rcases Bool.and_eq_true_iff.mp h3 with ⟨h4, hndNames⟩
  rcases Bool.and_eq_true_iff.mp h4 with ⟨h5, hnamesNe⟩
  rcases Bool.and_eq_true_iff.mp h5 with ⟨h6, hclsNe⟩
  rcases Bool.and_eq_true_iff.mp h6 with ⟨hsubjName, hobjName⟩
  have hndClasses : noDupKeys ClassDecl.name (vocabularyDecl v).classes = true := by
    unfold vocabularyDecl noDupKeys List.any
    dsimp only
    have hne : v.subjectClass ≠ v.objectClass := by
      change !(v.subjectClass == v.objectClass) = true at hclsNe
      cases hb : (v.subjectClass == v.objectClass) with
      | false => exact beq_eq_false_iff_ne.mp hb
      | true => rw [hb] at hclsNe; contradiction
    have hrev : (v.objectClass == v.subjectClass) = false :=
      beq_eq_false_iff_ne.mpr (Ne.symm hne)
    rw [hrev]
    rfl
  have hndTerms : noDupKeys TermDecl.ctor (vocabularyDecl v).terms = true := by
    rw [noDupKeys_eq_map TermDecl.ctor (vocabularyDecl v).terms, map_ctor_vocabularyDecl v]
    exact hndSyms
  have hndForms : noDupKeys FormDecl.verb (vocabularyDecl v).forms = true := by
    rw [noDupKeys_eq_map FormDecl.verb (vocabularyDecl v).forms, map_verb_vocabularyDecl v]
    exact hndSyms
  have hlegalNames : v.names.all isLegalToken = true :=
    all_mono isPlainName isLegalToken isPlainName_isLegalToken v.names hplainNames
  have hwfSubj : wellFormedClass { name := v.subjectClass, literals := v.names } = true := by
    unfold wellFormedClass
    dsimp only
    rw [hsubjName, hnamesNe, hndNames, hlegalNames]
    rfl
  have hwfObj : wellFormedClass { name := v.objectClass, literals := v.names } = true := by
    unfold wellFormedClass
    dsimp only
    rw [hobjName, hnamesNe, hndNames, hlegalNames]
    rfl
  have hclassesAll : (vocabularyDecl v).classes.all wellFormedClass = true := by
    unfold vocabularyDecl
    simp [List.all, hwfSubj, hwfObj]
  have hlcSubj := lookupClass_subject v
  have hlcObj := lookupClass_object v hclsNe
  have htermsUnary :
      (v.classes.map (fun p => TermDecl.mk p [v.subjectClass])).all (wellFormedTerm (vocabularyDecl v)) = true := by
    refine all_map_true (fun p => TermDecl.mk p [v.subjectClass]) (wellFormedTerm (vocabularyDecl v)) v.classes ?_
    intro p hp
    have hps : isPredicateSymbol v p = true :=
      all_mem_prop (isPredicateSymbol v) (v.classes ++ v.relations) p hpredAll
        (List.mem_append_left v.relations hp)
    unfold isPredicateSymbol at hps
    rcases Bool.and_eq_true_iff.mp hps with ⟨hps1, _⟩
    rcases Bool.and_eq_true_iff.mp hps1 with ⟨hps2, _⟩
    rcases Bool.and_eq_true_iff.mp hps2 with ⟨hpName, _⟩
    unfold wellFormedTerm
    simp [List.all, hpName, hlcSubj]
  have htermsBinary :
      (v.relations.map (fun r => TermDecl.mk r [v.subjectClass, v.objectClass])).all (wellFormedTerm (vocabularyDecl v)) = true := by
    refine all_map_true (fun r => TermDecl.mk r [v.subjectClass, v.objectClass]) (wellFormedTerm (vocabularyDecl v)) v.relations ?_
    intro r hr
    have hrs : isPredicateSymbol v r = true :=
      all_mem_prop (isPredicateSymbol v) (v.classes ++ v.relations) r hpredAll
        (List.mem_append_right v.classes hr)
    unfold isPredicateSymbol at hrs
    rcases Bool.and_eq_true_iff.mp hrs with ⟨hrs1, _⟩
    rcases Bool.and_eq_true_iff.mp hrs1 with ⟨hrs2, _⟩
    rcases Bool.and_eq_true_iff.mp hrs2 with ⟨hrName, _⟩
    unfold wellFormedTerm
    simp [List.all, hrName, hlcSubj, hlcObj]
  have htermsAll : (vocabularyDecl v).terms.all (wellFormedTerm (vocabularyDecl v)) = true := by
    change (v.classes.map (fun p => TermDecl.mk p [v.subjectClass]) ++
      v.relations.map (fun r => TermDecl.mk r [v.subjectClass, v.objectClass])).all
        (wellFormedTerm (vocabularyDecl v)) = true
    exact all_append_true (wellFormedTerm (vocabularyDecl v)) _ _ htermsUnary htermsBinary
  have hnoEarlyNames : v.names.all (fun u => noEarlySepMatch relationSeparator.toList u.toList) = true := by
    refine all_mono isPlainName (fun u => noEarlySepMatch relationSeparator.toList u.toList) ?_ v.names hplainNames
    intro u hu
    rcases isPlainName_wordChars u hu with ⟨c, cs, hcs, hc, hrest⟩
    have hw : u.toList.all isWordChar = true := by
      rw [hcs]
      simp [List.all, hc, hrest]
    exact noEarlySepMatch_wordChars u.toList hw
  have hformsUnary :
      (v.classes.map (fun p => FormDecl.unary p v.subjectClass p v.subjectClass)).all
        (wellFormedForm (vocabularyDecl v)) = true := by
    refine all_map_true (fun p => FormDecl.unary p v.subjectClass p v.subjectClass)
      (wellFormedForm (vocabularyDecl v)) v.classes ?_
    intro p hp
    have hps : isPredicateSymbol v p = true :=
      all_mem_prop (isPredicateSymbol v) (v.classes ++ v.relations) p hpredAll
        (List.mem_append_left v.relations hp)
    unfold isPredicateSymbol at hps
    rcases Bool.and_eq_true_iff.mp hps with ⟨hps1, _⟩
    rcases Bool.and_eq_true_iff.mp hps1 with ⟨hps2, _⟩
    rcases Bool.and_eq_true_iff.mp hps2 with ⟨hpName, hpVerb⟩
    have htdMem : TermDecl.mk p [v.subjectClass] ∈ (vocabularyDecl v).terms := by
      unfold vocabularyDecl
      exact List.mem_append_left _ (List.mem_map.mpr ⟨p, hp, rfl⟩)
    have hlt : lookupTerm (vocabularyDecl v) p = some (TermDecl.mk p [v.subjectClass]) :=
      mem_findByKey_of_noDup TermDecl.ctor (vocabularyDecl v).terms
        (TermDecl.mk p [v.subjectClass]) hndTerms htdMem
    unfold wellFormedForm
    simp [hpVerb, hsubjName, hpName, hlcSubj, hlt]
  have hformsBinary :
      (v.relations.map (fun r =>
        FormDecl.binary r v.subjectClass relationSeparator v.objectClass r v.subjectClass v.objectClass)).all
        (wellFormedForm (vocabularyDecl v)) = true := by
    refine all_map_true
      (fun r => FormDecl.binary r v.subjectClass relationSeparator v.objectClass r v.subjectClass v.objectClass)
      (wellFormedForm (vocabularyDecl v)) v.relations ?_
    intro r hr
    have hrs : isPredicateSymbol v r = true :=
      all_mem_prop (isPredicateSymbol v) (v.classes ++ v.relations) r hpredAll
        (List.mem_append_right v.classes hr)
    unfold isPredicateSymbol at hrs
    rcases Bool.and_eq_true_iff.mp hrs with ⟨hrs1, _⟩
    rcases Bool.and_eq_true_iff.mp hrs1 with ⟨hrs2, _⟩
    rcases Bool.and_eq_true_iff.mp hrs2 with ⟨hrName, hrVerb⟩
    have htdMem : TermDecl.mk r [v.subjectClass, v.objectClass] ∈ (vocabularyDecl v).terms := by
      unfold vocabularyDecl
      exact List.mem_append_right _ (List.mem_map.mpr ⟨r, hr, rfl⟩)
    have hlt : lookupTerm (vocabularyDecl v) r = some (TermDecl.mk r [v.subjectClass, v.objectClass]) :=
      mem_findByKey_of_noDup TermDecl.ctor (vocabularyDecl v).terms
        (TermDecl.mk r [v.subjectClass, v.objectClass]) hndTerms htdMem
    have hsepLeg : isLegalSeparator relationSeparator = true := by decide
    unfold wellFormedForm
    simp [hrVerb, hsubjName, hobjName, hclsNe, hsepLeg, hrName, hlt, hlcSubj, hlcObj, hnoEarlyNames]
  have hformsAll : (vocabularyDecl v).forms.all (wellFormedForm (vocabularyDecl v)) = true := by
    change (v.classes.map (fun p => FormDecl.unary p v.subjectClass p v.subjectClass) ++
      v.relations.map (fun r =>
        FormDecl.binary r v.subjectClass relationSeparator v.objectClass r v.subjectClass v.objectClass)).all
        (wellFormedForm (vocabularyDecl v)) = true
    exact all_append_true (wellFormedForm (vocabularyDecl v)) _ _ hformsUnary hformsBinary
  unfold WellFormedDecl wellFormedDeclBool
  rw [hndClasses, hndTerms, hndForms, hclassesAll, htermsAll, hformsAll]
  rfl

def vocabAssertionFactTerm : VocabAssertion → FactTerm
  | VocabAssertion.unary p x => FactTerm.mk p [x]
  | VocabAssertion.binary r x y => FactTerm.mk r [x, y]

theorem encodeVocabAssertion_step (v : Vocabulary) (hwf : WellFormedVocabulary v)
    (a : VocabAssertion) (ha : isAdmissibleIn v a = true) :
    isBlankOrCommentLine (encodeVocabAssertion a) = false ∧
    isFragmentLineBool (encodeVocabAssertion a) = true ∧
    recognizeLine (vocabularyDecl v) (encodeVocabAssertion a) = some (vocabAssertionFactTerm a) ∧
    decodeVocabTerm v (vocabAssertionFactTerm a) = some a := by
  have hwfDecl := vocabularyDecl_wellFormed v hwf
  unfold WellFormedDecl wellFormedDeclBool at hwfDecl
  rcases Bool.and_eq_true_iff.mp hwfDecl with ⟨hd1, _⟩
  rcases Bool.and_eq_true_iff.mp hd1 with ⟨hd2, _⟩
  rcases Bool.and_eq_true_iff.mp hd2 with ⟨hd3, _⟩
  rcases Bool.and_eq_true_iff.mp hd3 with ⟨hd4, hndForms⟩
  rcases Bool.and_eq_true_iff.mp hd4 with ⟨_, hndTerms⟩
  unfold WellFormedVocabulary wellFormedVocabularyBool at hwf
  rcases Bool.and_eq_true_iff.mp hwf with ⟨hv1, hpredAll⟩
  rcases Bool.and_eq_true_iff.mp hv1 with ⟨hv2, _⟩
  rcases Bool.and_eq_true_iff.mp hv2 with ⟨hv3, hplainNames⟩
  rcases Bool.and_eq_true_iff.mp hv3 with ⟨hv4, _⟩
  rcases Bool.and_eq_true_iff.mp hv4 with ⟨hv5, _⟩
  rcases Bool.and_eq_true_iff.mp hv5 with ⟨_, hclsNe⟩
  have hlcSubj := lookupClass_subject v
  have hlcObj := lookupClass_object v hclsNe
  cases a with
  | unary p x =>
    unfold isAdmissibleIn at ha
    rcases Bool.and_eq_true_iff.mp ha with ⟨hpCont, hxCont⟩
    have hpMem : p ∈ v.classes := (contains_iff_mem_str v.classes p).mp hpCont
    have hxMem : x ∈ v.names := (contains_iff_mem_str v.names x).mp hxCont
    have hps : isPredicateSymbol v p = true :=
      all_mem_prop (isPredicateSymbol v) (v.classes ++ v.relations) p hpredAll
        (List.mem_append_left v.relations hpMem)
    unfold isPredicateSymbol at hps
    rcases Bool.and_eq_true_iff.mp hps with ⟨hps1, _⟩
    rcases Bool.and_eq_true_iff.mp hps1 with ⟨hps2, _⟩
    rcases Bool.and_eq_true_iff.mp hps2 with ⟨_, hpVerb⟩
    have hxPlain : isPlainName x = true := all_mem_prop isPlainName v.names x hplainNames hxMem
    rcases isVerbToken_wordChars p hpVerb with ⟨vc, vcs, hvList, hvc, hvcs⟩
    rcases isPlainName_wordChars x hxPlain with ⟨xc, xcs, hxList, hxc, hxcs⟩
    rcases reverse_starts_wordChar xc xcs hxc hxcs with ⟨xd, xds, hxRev, hxd⟩
    have hxRev' : x.toList.reverse = xd :: xds := by
      rw [hxList]
      exact hxRev
    have hxLegal : x.toList.all isLegalChar = true := by
      have hxw : x.toList.all isWordChar = true := by rw [hxList]; simp [List.all, hxc, hxcs]
      exact all_mono isWordChar isLegalChar wordChar_legal x.toList hxw
    rcases encode_colon_line p x vc vcs hvList hvc hvcs xc xd xcs xds hxList hxRev' hxc hxd hxLegal
      with ⟨hbc, hfl, hsc⟩
    have hformMem : FormDecl.unary p v.subjectClass p v.subjectClass ∈ (vocabularyDecl v).forms := by
      unfold vocabularyDecl
      exact List.mem_append_left _ (List.mem_map.mpr ⟨p, hpMem, rfl⟩)
    have hlf : lookupForm (vocabularyDecl v) p =
        some (FormDecl.unary p v.subjectClass p v.subjectClass) :=
      mem_findByKey_of_noDup FormDecl.verb (vocabularyDecl v).forms
        (FormDecl.unary p v.subjectClass p v.subjectClass) hndForms hformMem
    have htdMem : TermDecl.mk p [v.subjectClass] ∈ (vocabularyDecl v).terms := by
      unfold vocabularyDecl
      exact List.mem_append_left _ (List.mem_map.mpr ⟨p, hpMem, rfl⟩)
    have hlt : lookupTerm (vocabularyDecl v) p = some (TermDecl.mk p [v.subjectClass]) :=
      mem_findByKey_of_noDup TermDecl.ctor (vocabularyDecl v).terms
        (TermDecl.mk p [v.subjectClass]) hndTerms htdMem
    refine ⟨hbc, hfl, ?_, ?_⟩
    · unfold encodeVocabAssertion recognizeLine vocabAssertionFactTerm
      rw [hsc]
      dsimp only
      rw [hlf]
      dsimp only
      rw [hlcSubj, hlt]
      simp [hxMem, lookupBinding_singleton]
    · dsimp only [decodeVocabTerm, vocabAssertionFactTerm]
      rw [hpCont]
      rfl
  | binary r x y =>
    unfold isAdmissibleIn at ha
    rcases Bool.and_eq_true_iff.mp ha with ⟨hrx, hyCont⟩
    rcases Bool.and_eq_true_iff.mp hrx with ⟨hrCont, hxCont⟩
    have hrMem : r ∈ v.relations := (contains_iff_mem_str v.relations r).mp hrCont
    have hxMem : x ∈ v.names := (contains_iff_mem_str v.names x).mp hxCont
    have hyMem : y ∈ v.names := (contains_iff_mem_str v.names y).mp hyCont
    have hrs : isPredicateSymbol v r = true :=
      all_mem_prop (isPredicateSymbol v) (v.classes ++ v.relations) r hpredAll
        (List.mem_append_right v.classes hrMem)
    unfold isPredicateSymbol at hrs
    rcases Bool.and_eq_true_iff.mp hrs with ⟨hrs1, _⟩
    rcases Bool.and_eq_true_iff.mp hrs1 with ⟨hrs2, _⟩
    rcases Bool.and_eq_true_iff.mp hrs2 with ⟨_, hrVerb⟩
    have hxPlain : isPlainName x = true := all_mem_prop isPlainName v.names x hplainNames hxMem
    have hyPlain : isPlainName y = true := all_mem_prop isPlainName v.names y hplainNames hyMem
    rcases isVerbToken_wordChars r hrVerb with ⟨vc, vcs, hvList, hvc, hvcs⟩
    rcases isPlainName_wordChars x hxPlain with ⟨xc, xcs, hxList, hxc, hxcs⟩
    rcases isPlainName_wordChars y hyPlain with ⟨yc, ycs, hyList, hyc, hycs⟩
    rcases reverse_starts_wordChar yc ycs hyc hycs with ⟨yd, yds, hyRev, hyd⟩
    let rStr := String.append x (String.append relationSeparator y)
    have hsepList : relationSeparator.toList = [' ', '=', ' '] := by decide
    have hrStrList : rStr.toList = (xc :: xcs) ++ [' ', '=', ' '] ++ (yc :: ycs) := by
      have hdef : rStr = x ++ (relationSeparator ++ y) := rfl
      rw [hdef, String.toList_append, String.toList_append, hxList, hsepList, hyList, List.append_assoc]
    have hrStrCons : rStr.toList = xc :: (xcs ++ [' ', '=', ' '] ++ (yc :: ycs)) := by
      rw [hrStrList]
      simp [List.append_assoc]
    have hrStrRev : rStr.toList.reverse = yd :: (yds ++ ([' ', '=', ' '].reverse ++ (xc :: xcs).reverse)) := by
      rw [hrStrList, List.reverse_append, List.reverse_append, hyRev]
      simp
    have hxLegal : (xc :: xcs).all isLegalChar = true :=
      all_mono isWordChar isLegalChar wordChar_legal (xc :: xcs) (by simp [List.all, hxc, hxcs])
    have hyLegal : (yc :: ycs).all isLegalChar = true :=
      all_mono isWordChar isLegalChar wordChar_legal (yc :: ycs) (by simp [List.all, hyc, hycs])
    have hsepLegal : ([' ', '=', ' '] : List Char).all isLegalChar = true := by decide
    have hrStrLegal : rStr.toList.all isLegalChar = true := by
      rw [hrStrList]
      exact all_append_true isLegalChar ((xc :: xcs) ++ [' ', '=', ' ']) (yc :: ycs)
        (all_append_true isLegalChar (xc :: xcs) [' ', '=', ' '] hxLegal hsepLegal) hyLegal
    rcases encode_colon_line r rStr vc vcs hvList hvc hvcs xc yd
      (xcs ++ [' ', '=', ' '] ++ (yc :: ycs))
      (yds ++ ([' ', '=', ' '].reverse ++ (xc :: xcs).reverse))
      hrStrCons hrStrRev hxc hyd hrStrLegal
      with ⟨hbc, hfl, hsc⟩
    have hformMem :
        FormDecl.binary r v.subjectClass relationSeparator v.objectClass r v.subjectClass v.objectClass ∈
          (vocabularyDecl v).forms := by
      unfold vocabularyDecl
      exact List.mem_append_right _ (List.mem_map.mpr ⟨r, hrMem, rfl⟩)
    have hlf : lookupForm (vocabularyDecl v) r =
        some (FormDecl.binary r v.subjectClass relationSeparator v.objectClass r v.subjectClass v.objectClass) :=
      mem_findByKey_of_noDup FormDecl.verb (vocabularyDecl v).forms
        (FormDecl.binary r v.subjectClass relationSeparator v.objectClass r v.subjectClass v.objectClass)
        hndForms hformMem
    have htdMem : TermDecl.mk r [v.subjectClass, v.objectClass] ∈ (vocabularyDecl v).terms := by
      unfold vocabularyDecl
      exact List.mem_append_right _ (List.mem_map.mpr ⟨r, hrMem, rfl⟩)
    have hlt : lookupTerm (vocabularyDecl v) r = some (TermDecl.mk r [v.subjectClass, v.objectClass]) :=
      mem_findByKey_of_noDup TermDecl.ctor (vocabularyDecl v).terms
        (TermDecl.mk r [v.subjectClass, v.objectClass]) hndTerms htdMem
    have hsepNe : relationSeparator.toList ≠ [] := by decide
    have hxWord : x.toList.all isWordChar = true := by rw [hxList]; simp [List.all, hxc, hxcs]
    have hnoEarly : noEarlySepMatch relationSeparator.toList x.toList = true :=
      noEarlySepMatch_wordChars x.toList hxWord
    have hrStrSplit : rStr.toList = x.toList ++ relationSeparator.toList ++ y.toList := by
      rw [hrStrList, ← hxList, ← hsepList, ← hyList]
    have hsp : splitAtFirstSep relationSeparator.toList rStr.toList = some (x.toList, y.toList) := by
      rw [hrStrSplit]
      exact splitAtFirstSep_complete relationSeparator.toList x.toList y.toList hsepNe hnoEarly
    have hneSlots : v.subjectClass ≠ v.objectClass := by
      change !(v.subjectClass == v.objectClass) = true at hclsNe
      cases hb : (v.subjectClass == v.objectClass) with
      | false => exact beq_eq_false_iff_ne.mp hb
      | true => rw [hb] at hclsNe; contradiction
    have hb1 : lookupBinding [(v.subjectClass, x), (v.objectClass, y)] v.subjectClass = some x :=
      (lookupBinding_pair_distinct v.subjectClass x v.objectClass y v.subjectClass x hneSlots).mpr
        (List.Mem.head _)
    have hb2 : lookupBinding [(v.subjectClass, x), (v.objectClass, y)] v.objectClass = some y :=
      (lookupBinding_pair_distinct v.subjectClass x v.objectClass y v.objectClass y hneSlots).mpr
        (List.Mem.tail _ (List.Mem.head []))
    refine ⟨hbc, hfl, ?_, ?_⟩
    · unfold encodeVocabAssertion recognizeLine vocabAssertionFactTerm
      change (match splitColonLine (String.append r (String.append " : " rStr)) with
        | none => none
        | some (verb, rhs) => _) = some (FactTerm.mk r [x, y])
      rw [hsc]
      dsimp only
      rw [hlf]
      dsimp only
      rw [hsp]
      dsimp only
      rw [hlcSubj, hlcObj, hlt]
      simp [String.ofList_toList, hxMem, hyMem, hb1, hb2]
    · dsimp only [decodeVocabTerm, vocabAssertionFactTerm]
      rw [hrCont]
      rfl

theorem readProgram_encodeVocabAssertions (v : Vocabulary) (hwf : WellFormedVocabulary v) :
    ∀ (assertions : List VocabAssertion),
      assertions.all (isAdmissibleIn v) = true →
      readProgram (vocabularyDecl v) (encodeVocabAssertions assertions) =
        some (assertions.map vocabAssertionFactTerm)
  | [], _ => rfl
  | a :: rest, hall => by
    unfold List.all at hall
    rcases Bool.and_eq_true_iff.mp hall with ⟨ha, hrest⟩
    rcases encodeVocabAssertion_step v hwf a ha with ⟨hbc, hfl, hrec, _⟩
    have ih := readProgram_encodeVocabAssertions v hwf rest hrest
    unfold encodeVocabAssertions at ih ⊢
    simp [List.map, readProgram, hbc, hfl, hrec, ih]
    rfl

theorem decodeVocabTerms_map_vocabFactTerm (v : Vocabulary) (hwf : WellFormedVocabulary v) :
    ∀ (assertions : List VocabAssertion),
      assertions.all (isAdmissibleIn v) = true →
      decodeVocabTerms v (assertions.map vocabAssertionFactTerm) = some assertions
  | [], _ => rfl
  | a :: rest, hall => by
    unfold List.all at hall
    rcases Bool.and_eq_true_iff.mp hall with ⟨ha, hrest⟩
    rcases encodeVocabAssertion_step v hwf a ha with ⟨_, _, _, hdec⟩
    have ih := decodeVocabTerms_map_vocabFactTerm v hwf rest hrest
    simp [List.map, decodeVocabTerms, hdec, ih]

theorem exactness_line (v : Vocabulary) (hwf : WellFormedVocabulary v)
    (line : SourceLine) (t : FactTerm)
    (hrec : recognizeLine (vocabularyDecl v) line = some t) :
    ∃ (a : VocabAssertion), isAdmissibleIn v a = true ∧ decodeVocabTerm v t = some a := by
  have hwfDecl := vocabularyDecl_wellFormed v hwf
  unfold WellFormedDecl wellFormedDeclBool at hwfDecl
  rcases Bool.and_eq_true_iff.mp hwfDecl with ⟨hd1, _⟩
  rcases Bool.and_eq_true_iff.mp hd1 with ⟨hd2, _⟩
  rcases Bool.and_eq_true_iff.mp hd2 with ⟨hd3, _⟩
  rcases Bool.and_eq_true_iff.mp hd3 with ⟨hd4, _⟩
  rcases Bool.and_eq_true_iff.mp hd4 with ⟨_, hndTerms⟩
  unfold WellFormedVocabulary wellFormedVocabularyBool at hwf
  rcases Bool.and_eq_true_iff.mp hwf with ⟨hv1, _⟩
  rcases Bool.and_eq_true_iff.mp hv1 with ⟨hv2, _⟩
  rcases Bool.and_eq_true_iff.mp hv2 with ⟨hv3, _⟩
  rcases Bool.and_eq_true_iff.mp hv3 with ⟨hv4, _⟩
  rcases Bool.and_eq_true_iff.mp hv4 with ⟨hv5, _⟩
  rcases Bool.and_eq_true_iff.mp hv5 with ⟨_, hclsNe⟩
  have hlcSubj := lookupClass_subject v
  have hlcObj := lookupClass_object v hclsNe
  unfold recognizeLine at hrec
  cases hsc : splitColonLine line with
  | none =>
    rw [hsc] at hrec
    contradiction
  | some vp =>
    rw [hsc] at hrec
    rcases vp with ⟨verb, rhs⟩
    dsimp only at hrec
    cases hlf : lookupForm (vocabularyDecl v) verb with
    | none =>
      rw [hlf] at hrec
      contradiction
    | some form =>
      rw [hlf] at hrec
      have ⟨hformMem, _⟩ := findByKey_mem FormDecl.verb (vocabularyDecl v).forms verb form hlf
      unfold vocabularyDecl at hformMem
      dsimp only at hformMem
      rcases List.mem_append.mp hformMem with hun | hbin
      · rcases List.mem_map.mp hun with ⟨p, hpMem, hpForm⟩
        subst hpForm
        dsimp only at hrec
        have htdMem : TermDecl.mk p [v.subjectClass] ∈ (vocabularyDecl v).terms := by
          unfold vocabularyDecl
          exact List.mem_append_left _ (List.mem_map.mpr ⟨p, hpMem, rfl⟩)
        have hlt : lookupTerm (vocabularyDecl v) p = some (TermDecl.mk p [v.subjectClass]) :=
          mem_findByKey_of_noDup TermDecl.ctor (vocabularyDecl v).terms
            (TermDecl.mk p [v.subjectClass]) hndTerms htdMem
        rw [hlcSubj, hlt] at hrec
        dsimp only at hrec
        by_cases hrhs : (v.names.contains rhs && ([v.subjectClass] == [v.subjectClass])) = true
        · rw [hrhs] at hrec
          simp [lookupBinding_singleton] at hrec
          subst hrec
          rcases Bool.and_eq_true_iff.mp hrhs with ⟨hrhsCont, _⟩
          have hpCont : v.classes.contains p = true := (contains_iff_mem_str v.classes p).mpr hpMem
          refine ⟨VocabAssertion.unary p rhs, ?_, ?_⟩
          · dsimp only [isAdmissibleIn]
            rw [hpCont, hrhsCont]
            rfl
          · dsimp only [decodeVocabTerm]
            rw [hpCont]
            rfl
        · have hrhsf : (v.names.contains rhs && ([v.subjectClass] == [v.subjectClass])) = false :=
            Bool.eq_false_iff.mpr hrhs
          rw [hrhsf] at hrec
          contradiction
      · rcases List.mem_map.mp hbin with ⟨r, hrMem, hrForm⟩
        subst hrForm
        dsimp only at hrec
        cases hsp : splitAtFirstSep relationSeparator.toList rhs.toList with
        | none =>
          rw [hsp] at hrec
          contradiction
        | some lr =>
          rw [hsp] at hrec
          rcases lr with ⟨lhsChars, rhsChars⟩
          dsimp only at hrec
          have htdMem : TermDecl.mk r [v.subjectClass, v.objectClass] ∈ (vocabularyDecl v).terms := by
            unfold vocabularyDecl
            exact List.mem_append_right _ (List.mem_map.mpr ⟨r, hrMem, rfl⟩)
          have hlt : lookupTerm (vocabularyDecl v) r =
              some (TermDecl.mk r [v.subjectClass, v.objectClass]) :=
            mem_findByKey_of_noDup TermDecl.ctor (vocabularyDecl v).terms
              (TermDecl.mk r [v.subjectClass, v.objectClass]) hndTerms htdMem
          rw [hlcSubj, hlcObj, hlt] at hrec
          dsimp only at hrec
          let tok1 := String.ofList lhsChars
          let tok2 := String.ofList rhsChars
          have hneSlots : v.subjectClass ≠ v.objectClass := by
            change !(v.subjectClass == v.objectClass) = true at hclsNe
            cases hb : (v.subjectClass == v.objectClass) with
            | false => exact beq_eq_false_iff_ne.mp hb
            | true => rw [hb] at hclsNe; contradiction
          have hb1 : lookupBinding [(v.subjectClass, tok1), (v.objectClass, tok2)] v.subjectClass = some tok1 :=
            (lookupBinding_pair_distinct v.subjectClass tok1 v.objectClass tok2 v.subjectClass tok1 hneSlots).mpr
              (List.Mem.head _)
          have hb2 : lookupBinding [(v.subjectClass, tok1), (v.objectClass, tok2)] v.objectClass = some tok2 :=
            (lookupBinding_pair_distinct v.subjectClass tok1 v.objectClass tok2 v.objectClass tok2 hneSlots).mpr
              (List.Mem.tail _ (List.Mem.head []))
          by_cases hcond :
              (v.names.contains tok1 && v.names.contains tok2 &&
                ([v.subjectClass, v.objectClass] == [v.subjectClass, v.objectClass])) = true
          · change (if v.names.contains tok1 && v.names.contains tok2 &&
                ([v.subjectClass, v.objectClass] == [v.subjectClass, v.objectClass]) then _ else none) =
                some t at hrec
            rw [hcond] at hrec
            simp only [ite_true] at hrec
            change (match lookupBinding [(v.subjectClass, tok1), (v.objectClass, tok2)] v.subjectClass,
                lookupBinding [(v.subjectClass, tok1), (v.objectClass, tok2)] v.objectClass with
              | some v1, some v2 => some (FactTerm.mk r [v1, v2])
              | _, _ => none) = some t at hrec
            rw [hb1, hb2] at hrec
            injection hrec with htEq
            subst htEq
            rcases Bool.and_eq_true_iff.mp hcond with ⟨ht12, _⟩
            rcases Bool.and_eq_true_iff.mp ht12 with ⟨ht1, ht2⟩
            have hrCont : v.relations.contains r = true :=
              (contains_iff_mem_str v.relations r).mpr hrMem
            refine ⟨VocabAssertion.binary r tok1 tok2, ?_, ?_⟩
            · dsimp only [isAdmissibleIn]
              rw [hrCont, ht1, ht2]
              rfl
            · dsimp only [decodeVocabTerm]
              rw [hrCont]
              rfl
          · have hcondf :
                (v.names.contains tok1 && v.names.contains tok2 &&
                  ([v.subjectClass, v.objectClass] == [v.subjectClass, v.objectClass])) = false :=
              Bool.eq_false_iff.mpr hcond
            change (if v.names.contains tok1 && v.names.contains tok2 &&
                ([v.subjectClass, v.objectClass] == [v.subjectClass, v.objectClass]) then _ else none) =
                some t at hrec
            rw [hcondf] at hrec
            contradiction

theorem exactness_prog (v : Vocabulary) (hwf : WellFormedVocabulary v) :
    ∀ (prog : SourceProgram) (terms : TermList),
      readProgram (vocabularyDecl v) prog = some terms →
      ∃ (assertions : List VocabAssertion),
        AdmissibleIn v assertions ∧ decodeVocabTerms v terms = some assertions
  | [], terms, h => by
    dsimp [readProgram] at h
    injection h with htEq
    subst htEq
    exact ⟨[], rfl, rfl⟩
  | line :: rest, terms, h => by
    dsimp [readProgram] at h
    by_cases hbc : isBlankOrCommentLine line = true
    · rw [hbc] at h
      simp at h
      exact exactness_prog v hwf rest terms h
    · have hbcf : isBlankOrCommentLine line = false := Bool.eq_false_iff.mpr hbc
      rw [hbcf] at h
      by_cases hfl : isFragmentLineBool line = true
      · rw [hfl] at h
        simp at h
        cases hr : recognizeLine (vocabularyDecl v) line with
        | none =>
          rw [hr] at h
          contradiction
        | some t =>
          cases hp : readProgram (vocabularyDecl v) rest with
          | none =>
            rw [hr, hp] at h
            contradiction
          | some ts =>
            rw [hr, hp] at h
            injection h with htEq
            subst htEq
            rcases exactness_line v hwf line t hr with ⟨a, ha, hdecA⟩
            rcases exactness_prog v hwf rest ts hp with ⟨asRest, hadmRest, hdecRest⟩
            refine ⟨a :: asRest, ?_, ?_⟩
            · unfold AdmissibleIn at hadmRest ⊢
              simp [List.all, ha, hadmRest]
            · simp [decodeVocabTerms, hdecA, hdecRest]
      · have hflf : isFragmentLineBool line = false := Bool.eq_false_iff.mpr hfl
        rw [hflf] at h
        contradiction

theorem holdsVocabAssertion_iff_term (v : Vocabulary)
    (D : Type) (val : String → D) (P : String → D → Prop) (R : String → D → D → Prop)
    (a : VocabAssertion) (ha : isAdmissibleIn v a = true) :
    HoldsVocabAssertion D val P R a ↔ HoldsVocabTerm v D val P R (vocabAssertionFactTerm a) := by
  cases a with
  | unary p x =>
    unfold isAdmissibleIn at ha
    rcases Bool.and_eq_true_iff.mp ha with ⟨hpCont, _⟩
    have hpMem : p ∈ v.classes := (contains_iff_mem_str v.classes p).mp hpCont
    simp [HoldsVocabAssertion, vocabAssertionFactTerm, HoldsVocabTerm, hpMem]
  | binary r x y =>
    unfold isAdmissibleIn at ha
    rcases Bool.and_eq_true_iff.mp ha with ⟨hrx, _⟩
    rcases Bool.and_eq_true_iff.mp hrx with ⟨hrCont, _⟩
    have hrMem : r ∈ v.relations := (contains_iff_mem_str v.relations r).mp hrCont
    simp [HoldsVocabAssertion, vocabAssertionFactTerm, HoldsVocabTerm, hrMem]

theorem satisfiesVocabSource_iff_terms (v : Vocabulary)
    (D : Type) (val : String → D) (P : String → D → Prop) (R : String → D → D → Prop) :
    ∀ (assertions : List VocabAssertion),
      assertions.all (isAdmissibleIn v) = true →
      (SatisfiesVocabSource D val P R assertions ↔
       SatisfiesVocabTerms v D val P R (assertions.map vocabAssertionFactTerm))
  | [], _ => Iff.rfl
  | a :: rest, hall => by
    unfold List.all at hall
    rcases Bool.and_eq_true_iff.mp hall with ⟨ha, hrest⟩
    simp [SatisfiesVocabSource, SatisfiesVocabTerms,
      holdsVocabAssertion_iff_term v D val P R a ha,
      satisfiesVocabSource_iff_terms v D val P R rest hrest]

theorem ontology_vocabulary_law : OntologyVocabularyLaw := by
  refine ⟨⟨by unfold WellFormedVocabulary; decide, rfl⟩, ?_⟩
  intro v hwf
  refine ⟨vocabularyDecl_wellFormed v hwf, ?_, ?_, ?_, ?_⟩
  · intro assertions hadm
    unfold AdmissibleIn at hadm
    exact ⟨assertions.map vocabAssertionFactTerm,
      readProgram_encodeVocabAssertions v hwf assertions hadm,
      decodeVocabTerms_map_vocabFactTerm v hwf assertions hadm⟩
  · intro prog terms hread
    exact exactness_prog v hwf prog terms hread
  · intro D _hdom val P R assertions hadm
    unfold AdmissibleIn at hadm
    rw [readProgram_encodeVocabAssertions v hwf assertions hadm]
    exact satisfiesVocabSource_iff_terms v D val P R assertions hadm
  · intro r x y hrMem hxMem hyMem hxy
    have hrCont : v.relations.contains r = true := (contains_iff_mem_str v.relations r).mpr hrMem
    have hxCont : v.names.contains x = true := (contains_iff_mem_str v.names x).mpr hxMem
    have hyCont : v.names.contains y = true := (contains_iff_mem_str v.names y).mpr hyMem
    have hadmXY : [VocabAssertion.binary r x y].all (isAdmissibleIn v) = true := by
      dsimp only [List.all, isAdmissibleIn]
      rw [hrCont, hxCont, hyCont]
      rfl
    have hadmYX : [VocabAssertion.binary r y x].all (isAdmissibleIn v) = true := by
      dsimp only [List.all, isAdmissibleIn]
      rw [hrCont, hyCont, hxCont]
      rfl
    refine ⟨Bool, ⟨true⟩, (fun s => s == x), (fun _ _ => True), (fun _ u w => u = true ∧ w = false), ?_, ?_⟩
    · rw [readProgram_encodeVocabAssertions v hwf [VocabAssertion.binary r x y] hadmXY]
      unfold SatisfiesVocabOutput SatisfiesVocabTerms vocabAssertionFactTerm HoldsVocabTerm
      have hxx : (x == x) = true := beq_iff_eq.mpr rfl
      have hyx : (y == x) = false := beq_eq_false_iff_ne.mpr (Ne.symm hxy)
      exact ⟨⟨hrMem, ⟨hxx, hyx⟩⟩, trivial⟩
    · rw [readProgram_encodeVocabAssertions v hwf [VocabAssertion.binary r y x] hadmYX]
      unfold SatisfiesVocabOutput SatisfiesVocabTerms vocabAssertionFactTerm HoldsVocabTerm
      have hyx : (y == x) = false := beq_eq_false_iff_ne.mpr (Ne.symm hxy)
      intro hsat
      have htrue : (y == x) = true := hsat.1.2.1
      rw [hyx] at htrue
      exact Bool.noConfusion htrue

def toWitnessVocabAssertion : Assertion → VocabAssertion
  | Assertion.unary x => VocabAssertion.unary "member" x
  | Assertion.binary x y => VocabAssertion.binary "knows" x y

def fromWitnessVocabAssertion : VocabAssertion → Option Assertion
  | VocabAssertion.unary p x => if p == "member" then some (Assertion.unary x) else none
  | VocabAssertion.binary r x y => if r == "knows" then some (Assertion.binary x y) else none

theorem isAdmissibleName_cases (x : IndName) (h : isAdmissibleName x = true) :
    x = "ada" ∨ x = "ben" := by
  unfold isAdmissibleName nameAda nameBen at h
  rcases Bool.or_eq_true_iff.mp h with ha | hb
  · exact Or.inl (beq_iff_eq.mp ha)
  · exact Or.inr (beq_iff_eq.mp hb)

theorem toWitness_admissible (a : Assertion) (ha : isAdmissibleAssertion a = true) :
    isAdmissibleIn witnessVocabulary (toWitnessVocabAssertion a) = true := by
  cases a with
  | unary x =>
    rcases isAdmissibleName_cases x ha with rfl | rfl <;> decide
  | binary x y =>
    rcases Bool.and_eq_true_iff.mp ha with ⟨hx, hy⟩
    rcases isAdmissibleName_cases x hx with rfl | rfl <;>
    rcases isAdmissibleName_cases y hy with rfl | rfl <;>
    decide

theorem toWitness_admissibleList :
    ∀ (assertions : List Assertion),
      assertions.all isAdmissibleAssertion = true →
      AdmissibleIn witnessVocabulary (assertions.map toWitnessVocabAssertion)
  | [], _ => rfl
  | a :: rest, hall => by
    unfold List.all at hall
    rcases Bool.and_eq_true_iff.mp hall with ⟨ha, hrest⟩
    have h1 := toWitness_admissible a ha
    have ih := toWitness_admissibleList rest hrest
    unfold AdmissibleIn at ih ⊢
    simp [List.map, h1, ih]

theorem string_append_assoc (a b c : String) :
    String.append a (String.append b c) = String.append (String.append a b) c := by
  have h1 : (String.append a (String.append b c)).toList =
      (String.append (String.append a b) c).toList := by
    change (a ++ (b ++ c)).toList = ((a ++ b) ++ c).toList
    rw [String.toList_append, String.toList_append, String.toList_append, String.toList_append,
        List.append_assoc]
  calc
    String.append a (String.append b c)
      = String.ofList (String.append a (String.append b c)).toList := (@String.ofList_toList _).symm
    _ = String.ofList (String.append (String.append a b) c).toList := by rw [h1]
    _ = String.append (String.append a b) c := @String.ofList_toList _

theorem encodeVocab_map_toWitness :
    ∀ (assertions : AssertionList),
      encodeVocabAssertions (assertions.map toWitnessVocabAssertion) = encodeAssertions assertions
  | [] => rfl
  | a :: rest => by
    have ih := encodeVocab_map_toWitness rest
    unfold encodeVocabAssertions encodeAssertions at ih ⊢
    cases a with
    | unary x =>
      change String at x
      dsimp only [List.map, toWitnessVocabAssertion, encodeVocabAssertion, encodeAssertion]
      rw [string_append_assoc "member" " : " x, ih]
      have hm : String.append "member" " : " = "member : " := by decide
      rw [hm]
    | binary x y =>
      change String at x y
      dsimp only [List.map, toWitnessVocabAssertion, encodeVocabAssertion, encodeAssertion,
        relationSeparator]
      rw [string_append_assoc "knows" " : " (String.append x (String.append " = " y)), ih]
      have hk : String.append "knows" " : " = "knows : " := by decide
      rw [hk]

theorem decodeTerm_eq_fromWitness (t : FactTerm) :
    decodeTerm t = Option.bind (decodeVocabTerm witnessVocabulary t) fromWitnessVocabAssertion := by
  cases t with
  | mk ctor args =>
    cases args with
    | nil => rfl
    | cons x rest1 =>
      cases rest1 with
      | nil =>
        unfold decodeTerm decodeVocabTerm witnessVocabulary
        dsimp only
        by_cases hm : (ctor == "member") = true
        · have heq : ctor = "member" := beq_iff_eq.mp hm
          subst heq
          rfl
        · have hmf : (ctor == "member") = false := Bool.eq_false_iff.mpr hm
          have hcf : (["member"].contains ctor) = false := by
            cases hc : ["member"].contains ctor with
            | false => rfl
            | true =>
              have hmem := (contains_iff_mem_str ["member"] ctor).mp hc
              have heq := List.mem_singleton.mp hmem
              subst heq
              simp at hmf
          rw [hmf, hcf]
          rfl
      | cons y rest2 =>
        cases rest2 with
        | nil =>
          unfold decodeTerm decodeVocabTerm witnessVocabulary
          dsimp only
          by_cases hk : (ctor == "knows") = true
          · have heq : ctor = "knows" := beq_iff_eq.mp hk
            subst heq
            rfl
          · have hkf : (ctor == "knows") = false := Bool.eq_false_iff.mpr hk
            have hcf : (["knows"].contains ctor) = false := by
              cases hc : ["knows"].contains ctor with
              | false => rfl
              | true =>
                have hmem := (contains_iff_mem_str ["knows"] ctor).mp hc
                have heq := List.mem_singleton.mp hmem
                subst heq
                simp at hkf
            rw [hkf, hcf]
            rfl
        | cons _ _ => rfl

theorem decodeTerms_from_decodeVocabTerms :
    ∀ (terms : List FactTerm) (assertions : List Assertion),
      decodeVocabTerms witnessVocabulary terms = some (assertions.map toWitnessVocabAssertion) →
      decodeTerms terms = some assertions
  | [], [], _ => rfl
  | [], _ :: _, h => by simp [decodeVocabTerms] at h
  | t :: ts, [], h => by
    dsimp [decodeVocabTerms, List.map] at h
    cases hdt : decodeVocabTerm witnessVocabulary t with
    | none => rw [hdt] at h; contradiction
    | some _ =>
      cases hdts : decodeVocabTerms witnessVocabulary ts with
      | none => rw [hdt, hdts] at h; contradiction
      | some _ => rw [hdt, hdts] at h; simp at h
  | t :: ts, a :: rest, h => by
    dsimp [decodeVocabTerms] at h
    cases hdt : decodeVocabTerm witnessVocabulary t with
    | none => rw [hdt] at h; contradiction
    | some va =>
      cases hdts : decodeVocabTerms witnessVocabulary ts with
      | none => rw [hdt, hdts] at h; contradiction
      | some vas =>
        rw [hdt, hdts] at h
        simp at h
        rcases h with ⟨rfl, rfl⟩
        have ih := decodeTerms_from_decodeVocabTerms ts rest hdts
        have h1 := decodeTerm_eq_fromWitness t
        rw [hdt] at h1
        have hfw : Option.bind (some (toWitnessVocabAssertion a)) fromWitnessVocabAssertion = some a := by
          cases a <;> rfl
        rw [hfw] at h1
        simp [decodeTerms, h1, ih]
        rfl

theorem satisfiesSource_iff_vocab
    (D : Type) (val : IndName → D) (P : D → Prop) (R : D → D → Prop) :
    ∀ (assertions : AssertionList),
      SatisfiesSource D val P R assertions ↔
      SatisfiesVocabSource D val (fun _ d => P d) (fun _ d1 d2 => R d1 d2)
        (assertions.map toWitnessVocabAssertion)
  | [] => Iff.rfl
  | a :: rest => by
    have ih := satisfiesSource_iff_vocab D val P R rest
    cases a <;>
      simp [SatisfiesSource, SatisfiesVocabSource, HoldsAssertion, HoldsVocabAssertion,
        toWitnessVocabAssertion, ih]

theorem holdsFactTerm_iff_vocab
    (D : Type) (val : IndName → D) (P : D → Prop) (R : D → D → Prop) (t : FactTerm) :
    HoldsFactTerm D val P R t ↔
    HoldsVocabTerm witnessVocabulary D val (fun _ d => P d) (fun _ d1 d2 => R d1 d2) t := by
  cases t with
  | mk ctor args =>
    cases args with
    | nil => rfl
    | cons x rest1 =>
      cases rest1 with
      | nil =>
        simp [HoldsFactTerm, HoldsVocabTerm, witnessVocabulary]
      | cons y rest2 =>
        cases rest2 with
        | nil =>
          simp [HoldsFactTerm, HoldsVocabTerm, witnessVocabulary]
        | cons _ _ => rfl

theorem satisfiesTermList_iff_vocab
    (D : Type) (val : IndName → D) (P : D → Prop) (R : D → D → Prop) :
    ∀ (ts : TermList),
      SatisfiesTermList D val P R ts ↔
      SatisfiesVocabTerms witnessVocabulary D val (fun _ d => P d) (fun _ d1 d2 => R d1 d2) ts
  | [] => Iff.rfl
  | t :: ts => by
    simp [SatisfiesTermList, SatisfiesVocabTerms,
      holdsFactTerm_iff_vocab D val P R t,
      satisfiesTermList_iff_vocab D val P R ts]

theorem satisfiesOutput_iff_vocab
    (D : Type) (val : IndName → D) (P : D → Prop) (R : D → D → Prop) (out : Option TermList) :
    SatisfiesOutput D val P R out ↔
    SatisfiesVocabOutput witnessVocabulary D val (fun _ d => P d) (fun _ d1 d2 => R d1 d2) out := by
  cases out with
  | none => exact Iff.rfl
  | some ts => exact satisfiesTermList_iff_vocab D val P R ts

theorem ontology_law : OntologyLaw := by
  rcases ontology_vocabulary_law with ⟨⟨hwfWit, hdeclWit⟩, hall⟩
  rcases hall witnessVocabulary hwfWit with ⟨hwfDecl, hround, _hexact, hsat, hdir⟩
  have hontEq : vocabularyDecl witnessVocabulary = ontologyDecl := hdeclWit
  refine ⟨?_, ?_, ?_, ?_⟩
  · rw [← hontEq]
    exact hwfDecl
  · intro assertions hadm
    unfold AdmissibleAssertions admissibleAssertionsBool at hadm
    have hadmV := toWitness_admissibleList assertions hadm
    rcases hround (assertions.map toWitnessVocabAssertion) hadmV with ⟨terms, hread, hdec⟩
    rw [hontEq, encodeVocab_map_toWitness assertions] at hread
    exact ⟨terms, hread, decodeTerms_from_decodeVocabTerms terms assertions hdec⟩
  · intro D hdom val P R assertions hadm
    unfold AdmissibleAssertions admissibleAssertionsBool at hadm
    have hadmV := toWitness_admissibleList assertions hadm
    have hsatV := hsat D hdom val (fun _ d => P d) (fun _ d1 d2 => R d1 d2)
      (assertions.map toWitnessVocabAssertion) hadmV
    rw [hontEq, encodeVocab_map_toWitness assertions] at hsatV
    exact (satisfiesSource_iff_vocab D val P R assertions).trans
      (hsatV.trans (satisfiesOutput_iff_vocab D val P R
        (readProgram ontologyDecl (encodeAssertions assertions))).symm)
  · have hkMem : "knows" ∈ witnessVocabulary.relations := List.Mem.head []
    have haMem : nameAda ∈ witnessVocabulary.names := List.Mem.head ["ben"]
    have hbMem : nameBen ∈ witnessVocabulary.names := List.Mem.tail "ada" (List.Mem.head [])
    have habNe : nameAda ≠ nameBen := by
      change ("ada" : String) ≠ "ben"
      decide
    rcases hdir "knows" nameAda nameBen hkMem haMem hbMem habNe with
      ⟨D, hdom, val, P', R', hsatAB, hnsatBA⟩
    have hadmAB : AdmissibleIn witnessVocabulary [VocabAssertion.binary "knows" nameAda nameBen] := by
      unfold AdmissibleIn witnessVocabulary isAdmissibleIn nameAda nameBen
      decide
    have hadmBA : AdmissibleIn witnessVocabulary [VocabAssertion.binary "knows" nameBen nameAda] := by
      unfold AdmissibleIn witnessVocabulary isAdmissibleIn nameAda nameBen
      decide
    have hsrcAB : SatisfiesVocabSource D val P' R' [VocabAssertion.binary "knows" nameAda nameBen] :=
      (hsat D hdom val P' R' [VocabAssertion.binary "knows" nameAda nameBen] hadmAB).mpr hsatAB
    have hnsrcBA : ¬ SatisfiesVocabSource D val P' R' [VocabAssertion.binary "knows" nameBen nameAda] :=
      fun h => hnsatBA ((hsat D hdom val P' R' [VocabAssertion.binary "knows" nameBen nameAda] hadmBA).mp h)
    have hrAB : R' "knows" (val nameAda) (val nameBen) := hsrcAB.1
    have hnrBA : ¬ R' "knows" (val nameBen) (val nameAda) := fun h => hnsrcBA ⟨h, trivial⟩
    let P : D → Prop := P' "member"
    let R : D → D → Prop := R' "knows"
    have hsrcAB' : SatisfiesVocabSource D val (fun _ d => P d) (fun _ d1 d2 => R d1 d2)
        ((singleBinary nameAda nameBen).map toWitnessVocabAssertion) := ⟨hrAB, trivial⟩
    have houtABV : SatisfiesVocabOutput witnessVocabulary D val (fun _ d => P d) (fun _ d1 d2 => R d1 d2)
        (readProgram (vocabularyDecl witnessVocabulary)
          (encodeVocabAssertions ((singleBinary nameAda nameBen).map toWitnessVocabAssertion))) :=
      (hsat D hdom val (fun _ d => P d) (fun _ d1 d2 => R d1 d2)
        ((singleBinary nameAda nameBen).map toWitnessVocabAssertion) hadmAB).mp hsrcAB'
    rw [hontEq, encodeVocab_map_toWitness (singleBinary nameAda nameBen)] at houtABV
    have houtAB : SatisfiesOutput D val P R
        (readProgram ontologyDecl (encodeAssertions (singleBinary nameAda nameBen))) :=
      (satisfiesOutput_iff_vocab D val P R _).mpr houtABV
    have hnoutBA : ¬ SatisfiesOutput D val P R
        (readProgram ontologyDecl (encodeAssertions (singleBinary nameBen nameAda))) := by
      intro houtBA
      have houtBAV := (satisfiesOutput_iff_vocab D val P R _).mp houtBA
      rw [← hontEq, ← encodeVocab_map_toWitness (singleBinary nameBen nameAda)] at houtBAV
      have hsrcBA' := (hsat D hdom val (fun _ d => P d) (fun _ d1 d2 => R d1 d2)
        ((singleBinary nameBen nameAda).map toWitnessVocabAssertion) hadmBA).mpr houtBAV
      exact hnrBA hsrcBA'.1
    exact ⟨D, hdom, val, P, R, hrAB, hnrBA, houtAB, hnoutBA⟩

end TypifyMetalogic
