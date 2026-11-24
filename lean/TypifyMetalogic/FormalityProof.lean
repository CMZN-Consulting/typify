import TypifyMetalogic.FormalityStatement

namespace TypifyMetalogic

theorem findByKey_mem {α : Type} (key : α → String) :
    ∀ (xs : List α) (k : String) (x : α),
      findByKey key xs k = some x → x ∈ xs ∧ key x = k
  | [], _, _, h => by simp [findByKey] at h
  | y :: ys, k, x, h => by
    unfold findByKey at h
    by_cases hk : (key y == k) = true
    · rw [hk] at h
      simp at h
      subst h
      have hkeq : key y = k := beq_iff_eq.mp hk
      exact ⟨List.Mem.head ys, hkeq⟩
    · have hkf : (key y == k) = false := Bool.eq_false_iff.mpr hk
      rw [hkf] at h
      simp at h
      have ⟨hmem, hkey⟩ := findByKey_mem key ys k x h
      exact ⟨List.Mem.tail y hmem, hkey⟩

theorem any_key_false_of_mem {α : Type} (key : α → String) :
    ∀ (ys : List α) (k : String),
      ys.any (fun y => key y == k) = false →
      ∀ y, y ∈ ys → key y ≠ k
  | [], _, _, _, hmem => nomatch hmem
  | z :: zs, k, hany, y, hmem => by
    dsimp [List.any] at hany
    cases hzk : (key z == k) with
    | true =>
      rw [hzk] at hany
      contradiction
    | false =>
      rw [hzk] at hany
      simp at hany
      cases hmem with
      | head =>
        exact beq_eq_false_iff_ne.mp hzk
      | tail _ htail =>
        exact hany y htail

theorem mem_findByKey_of_noDup {α : Type} (key : α → String) :
    ∀ (xs : List α) (x : α),
      noDupKeys key xs = true →
      x ∈ xs →
      findByKey key xs (key x) = some x
  | [], _, _, hmem => nomatch hmem
  | y :: ys, x, hnodup, hmem => by
    unfold noDupKeys at hnodup
    have ⟨hnot, htailnd⟩ := Bool.and_eq_true_iff.mp hnodup
    have hanyf : ys.any (fun z => key z == key y) = false := by
      cases h : ys.any (fun z => key z == key y)
      · rfl
      · rw [h] at hnot; contradiction
    unfold findByKey
    cases hmem with
    | head =>
      simp
    | tail _ htail =>
      have hneq : key x ≠ key y := any_key_false_of_mem key ys (key y) hanyf x htail
      have hbeq : (key y == key x) = false := beq_eq_false_iff_ne.mpr (Ne.symm hneq)
      rw [hbeq]
      simpa using mem_findByKey_of_noDup key ys x htailnd htail

theorem findByKey_iff_mem {α : Type} (key : α → String) (xs : List α) (k : String) (x : α)
    (hnodup : noDupKeys key xs = true) :
    findByKey key xs k = some x ↔ (x ∈ xs ∧ key x = k) := by
  constructor
  · exact findByKey_mem key xs k x
  · rintro ⟨hmem, rfl⟩
    exact mem_findByKey_of_noDup key xs x hnodup hmem

theorem splitFirstColon_sound :
    ∀ (cs lhs rhs : List Char),
      splitFirstColon cs = some (lhs, rhs) →
      cs = lhs ++ (':' :: rhs) ∧ (∀ c, c ∈ lhs → c ≠ ':')
  | [], _, _, h => by simp [splitFirstColon] at h
  | x :: xs, lhs, rhs, h => by
    unfold splitFirstColon at h
    by_cases hx : (x == ':') = true
    · rw [hx] at h
      simp at h
      rcases h with ⟨rfl, rfl⟩
      have hxc : x = ':' := beq_iff_eq.mp hx
      subst hxc
      exact ⟨rfl, fun _ hm => nomatch hm⟩
    · have hxf : (x == ':') = false := Bool.eq_false_iff.mpr hx
      rw [hxf] at h
      simp at h
      cases hs : splitFirstColon xs with
      | none =>
        rw [hs] at h
        contradiction
      | some pair =>
        rw [hs] at h
        rcases pair with ⟨l, r⟩
        simp at h
        rcases h with ⟨rfl, rfl⟩
        have ⟨heq, hall⟩ := splitFirstColon_sound xs l r hs
        refine ⟨by rw [heq]; rfl, ?_⟩
        intro c hc
        cases hc with
        | head => exact beq_eq_false_iff_ne.mp hxf
        | tail _ hmem => exact hall c hmem

theorem splitFirstColon_complete :
    ∀ (lhs rhs : List Char),
      (∀ c, c ∈ lhs → c ≠ ':') →
      splitFirstColon (lhs ++ (':' :: rhs)) = some (lhs, rhs)
  | [], rhs, _ => by
    simp [splitFirstColon]
  | x :: ls, rhs, hall => by
    have hx : x ≠ ':' := hall x (List.Mem.head ls)
    have hxf : (x == ':') = false := beq_eq_false_iff_ne.mpr hx
    have hrest : ∀ c, c ∈ ls → c ≠ ':' := fun c hc => hall c (List.Mem.tail x hc)
    simp [splitFirstColon, hxf, splitFirstColon_complete ls rhs hrest]

theorem splitColonLine_iff_shape (line : SourceLine) (verb rhs : String) :
    splitColonLine line = some (verb, rhs) ↔ HasColonShape line verb rhs := by
  constructor
  · intro h
    unfold splitColonLine at h
    by_cases hfrag : isFragmentLineBool line = true
    · rw [hfrag] at h
      dsimp only [ite_true] at h
      cases hs : splitFirstColon (trimChars (String.toList line)) with
      | none =>
        rw [hs] at h
        contradiction
      | some pair =>
        rw [hs] at h
        rcases pair with ⟨lhsRaw, rhsRaw⟩
        dsimp only at h
        by_cases hv : (String.ofList (trimChars lhsRaw) == "") = true
        · rw [hv] at h
          contradiction
        · have hvf : (String.ofList (trimChars lhsRaw) == "") = false := Bool.eq_false_iff.mpr hv
          rw [hvf] at h
          simp at h
          rcases h with ⟨rfl, rfl⟩
          have ⟨hcs, hall⟩ := splitFirstColon_sound (trimChars (String.toList line)) lhsRaw rhsRaw hs
          exact HasColonShape.intro line lhsRaw rhsRaw
            (String.ofList (trimChars lhsRaw)) (String.ofList (trimChars rhsRaw))
            hfrag hcs hall rfl (beq_eq_false_iff_ne.mp hvf) rfl
    · have hfragf : isFragmentLineBool line = false := Bool.eq_false_iff.mpr hfrag
      rw [hfragf] at h
      contradiction
  · intro hsh
    cases hsh with
    | intro lhsRaw rhsRaw _ _ hfrag hcs hall hveq hvne hreq =>
      subst hveq
      subst hreq
      unfold splitColonLine
      have hfragb : isFragmentLineBool line = true := hfrag
      have hs := splitFirstColon_complete lhsRaw rhsRaw hall
      have hvf : (String.ofList (trimChars lhsRaw) == "") = false := beq_eq_false_iff_ne.mpr hvne
      rw [hfragb]
      dsimp only [ite_true]
      rw [hcs, hs]
      dsimp only
      rw [hvf]
      rfl

theorem startsWithChars_self_prefix :
    ∀ (p rest : List Char), startsWithChars p (p ++ rest) = true
  | [], _ => rfl
  | c :: cs, rest => by
    simp [startsWithChars, startsWithChars_self_prefix cs rest]

theorem drop_self_prefix :
    ∀ (p rest : List Char), (p ++ rest).drop p.length = rest
  | [], _ => rfl
  | _ :: cs, rest => by
    simp [drop_self_prefix cs rest]

theorem startsWithChars_take_prefix :
    ∀ (p zs : List Char),
      startsWithChars p zs = true →
      zs = p ++ zs.drop p.length
  | [], _, _ => rfl
  | _ :: _, [], h => by simp [startsWithChars] at h
  | c :: cs, z :: zs, h => by
    unfold startsWithChars at h
    have ⟨hcz, hrest⟩ := Bool.and_eq_true_iff.mp h
    have heq : c = z := beq_iff_eq.mp hcz
    subst heq
    have ih := startsWithChars_take_prefix cs zs hrest
    simpa using congrArg (List.cons c) ih

theorem startsWithChars_append_right :
    ∀ (p xs ys : List Char),
      p.length ≤ xs.length →
      startsWithChars p (xs ++ ys) = startsWithChars p xs
  | [], _, _, _ => rfl
  | _ :: _, [], _, hle => absurd hle (Nat.not_succ_le_zero _)
  | c :: cs, x :: xs, ys, hle => by
    have hle' : cs.length ≤ xs.length := Nat.le_of_succ_le_succ hle
    simp [startsWithChars, startsWithChars_append_right cs xs ys hle']

theorem splitAtFirstSep_sound :
    ∀ (sep rhs lhs rest : List Char),
      splitAtFirstSep sep rhs = some (lhs, rest) →
      rhs = lhs ++ sep ++ rest
  | _, [], _, _, h => by simp [splitAtFirstSep] at h
  | sep, x :: xs, lhs, rest, h => by
    unfold splitAtFirstSep at h
    by_cases hsw : startsWithChars sep (x :: xs) = true
    · rw [hsw] at h
      simp at h
      rcases h with ⟨rfl, rfl⟩
      simpa using startsWithChars_take_prefix sep (x :: xs) hsw
    · have hswf : startsWithChars sep (x :: xs) = false := Bool.eq_false_iff.mpr hsw
      rw [hswf] at h
      simp at h
      cases hs : splitAtFirstSep sep xs with
      | none =>
        rw [hs] at h
        contradiction
      | some pair =>
        rw [hs] at h
        rcases pair with ⟨l, r⟩
        simp at h
        rcases h with ⟨rfl, rfl⟩
        have ih := splitAtFirstSep_sound sep xs l r hs
        rw [ih]
        simp [List.append_assoc]

theorem splitAtFirstSep_complete :
    ∀ (sep u w : List Char),
      sep ≠ [] →
      noEarlySepMatch sep u = true →
      splitAtFirstSep sep (u ++ sep ++ w) = some (u, w)
  | [], _, _, hne, _ => absurd rfl hne
  | s₀ :: sRest, [], w, _, _ => by
    have hsw : startsWithChars (s₀ :: sRest) (s₀ :: (sRest ++ w)) = true :=
      startsWithChars_self_prefix (s₀ :: sRest) w
    have hdrop : (sRest ++ w).drop sRest.length = w := drop_self_prefix sRest w
    change splitAtFirstSep (s₀ :: sRest) (s₀ :: (sRest ++ w)) = some ([], w)
    unfold splitAtFirstSep
    rw [hsw]
    simp [hdrop]
  | sep, c :: u', w, hne, hno => by
    unfold noEarlySepMatch at hno
    have ⟨hnot, htail⟩ := Bool.and_eq_true_iff.mp hno
    have hswf0 : startsWithChars sep (c :: (u' ++ sep)) = false := by
      cases hs0 : startsWithChars sep (c :: (u' ++ sep))
      · rfl
      · rw [hs0] at hnot; contradiction
    have hlen : sep.length ≤ (c :: (u' ++ sep)).length := by
      simp [List.length_append]
      omega
    have hswf : startsWithChars sep ((c :: (u' ++ sep)) ++ w) = false := by
      rw [startsWithChars_append_right sep (c :: (u' ++ sep)) w hlen]
      exact hswf0
    have hswf' : startsWithChars sep (c :: (u' ++ (sep ++ w))) = false := by
      simpa [List.append_assoc] using hswf
    have ih : splitAtFirstSep sep (u' ++ (sep ++ w)) = some (u', w) := by
      simpa [List.append_assoc] using splitAtFirstSep_complete sep u' w hne htail
    have htarget : (c :: u') ++ sep ++ w = c :: (u' ++ (sep ++ w)) := by
      simp [List.append_assoc]
    rw [htarget]
    unfold splitAtFirstSep
    simp [hswf', ih]

theorem all_mem_prop {α : Type} (p : α → Bool) :
    ∀ (xs : List α) (x : α), xs.all p = true → x ∈ xs → p x = true
  | [], _, _, hm => nomatch hm
  | y :: ys, x, hall, hm => by
    unfold List.all at hall
    have ⟨hy, hys⟩ := Bool.and_eq_true_iff.mp hall
    cases hm with
    | head => exact hy
    | tail _ htail => exact all_mem_prop p ys x hys htail

theorem contains_iff_mem_str (xs : List String) (s : String) :
    xs.contains s = true ↔ s ∈ xs := by
  simp

theorem lookupBinding_singleton (slot v q : String) :
    lookupBinding [(slot, v)] q = if slot == q then some v else none := by
  unfold lookupBinding findByKey
  by_cases h : (slot == q) = true
  · rw [h]; simp
  · have hf : (slot == q) = false := Bool.eq_false_iff.mpr h
    rw [hf]
    rfl

theorem lookupBinding_pair_distinct (s1 v1 s2 v2 q w : String) (hne : s1 ≠ s2) :
    (lookupBinding [(s1, v1), (s2, v2)] q = some w) ↔ (q, w) ∈ [(s1, v1), (s2, v2)] := by
  have hnd : noDupKeys Prod.fst [(s1, v1), (s2, v2)] = true := by
    have hne' : (s2 == s1) = false := beq_eq_false_iff_ne.mpr (Ne.symm hne)
    simp [noDupKeys, List.any, hne']
  unfold lookupBinding
  constructor
  · intro h
    cases hf : findByKey Prod.fst [(s1, v1), (s2, v2)] q with
    | none =>
      rw [hf] at h
      contradiction
    | some pair =>
      rw [hf] at h
      rcases pair with ⟨k, val⟩
      simp at h
      subst h
      have ⟨hm, hk⟩ := (findByKey_iff_mem Prod.fst [(s1, v1), (s2, v2)] q (k, val) hnd).mp hf
      subst hk
      exact hm
  · intro hm
    have hf := (findByKey_iff_mem Prod.fst [(s1, v1), (s2, v2)] q (q, w) hnd).mpr ⟨hm, rfl⟩
    rw [hf]

theorem recognizeLine_iff_derives (d : Decl) (line : SourceLine)
    (hwf : WellFormedDecl d) (_hfrag : FragmentLine line) (t : FactTerm) :
    recognizeLine d line = some t ↔ DerivesLine d line t := by
  unfold WellFormedDecl wellFormedDeclBool at hwf
  rcases Bool.and_eq_true_iff.mp hwf with ⟨hwf1, hformsAll⟩
  rcases Bool.and_eq_true_iff.mp hwf1 with ⟨hwf2, _htermsAll⟩
  rcases Bool.and_eq_true_iff.mp hwf2 with ⟨hwf3, _hclassesAll⟩
  rcases Bool.and_eq_true_iff.mp hwf3 with ⟨hwf4, hndForms⟩
  rcases Bool.and_eq_true_iff.mp hwf4 with ⟨hndClasses, hndTerms⟩
  constructor
  · intro hrec
    unfold recognizeLine at hrec
    cases hsc : splitColonLine line with
    | none =>
      rw [hsc] at hrec
      contradiction
    | some vp =>
      rw [hsc] at hrec
      rcases vp with ⟨verb, rhs⟩
      dsimp only at hrec
      have hshape : HasColonShape line verb rhs := (splitColonLine_iff_shape line verb rhs).mp hsc
      cases hlf : lookupForm d verb with
      | none =>
        rw [hlf] at hrec
        contradiction
      | some form =>
        rw [hlf] at hrec
        have ⟨hformMem, hformVerb⟩ :=
          (findByKey_iff_mem FormDecl.verb d.forms verb form hndForms).mp hlf
        have hwff : wellFormedForm d form = true :=
          all_mem_prop (wellFormedForm d) d.forms form hformsAll hformMem
        cases form with
        | unary fverb slot ctor doesSlot =>
          dsimp only at hrec
          cases hlc : lookupClass d slot with
          | none =>
            rw [hlc] at hrec
            contradiction
          | some cls =>
            cases hlt : lookupTerm d ctor with
            | none =>
              rw [hlc, hlt] at hrec
              contradiction
            | some td =>
              rw [hlc, hlt] at hrec
              simp at hrec
              rcases hrec with ⟨⟨hrhsIn, htdParams⟩, hbind⟩
              have ⟨hclsMem, hclsName⟩ :=
                (findByKey_iff_mem ClassDecl.name d.classes slot cls hndClasses).mp hlc
              have ⟨htdMem, htdCtor⟩ :=
                (findByKey_iff_mem TermDecl.ctor d.terms ctor td hndTerms).mp hlt
              cases hlb : lookupBinding [(slot, rhs)] doesSlot with
              | none =>
                rw [hlb] at hbind
                contradiction
              | some v1 =>
                rw [hlb] at hbind
                injection hbind with htEq
                subst htEq
                rw [lookupBinding_singleton] at hlb
                by_cases hs : (slot == doesSlot) = true
                · rw [hs] at hlb
                  simp at hlb
                  subst hlb
                  have hsEq : slot = doesSlot := beq_iff_eq.mp hs
                  have htdEq : td = TermDecl.mk ctor [doesSlot] := by
                    cases td
                    simp at htdCtor htdParams
                    subst htdCtor
                    subst htdParams
                    rfl
                  subst htdEq
                  refine DerivesLine.intro verb rhs
                    (FormDecl.unary fverb slot ctor doesSlot) [(slot, rhs)]
                    hshape hformMem hformVerb (CapturesForm.unary fverb slot ctor doesSlot rhs)
                    (ValidCaptures.cons slot rhs cls [] hclsMem hclsName hrhsIn ValidCaptures.nil)
                    (BuildsFactTerm.unary fverb slot ctor doesSlot rhs htdMem ?_)
                  subst hsEq
                  exact List.Mem.head []
                · have hsf : (slot == doesSlot) = false := Bool.eq_false_iff.mpr hs
                  rw [hsf] at hlb
                  contradiction
        | binary fverb slot1 sep slot2 ctor does1 does2 =>
          dsimp only at hrec
          cases hsp : splitAtFirstSep sep.toList rhs.toList with
          | none =>
            rw [hsp] at hrec
            contradiction
          | some lr =>
            rw [hsp] at hrec
            rcases lr with ⟨lhsChars, rhsChars⟩
            dsimp only at hrec
            cases hlc1 : lookupClass d slot1 with
            | none =>
              rw [hlc1] at hrec
              contradiction
            | some cls1 =>
              cases hlc2 : lookupClass d slot2 with
              | none =>
                rw [hlc1, hlc2] at hrec
                contradiction
              | some cls2 =>
                cases hlt : lookupTerm d ctor with
                | none =>
                  rw [hlc1, hlc2, hlt] at hrec
                  contradiction
                | some td =>
                  rw [hlc1, hlc2, hlt] at hrec
                  simp at hrec
                  rcases hrec with ⟨⟨⟨htok1, htok2⟩, htdParams⟩, hbind⟩
                  have ⟨hcls1Mem, hcls1Name⟩ :=
                    (findByKey_iff_mem ClassDecl.name d.classes slot1 cls1 hndClasses).mp hlc1
                  have ⟨hcls2Mem, hcls2Name⟩ :=
                    (findByKey_iff_mem ClassDecl.name d.classes slot2 cls2 hndClasses).mp hlc2
                  have ⟨htdMem, htdCtor⟩ :=
                    (findByKey_iff_mem TermDecl.ctor d.terms ctor td hndTerms).mp hlt
                  unfold wellFormedForm at hwff
                  have hneSlots : slot1 ≠ slot2 := by
                    intro heq
                    subst heq
                    simp at hwff
                  have hsplitStr : rhs = String.ofList lhsChars ++ sep ++ String.ofList rhsChars := by
                    have hchars := splitAtFirstSep_sound sep.toList rhs.toList lhsChars rhsChars hsp
                    have h1 : String.ofList rhs.toList =
                        String.ofList (lhsChars ++ sep.toList ++ rhsChars) :=
                      congrArg String.ofList hchars
                    rw [String.ofList_toList] at h1
                    have h2 : (String.ofList lhsChars ++ sep ++ String.ofList rhsChars).toList =
                        lhsChars ++ sep.toList ++ rhsChars := by
                      simp [String.toList_append, String.toList_ofList, List.append_assoc]
                    rw [← h2, String.ofList_toList] at h1
                    exact h1
                  let bnds := [(slot1, String.ofList lhsChars), (slot2, String.ofList rhsChars)]
                  cases hb1 : lookupBinding bnds does1 with
                  | none =>
                    simp [bnds, hb1] at hbind
                  | some v1 =>
                    cases hb2 : lookupBinding bnds does2 with
                    | none =>
                      simp [bnds, hb1, hb2] at hbind
                    | some v2 =>
                      simp [bnds, hb1, hb2] at hbind
                      subst hbind
                      have hm1 := (lookupBinding_pair_distinct slot1 (String.ofList lhsChars)
                        slot2 (String.ofList rhsChars) does1 v1 hneSlots).mp hb1
                      have hm2 := (lookupBinding_pair_distinct slot1 (String.ofList lhsChars)
                        slot2 (String.ofList rhsChars) does2 v2 hneSlots).mp hb2
                      have htdEq : td = TermDecl.mk ctor [does1, does2] := by
                        cases td
                        simp at htdCtor htdParams
                        subst htdCtor
                        subst htdParams
                        rfl
                      subst htdEq
                      rw [hsplitStr] at hshape
                      exact DerivesLine.intro verb
                        (String.ofList lhsChars ++ sep ++ String.ofList rhsChars)
                        (FormDecl.binary fverb slot1 sep slot2 ctor does1 does2) bnds
                        hshape hformMem hformVerb
                        (CapturesForm.binary fverb slot1 sep slot2 ctor does1 does2
                          (String.ofList lhsChars) (String.ofList rhsChars))
                        (ValidCaptures.cons slot1 (String.ofList lhsChars) cls1
                          [(slot2, String.ofList rhsChars)] hcls1Mem hcls1Name htok1
                          (ValidCaptures.cons slot2 (String.ofList rhsChars) cls2 []
                            hcls2Mem hcls2Name htok2 ValidCaptures.nil))
                        (BuildsFactTerm.binary fverb slot1 sep slot2 ctor does1 does2 v1 v2
                          htdMem hm1 hm2)
  · rintro ⟨verb, rhs, form, bindings, hshape, hformMem, hformVerb, hcap, hvalid, hbuild⟩
    have hsc : splitColonLine line = some (verb, rhs) :=
      (splitColonLine_iff_shape line verb rhs).mpr hshape
    have hlf : lookupForm d verb = some form :=
      (findByKey_iff_mem FormDecl.verb d.forms verb form hndForms).mpr ⟨hformMem, hformVerb⟩
    have hwff : wellFormedForm d form = true :=
      all_mem_prop (wellFormedForm d) d.forms form hformsAll hformMem
    unfold recognizeLine
    rw [hsc]
    dsimp only
    rw [hlf]
    cases hcap with
    | unary fverb slot ctor doesSlot _ =>
      dsimp only
      cases hvalid with
      | cons _ _ cls _ hclsMem hclsName htokMem _ =>
        cases hbuild with
        | unary _ _ _ _ v1 htdMem hbindMem =>
          have hlc : lookupClass d slot = some cls :=
            (findByKey_iff_mem ClassDecl.name d.classes slot cls hndClasses).mpr ⟨hclsMem, hclsName⟩
          have hlt : lookupTerm d ctor = some (TermDecl.mk ctor [doesSlot]) :=
            (findByKey_iff_mem TermDecl.ctor d.terms ctor (TermDecl.mk ctor [doesSlot]) hndTerms).mpr
              ⟨htdMem, rfl⟩
          rw [hlc, hlt]
          cases List.mem_singleton.mp hbindMem
          simp [htokMem, lookupBinding_singleton]
    | binary fverb slot1 sep slot2 ctor does1 does2 tok1 tok2 =>
      dsimp only
      cases hvalid with
      | cons _ _ cls1 _ hcls1Mem hcls1Name htok1Mem hvalid2 =>
        cases hvalid2 with
        | cons _ _ cls2 _ hcls2Mem hcls2Name htok2Mem _ =>
          cases hbuild with
          | binary _ _ _ _ _ _ _ v1 v2 htdMem hbind1 hbind2 =>
            have hlc1 : lookupClass d slot1 = some cls1 :=
              (findByKey_iff_mem ClassDecl.name d.classes slot1 cls1 hndClasses).mpr ⟨hcls1Mem, hcls1Name⟩
            have hlc2 : lookupClass d slot2 = some cls2 :=
              (findByKey_iff_mem ClassDecl.name d.classes slot2 cls2 hndClasses).mpr ⟨hcls2Mem, hcls2Name⟩
            have hlt : lookupTerm d ctor = some (TermDecl.mk ctor [does1, does2]) :=
              (findByKey_iff_mem TermDecl.ctor d.terms ctor (TermDecl.mk ctor [does1, does2]) hndTerms).mpr
                ⟨htdMem, rfl⟩
            unfold wellFormedForm at hwff
            have hneSlots : slot1 ≠ slot2 := by
              intro heq; subst heq; simp at hwff
            have hsepNe : sep.toList ≠ [] := by
              intro hnil
              have hleg : isLegalSeparator sep = true := by
                rcases Bool.and_eq_true_iff.mp hwff with ⟨hwffL, _⟩
                rcases Bool.and_eq_true_iff.mp hwffL with ⟨hwffL2, _⟩
                rcases Bool.and_eq_true_iff.mp hwffL2 with ⟨hwffL3, _⟩
                rcases Bool.and_eq_true_iff.mp hwffL3 with ⟨hwffL4, _⟩
                exact (Bool.and_eq_true_iff.mp hwffL4).2
              unfold isLegalSeparator at hleg
              simp [hnil] at hleg
            have hnoEarlyAll : cls1.literals.all (fun u => noEarlySepMatch sep.toList u.toList) = true := by
              rcases Bool.and_eq_true_iff.mp hwff with ⟨_, hrhsWf⟩
              simpa [hlc1, hlc2] using hrhsWf
            have hnoEarly : noEarlySepMatch sep.toList tok1.toList = true :=
              all_mem_prop (fun u => noEarlySepMatch sep.toList u.toList) cls1.literals tok1 hnoEarlyAll htok1Mem
            have htoList : (tok1 ++ sep ++ tok2).toList = tok1.toList ++ sep.toList ++ tok2.toList := by
              simp [String.toList_append, List.append_assoc]
            have hsp : splitAtFirstSep sep.toList (tok1 ++ sep ++ tok2).toList = some (tok1.toList, tok2.toList) := by
              rw [htoList]
              exact splitAtFirstSep_complete sep.toList tok1.toList tok2.toList hsepNe hnoEarly
            rw [hsp]
            dsimp only
            rw [hlc1, hlc2, hlt]
            have hb1 : lookupBinding [(slot1, tok1), (slot2, tok2)] does1 = some v1 :=
              (lookupBinding_pair_distinct slot1 tok1 slot2 tok2 does1 v1 hneSlots).mpr hbind1
            have hb2 : lookupBinding [(slot1, tok1), (slot2, tok2)] does2 = some v2 :=
              (lookupBinding_pair_distinct slot1 tok1 slot2 tok2 does2 v2 hneSlots).mpr hbind2
            simp [String.ofList_toList, htok1Mem, htok2Mem, hb1, hb2]

theorem formality_law : FormalityLaw := by
  refine ⟨by unfold WellFormedDecl; decide, ?_⟩
  intro d line hwf hfrag
  refine ⟨?_, ?_, ?_⟩
  · intro t
    exact recognizeLine_iff_derives d line hwf hfrag t
  · intro t₁ t₂ hd1 hd2
    have hr1 := (recognizeLine_iff_derives d line hwf hfrag t₁).mpr hd1
    have hr2 := (recognizeLine_iff_derives d line hwf hfrag t₂).mpr hd2
    rw [hr1] at hr2
    injection hr2
  · unfold AcceptsLine acceptsLine
    constructor
    · intro hs
      cases hr : recognizeLine d line with
      | none =>
        rw [hr] at hs
        contradiction
      | some t =>
        exact ⟨t, (recognizeLine_iff_derives d line hwf hfrag t).mp hr⟩
    · rintro ⟨t, hd⟩
      have hr := (recognizeLine_iff_derives d line hwf hfrag t).mpr hd
      rw [hr]
      rfl

end TypifyMetalogic
