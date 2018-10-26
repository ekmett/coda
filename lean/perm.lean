open function
open nat

-- seeking some position i, currently at some position n.
structure cursor (n s i: ℕ) := (k: ℕ) (p: n + s * k = i ∧ s > 0)

def start(i: ℕ) : cursor 0 1 i := begin
  apply (@cursor.mk 0 1 i i),
  apply and.intro,
  rw [zero_add, one_mul],
  exact (nat.zero_lt_one_add 0),
end

inductive action :Π (n s i: ℕ), cursor n s i → Type
| stop:  Π (n s i: ℕ) (c : cursor n s i), cursor.k c = 0                                        → action n s i c
| left:  Π (n s i: ℕ) (c : cursor n s i), (cursor.k c - 1) % 2 = 0 → cursor (n + s) (s*2) i     → action n s i c
| right: Π (n s i: ℕ) (c : cursor n s i), (cursor.k c - 1) % 2 = 1 → cursor (n + 2 * s) (s*2) i → action n s i c

private def lt_not_gt (m n: ℕ) (mn : m < n): ¬ m ≥ n := ((@nat.lt_iff_le_not_le m n).1 mn).2

def step: Π(n s i : ℕ) (c : cursor n s i), action n s i c := begin
  introv,
  cases h : c with k p, cases p with pi ps, cases k with km1,
  refine (action.stop n s i _ _), simp, -- k = 0, stop
  let ds : s * 2 > 0 := eq.subst (nat.zero_mul 2) (mul_lt_mul_of_pos_right ps (nat.zero_lt_one_add 1)),
  let range: km1%2 < 2 := @nat.mod_lt km1 2 (nat.zero_lt_one_add 1),
  by_cases h2 : km1 % 2 <= 0,
  begin -- (k-1) % 2 = 0, go left
    apply (action.left n s i), exact (nat.eq_zero_of_le_zero h2),
    apply (@cursor.mk (n+s) (s*2) i (km1/2) _), refine (and.intro _ ds),
    begin -- proof
      rw [← pi, ← nat.add_sub_cancel (nat.succ km1) 1, ← nat.add_comm 1 (nat.succ km1)],
      rw [← one_add km1, ← nat.add_comm (1 + km1) 1, nat.add_sub_cancel (1 + km1) 1],
      rw [left_distrib s 1 km1, ← nat.add_assoc n (s*1) (s*km1), nat.mul_one s],
      rw [nat.mul_assoc s 2 (km1/2), ← nat.zero_add (2 * (km1 / 2)), ← eq_zero_of_le_zero h2, mod_add_div km1 2],
    end
  end,
  begin -- (k-1) % 2 = 1, go right
    have r_equals_1 : km1%2 = 1,
    begin
      cases h2 : km1 % 2, let c := nat.le_of_eq h2, contradiction, -- not 0
      cases n_1, refl, -- exactly 1
      exfalso, refine (lt_not_gt (km1%2) 2 range _), erw [h2], exact (succ_le_succ (succ_le_succ (zero_le n_1))) -- not 2+
    end,
    apply (action.right n s i),
    exact r_equals_1, apply (@cursor.mk (n+2*s) (s*2) i (km1/2) _); refine (and.intro _ ds),
    begin -- proof
      rw [mul_assoc, mul_comm 2 s, add_assoc],
      rw [← left_distrib s 2 (2 * (km1 / 2))],
      have one_plus_one : 2 + 2 * (km1 / 2) = 1 + 1 + 2 * (km1 / 2), refl,
      have one_plus_r : 1 + 1 = 1 + km1%2, rw [r_equals_1],
      rw [one_plus_one, one_plus_r, add_assoc, mod_add_div km1 2, add_comm 1 km1, add_one km1],
      exact pi,
    end
  end
end

-- step shrinks k

inductive T: Π(n s : ℕ) (occ : Prop), Type
| tip : Π(n s: ℕ), T n s false
| bin : Π (n s j: ℕ) {x y : Prop}, n ≠ j ∨ x ∨ y → T (n+s) (2*s) x → T (n+2*s) (2*s) y → T n s true

-- add invariants that it is a proper permutation here?
structure tree := (occ : Prop) (content: T 0 1 occ)

/-
def set.rec: Π (n s i j: ℕ) (c: cursor n s i) (o: Prop) (t : T n s o), Σ p : Prop, T n s p
:= begin
    intros n s i j c o t,
end

def set (i j: ℕ) (t0: tree): tree :=
-/
-- #reduce step 0 11 (start 11)

-- simple implicit-heap-like navigation
namespace dir
  @[simp] def u (i:ℕ) := (i-1)/2
  @[simp] def l (i:ℕ) := i*2+1
  @[simp] def r (i:ℕ) := i*2+2

  def ul : left_inverse u l := begin
    delta u l left_inverse at ⊢, simp_intros i,
    rw [add_comm, nat.add_sub_cancel, nat.mul_div_cancel i (nat.zero_lt_one_add 1)],
  end

  def ur : left_inverse u r := begin
    delta u r left_inverse at ⊢, simp_intros i,
    erw [add_comm, nat.add_sub_cancel (i*2+1) 1, add_comm, mul_comm, nat.add_mul_div_left 1 i (nat.zero_lt_one_add 1), zero_add],
  end
end dir
