open function
open nat

-- seeking some position i, currently at some position n.
-- todo: convert a cursor to a Prop
def cursor (n s i k: ℕ) := n + s * k = i ∧ s > 0

-- k = 0 → i = n

def start(i: ℕ) : cursor 0 1 i i := begin
--   apply (@cursor.mk 0 1 i i),
  apply and.intro,
  rw [zero_add, one_mul],
  from nat.zero_lt_one_add 0,
end

inductive action :Π (n s i k: ℕ), cursor n s i k → Type
| stop:  Π (n s i: ℕ) (c : cursor n s i 0), action n s i 0 c
| left:  Π (n s i k: ℕ) (c : cursor n s i k), (k - 1) % 2 = 0 → cursor (n + s) (s*2) i ((k-1)/2) → action n s i k c
| right: Π (n s i k: ℕ) (c : cursor n s i k), (k - 1) % 2 = 1 → cursor (n + 2 * s) (s*2) i ((k-1)/2) → action n s i k c

private def lt_not_gt (m n: ℕ) (mn : m < n): ¬ m ≥ n := ((@nat.lt_iff_le_not_le m n).1 mn).2

def step: Π(n s i k : ℕ) (c : cursor n s i k), action n s i k c := begin
  introv, cases h : c with pi ps, cases k with km1,
  refine (action.stop n s i _), -- k = 0, stop
  have ds : s * 2 > 0, by rw [← nat.zero_mul 2]; from mul_lt_mul_of_pos_right ps (nat.zero_lt_one_add 1),
  have range: km1%2 < 2 := @nat.mod_lt km1 2 (nat.zero_lt_one_add 1),
  have sp : succ km1 - 1 = km1 := by simp,
  by_cases h2 : km1 % 2 <= 0,
  begin -- (k-1) % 2 = 0, go left
    apply action.left n s i (succ km1), from nat.eq_zero_of_le_zero h2, -- apply @cursor.mk (n+s) (s*2) i (km1/2) 
    refine (and.intro _ ds),
    begin -- proof
      rw [sp, ← pi, nat.mul_assoc _ 2,← nat.zero_add (2*_), ← eq_zero_of_le_zero h2, mod_add_div],
      refine (eq.symm _), rw [← one_add km1, left_distrib, mul_one, ← add_assoc],
    end
  end,
  begin -- (k-1) % 2 = 1, go right
    have r_equals_1 : km1%2 = 1,
    begin
      cases h2 : km1 % 2, let c := nat.le_of_eq h2, contradiction, -- not 0
      cases n_1, refl, -- exactly 1
      exfalso, refine (lt_not_gt (km1%2) 2 range _), erw [h2], from succ_le_succ (succ_le_succ (zero_le n_1)) -- not 2+
    end,
    apply action.right n s i, from r_equals_1, -- apply @cursor.mk (n+2*s) (s*2) i (km1/2)
    refine and.intro _ ds,
    begin -- proof
      rw [sp, mul_assoc, mul_comm _ s, add_assoc, ← left_distrib],
      have p11 : 2 + 2 * (km1 / 2) = 1 + 1 + 2 * (km1 / 2), by refl,
      have p1r : 1 + 1 = 1 + km1%2, by rw [r_equals_1],
      rw [p11, p1r, add_assoc, mod_add_div, add_comm 1 km1],
      from pi,
    end
  end
end

inductive T: Π(n s : ℕ) (occ : Prop), Type
| tip : Π(n s: ℕ), T n s false
| bin : Π (n s j: ℕ) (x y : Prop), n ≠ j ∨ x ∨ y → T (n+s) (2*s) x → T (n+2*s) (2*s) y → T n s true

structure tree := (occ : Prop) (content: T 0 1 occ)

-- def well_founded.fix_F : Π {α : Sort u} {r : α → α → Prop} {C : α → Sort v},
--  (Π (x : α), (Π (y : α), r y x → C y) → C x) → Π (x : α), acc r x → C x

-- nat.well_founded

-- induction on k
def set.rec: Π (n s i j k: ℕ) (c: cursor n s i k) (o: Prop) (t : T n s o), Σ p : Prop, T n s p := begin
  simp_intros n s i j k c o t,
  let act := step n s i k c, 
  cases hact: act, 
  have ni : n = i := c_1.1; rw [ni]; from or.inl ij, 
  cases ht: t, by_cases ij: i = j,
  -- i = j, stop, tip
  from sigma.mk false (T.tip n s), 
  -- i /= j, stop, tip
  apply sigma.mk true, apply (T.bin n s j false false), 
  apply T.tip, apply T.tip,
  by_cases ij: i = j,
  -- i = j, stop, bin
  cases ha_1: a_1,
  cases ha_2: a_2,
  -- tip/tip -> tip
  apply sigma.mk false, apply T.tip,
  -- bin/tip -> bin
  repeat { apply sigma.mk true, apply T.bin _ _ j _ _ _ a_1 a_2, simp },
  -- i /= j, stop, bin
  apply sigma.mk true, apply T.bin n s j 
  
  

end

-- def set (i j: ℕ) (t0: tree): tree :=

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
