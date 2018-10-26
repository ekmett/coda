open function

-- simple implicit-heap-like navigation
namespace dir    
  @[simp] def u (i:ℕ) := (i-1)/2
  @[simp] def l (i:ℕ) := i*2+1 
  @[simp] def r (i:ℕ) := i*2+2

  def ul : left_inverse u l :=
  begin
    delta u l left_inverse at ⊢, simp_intros i,
    rw [nat.add_comm, nat.add_sub_cancel, nat.mul_div_cancel i (nat.zero_lt_one_add 1)],
  end

  def ur : left_inverse u r :=
  begin
    delta u r left_inverse at ⊢, simp_intros i,
    erw [add_comm, nat.add_sub_cancel (i*2+1) 1, add_comm, mul_comm, nat.add_mul_div_left 1 i (nat.zero_lt_one_add 1), zero_add],
  end
end dir

-- seeking some position i, from some position n.
structure cursor (n i: ℕ) := (s k: ℕ) (p: n + s * k = i ∧ s > 0)

def start(i: ℕ) : cursor 0 i := 
begin
    apply (@cursor.mk 0 i 1 i),
    apply and.intro,
    rw [zero_add, one_mul],
    exact (nat.zero_lt_one_add 0),
end

inductive action :Π (n i: ℕ), cursor n i → Type
| stop:  Π (n i: ℕ) (c : cursor n i), cursor.k c = 0                                           → action n i c
| left:  Π (n i: ℕ) (c : cursor n i), (cursor.k c - 1) % 2 = 0 → cursor (n +     cursor.s c) i → action n i c
| right: Π (n i: ℕ) (c : cursor n i), (cursor.k c - 1) % 2 = 1 → cursor (n + 2 * cursor.s c) i → action n i c

private def lt_not_gt (m n: ℕ) (mn : m < n): ¬ m ≥ n := ((@nat.lt_iff_le_not_le m n).1 mn).2

open nat
def step: Π(n i : ℕ) (c : cursor n i), action n i c :=
begin
    introv,
    cases h : c with s k p, cases p with pi ps, cases k with km1,
    refine (action.stop n i _ _); simp, -- k = 0, stop
    let ds : s * 2 > 0 := eq.subst (nat.zero_mul 2) (mul_lt_mul_of_pos_right ps (nat.zero_lt_one_add 1)),
    let range: km1%2 < 2 := @nat.mod_lt km1 2 (nat.zero_lt_one_add 1),
    by_cases h2 : km1 % 2 <= 0,
    begin -- (k-1) % 2 = 0, go left
        apply (action.left n i), exact (nat.eq_zero_of_le_zero h2),
        simp, apply (@cursor.mk (n+s) i (s*2) (km1/2) _); refine (and.intro _ ds),
        begin
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
        apply (action.right n i),
        simp, exact r_equals_1, simp, apply (@cursor.mk (n+2*s) i (s*2) (km1/2) _); refine (and.intro _ ds),
        begin  
            rw [mul_assoc, mul_comm 2 s, add_assoc],
            rw [← left_distrib s 2 (2 * (km1 / 2))],
            have one_plus_one : 2 + 2 * (km1 / 2) = 1 + 1 + 2 * (km1 / 2), refl,
            have one_plus_r : 1 + 1 = 1 + km1%2, rw [r_equals_1],
            rw [one_plus_one, one_plus_r, add_assoc, mod_add_div km1 2, add_comm 1 km1, add_one km1],
            exact pi,
        end
    end
end

-- #reduce step 0 11 (start 11)