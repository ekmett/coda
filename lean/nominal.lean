open function
open group

-- generally following 
-- https://www.cl.cam.ac.uk/~amp12/agda/choudhury/choudhury-dissertation.pdf

def atom : Type := ℕ 

def {u} neq {A : Sort u} (a b : A) : Prop
:= (a = b) → false

inductive nelem (a : ℕ) : list ℕ → Prop
| nil : nelem []
| cons : Π {b as}, nelem as -> neq a b -> nelem (b :: as)

inductive all {A : Type} (P : A → Prop): list A → Prop
| nil : all []
| cons : ∀ {a as}, P a → all as → all (a :: as)

-- all is a functor from (i -> Prop) to [i] -> Prop
def all.map {A : Type} {P Q : A -> Prop} (pq : ∀ {a : A}, P a → Q a) {as : list A}
: all P as → all Q as
:= @all.rec _ P (all Q) (all.nil Q) (λ {a as} p _ qs, all.cons (pq p) qs) as

@[pattern] def Z : ℕ := nat.zero
@[pattern] def S : ℕ -> ℕ := nat.succ

variables a b c: ℕ

@[simp]
def greater: ℕ := S (max a b)   

@[simp]
def nat_one_add : 1 + a = S a := eq.trans (nat.add_comm 1 a) (nat.add_one a)

def max_succ : max (S a) (S b) = S (max a b) := begin
  refine (eq.subst (nat_one_add a) _),
  refine (eq.subst (nat_one_add b) _),
  refine (eq.subst (nat_one_add (max a b)) _),
  exact (max_add_add_left 1 a b)
end

def greater_succ : greater (S a) (S b) = S (greater a b) := begin
  let m : max (S a) (S b) = greater a b := max_succ a b,
  refine (eq.subst m _),
  reflexivity,
end

def greater0 : greater 0 a = S a := begin
  let m0a : max 0 a = a := max_eq_right (eq.subst (nat.zero_add a) (nat.le_add_right 0 a)),
  let Sm0a : S (max 0 a) = S a := by cc,
  exact Sm0a
end
  
def greater.comm : greater a b = greater b a := begin
  let m := max_comm a b,
  let sm : S (max a b) = S (max b a) := by cc,
  exact sm    
end

variable as : list ℕ

def all_lt (b : ℕ) := all (λ a, a < b) as

notation as `≺`:50 b := @all_lt as b 

def all_lt_le (asb : as ≺ b) (bc : b <= c): as ≺ c
:= all.map (λ {a} ab, lt_of_lt_of_le ab bc) asb

def all_lt_lt (asb : as ≺ b) (bc : b < c): as ≺ c
:= all.map (λ {a} ab, trans ab bc) asb

def lt_greater_left (b c : ℕ): b < greater b c := nat.lt_succ_of_le (le_max_left b c)
def lt_greater_right (b c : ℕ): c < greater b c := nat.lt_succ_of_le (le_max_right b c)

def outside : list ℕ -> ℕ := list.foldr greater 0

def outside_more : outside as <= outside (a :: as) := begin
  let m : greater a (outside as) = outside (a :: as) , refl,
  refine (trans _ (le_of_eq m)),
  refine (le_of_lt _),
  exact (lt_greater_right a (outside as))
end

def outside_lt : as ≺ outside as := begin 
  introv,
  induction as,
  refine (all.nil _), -- nil
  refine (all.cons _ (all.map _ as_ih)), -- cons
  calc as_hd < greater as_hd (outside as_tl) : lt_greater_left as_hd (outside as_tl)
         ... = outside (as_hd :: as_tl)      : by refl,
  intros a ao,
  exact (lt_of_lt_of_le ao (outside_more as_hd as_tl))          
end

-- permutations
structure perm := (perm: list (ℕ × ℕ))

def swap : ℕ × ℕ → ℕ → ℕ  
| ⟨a, b⟩ c := if a = c then b else if b = c then a else c    

def act (p:perm) (n:ℕ) : ℕ := list.foldr swap n p.1

def injective_act (p:perm): injective (act p) :=
by admit

def surjective_act (p:perm): surjective (act p) :=
by admit

-- bijective (act p)
-- has_left_inverse (act p)
-- has_right_inverse (act p)
-- invertible (act p)

-- TODO: prove that act is a group homomorphism

instance perm_inv : has_inv perm := by 
apply has_inv.mk; intro a; cases a; exact (perm.mk (list.reverse a))

def reverse.injective {A : Type}: injective (@list.reverse A) := 
by admit

def reverse.surjective {A: Type}: surjective (@list.reverse A) := 
by admit

-- messy, requires actual equality! quotient by permutation equivalence?
-- or i can just make my own setoid encoding, blah
instance group_perm : group perm
:= begin
  apply group.mk,
  admit, -- inverse proof
  intros p q; cases p; cases q; exact (perm.mk (p ++ q)), -- cleanup?
  admit, -- associativity of append
  exact (perm.mk []),
  begin intro, admit, end, -- 1*a = a
  admit, -- a *1 = a
  exact perm_inv.1,
end 