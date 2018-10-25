import .invertible
open function

-- mostly just experimenting with more complicated symmetries, etc.
section mobiles

  inductive rose : Type
  | leaf : rose
  | node : (ℕ -> rose) -> rose

  -- mobile equivalence
  inductive mobile : rose -> rose -> Prop
  | leaf : mobile rose.leaf rose.leaf
  | node : Π {f g : ℕ -> rose}, (∀ {n : ℕ}, mobile (f n) (g n)) → mobile (rose.node f) (rose.node g)
  | perm : Π (g : ℕ -> rose) (f : ℕ -> ℕ) [bf : invertible f], mobile (rose.node g) (rose.node (g∘f))

  @[refl] def mobile_refl : ∀ p : rose, mobile p p := 
  @rose.rec (λ x, mobile x x) mobile.leaf (λ _, mobile.node)

  @[symm] def mobile_symm : ∀ p q : rose, mobile p q -> mobile q p :=
    begin
      apply (@mobile.rec (λ x y, mobile y x) mobile.leaf),
      intros f0 g0 _x h,
      apply mobile.node,
      intros n,
      apply h,
      intros g f bf,
      let f' := invertible.invf bf,
      let ri := @invertible_right_inverse ℕ ℕ f bf,
      let go := rhs g (f ∘ f') id (id_of_left_inverse ri),
      let reg := eq.trans (eq.trans (comp.assoc g f f') go) (comp.right_id g),      
      apply (eq.subst reg),
      let gfg := eq.symm (lhs ((g ∘ f) ∘ f') g f reg),
      refine (eq.subst gfg _),
      exact (@mobile.perm (g ∘ f) f' (@invertible_inverse ℕ ℕ f bf))
    end

  --@[trans] def mobile_trans : ∀ p q r : rose, mobile p q → mobile q r -> mobile p r :=
  --  begin admit
  --  end

end mobiles


