open function
open sigma

section bijections
  universes u₁ u₂
  variables {α : Type u₁} {β : Type u₂}

  -- promote some existing propositions to classes
  attribute [class] injective
  attribute [class] surjective
  attribute [class] bijective
  attribute [class] has_left_inverse
  attribute [class] has_right_inverse

  instance mk_bijective {f: α → β} [fi : injective f] [fs: surjective f]: bijective f 
    := (| fi, fs |)

  instance bijective_injective {f: α → β} [bf: bijective f]: injective f := bf.1
  instance bijective_surjective {f: α → β} [bf: bijective f]: surjective f := bf.2

  instance has_left_inverse_injective {f : α → β} [h : has_left_inverse f]: injective f :=
    injective_of_has_left_inverse h

  instance has_right_inverse_surjective {f : α → β} [h : has_right_inverse f]: surjective f :=
    surjective_of_has_right_inverse h
end bijections

structure {u₁ u₂} invertible {α : Type u₁} {β : Type u₂} (f : α → β) := 
  (invf : β → α)
  (invinj : injective invf) 
  (linv : left_inverse invf f)
attribute [class] invertible

section inverses
  -- computable inverses

  -- end goal: uniqueness of inverses
  universes u₁ u₂
  variables {α : Type u₁} {β : Type u₂}

  def inverse (f : α → β) [iF : invertible f]: β → α := invertible.invf iF

  def invertible_left_inverse (f : α → β) [iF : invertible f]: left_inverse (inverse f) f :=
    invertible.linv iF

  def invertible_right_inverse (f : α → β) [iF : invertible f]: right_inverse (inverse f) f :=
    right_inverse_of_injective_of_left_inverse (invertible.invinj iF) (invertible.linv iF)

  def invertible_has_right_inverse (f : α → β) [iF : invertible f]: has_right_inverse f :=
    exists.intro (invertible.invf iF) (invertible_right_inverse f)

  def invertible_has_left_inverse (f : α → β) [iF : invertible f]: has_left_inverse f :=
    exists.intro (invertible.invf iF) (invertible.linv iF)

  instance invertible_injective_inverse {f : α → β} [iF : invertible f]: injective (inverse f) :=
    invertible.invinj iF

  instance invertible_surjective {f : α → β} [iF : invertible f]: surjective f := 
    begin
      destruct (invertible_has_right_inverse f),
      intros g fg b,
      apply exists.intro,
      exact (fg b)
    end

  instance invertible_injective {f: α → β} [iF : invertible f]: injective f :=
    injective_of_has_left_inverse (invertible_has_left_inverse f)

  instance invertible_surjective_inverse {f : α → β} [iF : invertible f]: surjective (inverse f) :=
    begin
      intro a,
      apply exists.intro,
      exact (invertible.linv iF a),
    end

  instance invertible_inverse {f : α → β} [iF : invertible f]: invertible (inverse f) :=
    invertible.mk f invertible_injective (invertible_right_inverse f)

  instance invertible_bijective (f : α → β) [iF : invertible f]: bijective f :=
    (| invertible_injective, invertible_surjective |)

  instance invertible_bijective_inverse (f : α → β) [iF : invertible f]: bijective (inverse f) :=
    (| invertible_injective_inverse, invertible_surjective_inverse |)

  -- def invertible_is_unique {f : α → β} (p q: invertible f) : p = q -- up to funext/propext

end inverses

def {u1 u2 u3} lhs {α : Sort u1} { β : Sort u2 } {γ : Sort u3} (f g : β → α) (h : γ -> β) (fg : f = g) : f ∘ h = g ∘ h := 
  eq.subst fg (@eq.refl (γ → α) (f ∘ h))

def {u1 u2 u3} rhs {α : Sort u1} { β : Sort u2 } {γ : Sort u3} (f : β → α) (g h : γ -> β) (gh : g = h) : f ∘ g = f ∘ h := 
  eq.subst gh (@eq.refl (γ → α) (f ∘ g))

