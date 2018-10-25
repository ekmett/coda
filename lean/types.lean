inductive ty : Type
| i : ty
| arr : ty -> ty -> ty

inductive ctx : Type
| n : ctx
| s : ctx -> ty -> ctx

notation Γ >> σ := ctx.s Γ σ 

inductive var : ctx -> ty -> Type
| z : ∀ (Γ : ctx) (σ : ty), var (Γ >> σ) σ
| s : ∀ (Γ : ctx) (σ : ty), var Γ σ → var (Γ >> σ) σ

inductive tm : ctx -> ty -> Type
| vr : ∀ (Γ : ctx) (σ : ty), var Γ σ → tm Γ σ 
| ap : ∀ (Γ : ctx) (σ τ : ty), tm Γ (ty.arr σ τ) -> tm Γ σ -> tm Γ τ
| lm : ∀ (Γ : ctx) (σ τ : ty), tm (Γ >> σ) τ -> tm Γ (ty.arr σ τ)
