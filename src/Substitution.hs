module Substitution where

import Type

type Substitution = [(Type, Type)]

(↦) :: Type -> Type -> (Type, Type)
(↦) α β = (α, β)

occurs :: Type -> Type -> Bool
occurs α β = undefined

unify :: Type -> Type -> Substitution
unify α β = undefined

apply :: Substitution -> Type -> Type
apply substitution type = undefined

compose :: Substitution -> Substitution -> Substitution
compose s₁ s₂ = [(τ₁, apply s₁ τ₂) | (τ₁, τ₂) <- s₂] ++ s₁