module Substitution where

import Data.List (nub, union)
import Type (Type, Typescheme)

type Identifier = String
type Substitution = [(Type, Type)]
data Assumption = Type ::: Identifier

class Substitutable a where
    apply :: Substitution -> a -> a
    free :: a -> [Identifier]

instance Substitutable Type where
    apply :: Substitution -> Type -> Type
    apply s (α :+: β) = (apply s α) :+: (apply s β)
    apply s (α :×: β) = (apply s α) :×: (apply s β)
    apply s (α :→: β) = (apply s α) :→: (apply s β)
    apply s τ = case lookup τ s of Nothing -> τ; Just σ -> σ 
    free :: Type -> [Identifier]
    free (α :+: β) = (free α) ∪ (free β) 
    free (α :×: β) = (free α) ∪ (free β)
    free (α :→: β) = (free α) ∪ (free β)
    free τ = [τ]

instance Substitutable Typescheme where
    apply :: Substitution -> Typescheme -> Typescheme
    apply s σ = undefined
    free :: Typescheme -> [Identifier]
    free σ = undefined

instance Substitutable Assumption where
    apply :: Substitution -> Assumption -> Assumption
    apply s (i ::: τ) = i ::: (apply s τ)
    free :: Assumption -> [Identifier]
    free (i ::: τ) = free τ

instance Substitutable a => Substitutable [a] where
    apply :: Substitution -> [a] -> [a]
    apply s = map (apply s)
    free :: [a] -> [Identifier]
    free = nub ∘ concat ∘ map free

(∪) :: Eq a => [a] -> [a] -> [a]
(∪) = union

(∘) :: (b -> c) -> (a -> b) -> a -> c
(∘) = (.)

(↦) :: Type -> Type -> (Type, Type)
(↦) α β = (α, β)

occurs :: Type -> Type -> Bool
occurs α β = undefined

unify :: Type -> Type -> Maybe Substitution
unify α β = undefined

compose :: Substitution -> Substitution -> Substitution
compose s₁ s₂ = [(τ₁, apply s₁ τ₂) | (τ₁, τ₂) <- s₂] ++ s₁