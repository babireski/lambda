module Unifier where

import Substitution
import Type

class Unifiable a where
    unify :: a -> a -> Maybe (Substitution a)

instance Unifiable Type where
    unify :: Type -> Type -> Maybe (Substitution Type)
    unify (Variable α) τ₂@(Variable β) = Just [α ↦ τ₂ | α /= β]
    unify τ₁@(Variable α) τ₂ = if occurs τ₁ τ₂ then Nothing else Just [α ↦ τ₂]
    unify τ₁ τ₂@(Variable β) = if occurs τ₂ τ₁ then Nothing else Just [β ↦ τ₁]
    unify (τ₁ :→: τ₂) (τ₃ :→: τ₄) = unify τ₁ τ₃ <> unify τ₂ τ₄
    unify (τ₁ :+: τ₂) (τ₃ :+: τ₄) = unify τ₁ τ₃ <> unify τ₂ τ₄
    unify (τ₁ :×: τ₂) (τ₃ :×: τ₄) = unify τ₁ τ₃ <> unify τ₂ τ₄

instance Unifiable Typescheme where
    unify :: Typescheme -> Typescheme -> Maybe (Substitution Typescheme)
    unify = undefined

occurs :: Type -> Type -> Bool
occurs α β = undefined