module Unifier where

import Substitution
import Type

class Unifiable a where
    unify :: a -> a -> Maybe Substitution

instance Unifiable Type where
    unify :: Type -> Type -> Maybe Substitution
    unify (Variable α) τ₂@(Variable β) = Just [α ↦ τ₂ | α /= β]
    unify (Variable α) τ₂ = if occurs α τ₂ then Nothing else Just [α ↦ τ₂]
    unify τ₁ (Variable β) = if occurs β τ₁ then Nothing else Just [β ↦ τ₁]
    unify (τ₁ :→: τ₂) (τ₃ :→: τ₄) = unify τ₁ τ₃ <> unify τ₂ τ₄
    unify (τ₁ :+: τ₂) (τ₃ :+: τ₄) = unify τ₁ τ₃ <> unify τ₂ τ₄
    unify (τ₁ :×: τ₂) (τ₃ :×: τ₄) = unify τ₁ τ₃ <> unify τ₂ τ₄

occurs :: Identifier -> Type -> Bool
occurs α (Variable β) = α == β
occurs α (τ₁ :→: τ₂) = occurs α τ₁ || occurs α τ₂
occurs α (τ₁ :+: τ₂) = occurs α τ₁ || occurs α τ₂
occurs α (τ₁ :×: τ₂) = occurs α τ₁ || occurs α τ₂
