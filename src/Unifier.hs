module Unifier where

import Substitution
import Type

class Unifiable a where
    unify :: a -> a -> Maybe Substitution

instance Unifiable Type where
    unify :: Type -> Type -> Maybe Substitution
    unify τ₁@(Variable α) (Variable β) = Just [β ↦ τ₁ | α /= β]
    unify (Variable α) τ₂ = if occurs α τ₂ then Nothing else Just [α ↦ τ₂]
    unify τ₁ (Variable β) = if occurs β τ₁ then Nothing else Just [β ↦ τ₁]
    unify (τ₁ :→: τ₂) (τ₃ :→: τ₄) = do
        s₁ <- unify τ₁ τ₃
        s₂ <- unify (apply s₁ τ₂) (apply s₁ τ₄)
        return (s₂ · s₁)
    unify (τ₁ :+: τ₂) (τ₃ :+: τ₄) = do
        s₁ <- unify τ₁ τ₃
        s₂ <- unify (apply s₁ τ₂) (apply s₁ τ₄)
        return (s₂ · s₁)
    unify (τ₁ :×: τ₂) (τ₃ :×: τ₄) = do
        s₁ <- unify τ₁ τ₃
        s₂ <- unify (apply s₁ τ₂) (apply s₁ τ₄)
        return (s₂ · s₁)

instance Unifiable Typescheme where
    unify :: Typescheme -> Typescheme -> Maybe Substitution
    unify (Universal b₁ τ₁) (Universal b₂ τ₂) = if length b₁ /= length b₂ then Nothing else unify τ₁ τ₂

occurs :: Identifier -> Type -> Bool
occurs α (Variable β) = α == β
occurs α (τ₁ :→: τ₂) = occurs α τ₁ || occurs α τ₂
occurs α (τ₁ :+: τ₂) = occurs α τ₁ || occurs α τ₂
occurs α (τ₁ :×: τ₂) = occurs α τ₁ || occurs α τ₂
