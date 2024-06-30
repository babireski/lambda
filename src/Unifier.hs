module Unifier where

import Substitution
import Type

class Unifiable a where
    unify :: a -> a -> Substitution

instance Unifiable Type where
    unify :: Type -> Type -> Substitution
    unify τ₁@(Variable α) (Variable β) = [β ↦ τ₁ | α /= β]
    unify (Variable α) τ₂ = if occurs α τ₂ then error ("Type error: unable to unify " ++ α ++ " with " ++ show τ₂) else [α ↦ τ₂]
    unify τ₁ (Variable β) = if occurs β τ₁ then error ("Type error: unable to unify " ++ show τ₁ ++ " with " ++ β) else [β ↦ τ₁]
    unify (τ₁ :→: τ₂) (τ₃ :→: τ₄) =
        let s₁ = unify τ₁ τ₃
            s₂ = unify (apply s₁ τ₂) (apply s₁ τ₄)
        in s₂ · s₁

instance Unifiable Typescheme where
    unify :: Typescheme -> Typescheme -> Substitution
    unify (Universal b₁ τ₁) (Universal b₂ τ₂) = if length b₁ /= length b₂ then error ("Type error: unable to unify " ++ show (Universal b₁ τ₁) ++ " with " ++ show (Universal b₂ τ₂)) else unify τ₁ τ₂

occurs :: Identifier -> Type -> Bool
occurs α (Variable β) = α == β
occurs α (τ₁ :→: τ₂) = occurs α τ₁ || occurs α τ₂
