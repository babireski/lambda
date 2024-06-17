{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use concatMap" #-}
module Substitution where

import Data.List (nub)
import Type (Type, Typescheme)

type Identifier = String
type Substitution = [(Type, Type)]
data Assumption = Assumption Type Identifier

class Substitutable a where
    apply :: Substitution -> a -> a
    free :: a -> [Identifier]

instance Substitutable Type where
    apply :: Substitution -> Type -> Type
    apply s τ = undefined
    free :: Type -> [Identifier]
    free τ = undefined

instance Substitutable Typescheme where
    apply :: Substitution -> Typescheme -> Typescheme
    apply s σ = undefined
    free :: Typescheme -> [Identifier]
    free σ = undefined

instance Substitutable Assumption where
    apply :: Substitution -> Assumption -> Assumption
    apply s a = undefined
    free :: Assumption -> [Identifier]
    free a = undefined

instance Substitutable a => Substitutable [a] where
    apply :: Substitution -> [a] -> [a]
    apply s = map (apply s)
    free :: [a] -> [Identifier]
    free = nub ∘ concat ∘ map free

(∘) :: (b -> c) -> (a -> b) -> a -> c
(∘) = (.)

(↦) :: Type -> Type -> (Type, Type)
(↦) α β = (α, β)

occurs :: Type -> Type -> Bool
occurs α β = undefined

unify :: Type -> Type -> Substitution
unify α β = undefined

compose :: Substitution -> Substitution -> Substitution
compose s₁ s₂ = [(τ₁, apply s₁ τ₂) | (τ₁, τ₂) <- s₂] ++ s₁