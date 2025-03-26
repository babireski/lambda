module Substitution where

import Data.List (nub, union, intersect)
import Type (Identifier, Type (..), Typescheme(..))

type Assumption = (Identifier, Typescheme)
type Substitution = [(Identifier, Type)]

class Substitutable a where
    apply :: Substitution -> a -> a
    free :: a -> [Identifier]

instance Substitutable Type where
    apply :: Substitution -> Type -> Type
    apply s (α :+: β) = apply s α :+: apply s β
    apply s (α :×: β) = apply s α :×: apply s β
    apply s (α :→: β) = apply s α :→: apply s β
    apply s (Variable α) = case lookup α s of
        Nothing -> Variable α
        Just σ -> σ
    free :: Type -> [Identifier]
    free (α :+: β) = free α ∪ free β
    free (α :×: β) = free α ∪ free β
    free (α :→: β) = free α ∪ free β
    free (Variable α) = [α]

instance Substitutable Typescheme where
    apply :: Substitution -> Typescheme -> Typescheme
    apply s (Universal α τ) = if null (α ∩ free s) then Universal α (apply [(x, t) | (x, t) <- s, x ∉ α] τ) else error "Type error: unable to substitute"
    free :: Typescheme -> [Identifier]
    free (Universal α τ) = filter (∉ α) (free τ)

instance Substitutable (Identifier, Type) where
    apply :: Substitution -> (Identifier, Type) -> (Identifier, Type)
    apply s (α, τ) = (α, apply s τ)
    free :: (Identifier, Type) -> [Identifier]
    free (α, τ) = free τ

instance Substitutable (Identifier, Typescheme) where
    apply :: Substitution -> Assumption -> Assumption
    apply s (α, τ) = (α, apply s τ)
    free :: Assumption -> [Identifier]
    free (α, τ) = free τ

instance Substitutable a => Substitutable [a] where
    apply :: Substitution -> [a] -> [a]
    apply s = map (apply s)
    free :: [a] -> [Identifier]
    free = nub ∘ concat ∘ map free

(∪) :: Eq a => [a] -> [a] -> [a]
(∪) = union

(∩) :: Eq a => [a] -> [a] -> [a]
(∩) = intersect

(∈) :: (Foldable t, Eq a) => a -> t a -> Bool
(∈) = elem

(∉) :: (Foldable t, Eq a) => a -> t a -> Bool
(∉) x l = not (x ∈ l)

(∘) :: (b -> c) -> (a -> b) -> a -> c
(∘) = (.)

(↦) :: Substitutable a => Identifier -> a -> (Identifier, a)
(↦) x t = (x, t)

infixr 4 ·
(·) :: Substitution -> Substitution -> Substitution
(·) s₁ s₂ = apply s₁ s₂ ∪ s₁
