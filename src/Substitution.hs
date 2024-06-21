module Substitution where

import Data.List (nub, union)
import Expression
import Type

type Substitution a = [(Identifier, a)]
data Assumption = Type ::: Identifier

class Substitutable a where
    substitute :: Substitution a -> Identifier -> a
    apply :: Substitution a -> a -> a
    free :: a -> [Identifier]

instance Substitutable Expression where
    substitute :: Substitution Expression -> Identifier -> Expression
    substitute s x = case lookup x s of Nothing -> Expression.Variable x; Just y -> y

    apply :: Substitution Expression -> Expression -> Expression
    apply s (Expression.Variable x) = substitute s x
    apply s (Application e₁ e₂) = Application (apply s e₁) (apply s e₂)
    apply s (Abstraction x₁ e₁) = Abstraction x₁ (apply [(x₂, e₂) | (x₂, e₂) <- s, x₂ /= x₁] e₁) -- Testar se x₁ não aparece livre em e₂

    free :: Expression -> [Identifier]
    free (Expression.Variable x) = [x]
    free (Abstraction x e) = [y | y <- free e, y /= x]
    free (Application e₁ e₂) = free e₁ ∪ free e₂

-- instance Substitutable Type where
--     apply :: Substitution -> Type -> Type
--     apply s (α :+: β) = apply s α :+: apply s β
--     apply s (α :×: β) = apply s α :×: apply s β
--     apply s (α :→: β) = apply s α :→: apply s β
--     apply s τ = case lookup τ s of Nothing -> τ; Just σ -> σ
--     free :: Type -> [Identifier]
--     free (α :+: β) = free α ∪ free β
--     free (α :×: β) = free α ∪ free β
--     free (α :→: β) = free α ∪ free β
--     free τ = [τ]

-- instance Substitutable Typescheme where
--     apply :: Substitution Typescheme -> Typescheme -> Typescheme
--     apply s σ = undefined
--     free :: Typescheme -> [Identifier]
--     free σ = undefined

-- instance Substitutable Assumption where
--     apply :: Substitution -> Assumption -> Assumption
--     apply s (i ::: τ) = i ::: apply s τ
--     free :: Assumption -> [Identifier]
--     free (i ::: τ) = free τ

-- instance Substitutable a => Substitutable [a] where
--     apply :: Substitution a -> [a] -> [a]
--     apply s = map (apply s)
--     free :: [a] -> [Identifier]
--     free = nub ∘ concat ∘ map free

(∪) :: Eq a => [a] -> [a] -> [a]
(∪) = union

(∘) :: (b -> c) -> (a -> b) -> a -> c
(∘) = (.)

-- (↦) :: Substitutable a => Identifier -> a -> (Identifier, a)
-- (↦) x t = (x, t)

(↦) :: Identifier -> a -> (Identifier, a)
(↦) x t = (x, t)

-- unify :: Type -> Type -> Maybe (Substitution Type)
-- unify α β = undefined

compose :: Substitutable a => Substitution a -> Substitution a -> Substitution a
compose s₁ s₂ = [(τ₁, apply s₁ τ₂) | (τ₁, τ₂) <- s₂] ++ s₁
