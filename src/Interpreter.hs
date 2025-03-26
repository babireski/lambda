module Interpreter where

import Data.List (nub, union)
import Expression (Expression (..))
import Type (Identifier)

type Substitution = [(Identifier, Expression)]

(∪) :: Eq a => [a] -> [a] -> [a]
(∪) = union

(∈) :: (Foldable t, Eq a) => a -> t a -> Bool
(∈) = elem

(∉) :: (Foldable t, Eq a) => a -> t a -> Bool
(∉) x l = not (x ∈ l)

reduce :: Expression -> Expression
reduce (Abstraction x e)   = undefined
reduce (Application e₁ e₂) = undefined
reduce (Variable x)        = undefined

free :: Expression -> [Identifier]
free (Abstraction x e)   = filter (∉ [x]) (free e)
free (Application e₁ e₂) = nub (free e₁ ∪ free e₂)
free (Variable x)        = [x]

substitute :: Substitution -> Expression -> Expression
substitute s (Abstraction x e)   = undefined
substitute s (Application e₁ e₂) = Application (substitute s e₁) (substitute s e₂)
substitute s (Variable x)        = maybe (Variable x) id (lookup x s)
