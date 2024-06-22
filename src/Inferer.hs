module Inferer where

import Control.Monad.State
import Expression
import Substitution
import Type
import Unifier

import Debug.Trace

type Inferer a = State Int a
type Context = [Assumption]

fresh :: Inferer Typescheme
fresh = do
    n <- get
    put (n + 1)
    return $ Universal [] $ Type.Variable (variables !! n)

generalize :: Context -> Type -> Type
generalize context τ = undefined

instantiate :: Context -> Type -> Type
instantiate context typescheme = undefined

infer :: Context -> Expression -> Inferer (Typescheme, Substitution)
infer context (Expression.Variable x) = case lookup x context of Nothing -> error "Type error: unbound variable"; Just τ -> return (τ, [])
infer context (Application e₁ e₂) = do
    (t₁, s₁) <- infer context e₁
    (t₂, s₂) <- infer (apply s₁ context) e₂
    α <- fresh
    let s₃ = case unify (apply s₂ t₁) (t₂ → α) of Nothing -> error "Type error: unable to unify"; Just s -> s
    return (apply s₃ α, s₃ · s₂ · s₁)
infer context (Abstraction x e) = do
    α <- fresh
    (τ, s) <- infer ((x, α) : context) e
    return (apply s (α → τ), s)

e :: Expression
e = Expression.parse input

i :: Inferer (Typescheme, Substitution)
i = infer [] e

t :: (Typescheme, Substitution)
t = evalState i 0
