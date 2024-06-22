module Inferer where

import Control.Monad.State
import Expression
import Substitution
import Type
import Unifier

type Inferer a = State Integer a
type Context = [(Identifier, Type)]

fresh :: Inferer Type
fresh = do
    n <- get
    put (n + 1)
    return $ Type.Variable (variables !! n)

generalize :: Context -> Type -> Type
generalize context τ = undefined

instantiate :: Context -> Type -> Type
instantiate context typescheme = undefined

infer :: Context -> Expression -> Inferer (Type, Substitution)
infer context (Expression.Variable x) = case lookup x context of Nothing -> error "Type error: unbound variable"; Just τ -> return (τ, [])
infer context (Application e₁ e₂) = do
    (t₁, s₁) <- infer context e₁
    (t₂, s₂) <- infer (apply s₁ context) e₂
    t₃ <- fresh
    let s₃ = case unify (apply s₂ t₁) (t₂ :→: t₃) of Nothing -> error "Type error: unable to unify"; Just s -> s
    return (apply s₃ t₃, compose s₃ $ compose s₂ s₁)
infer context (Abstraction x e) = do
    α <- fresh
    infer ((x, α) : context) e
