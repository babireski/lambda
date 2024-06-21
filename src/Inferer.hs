module Inferer where

import Control.Monad.State
import Expression (Expression (Abstraction, Application, Variable))
import Substitution (Substitution, Substitutable (apply), compose)
import Type (Identifier, Type (Variable, (:→:)), Typescheme (Universal))
import Unifier

type Inferer a = State Integer a
type Context = [(Identifier, Type)]

fresh :: Inferer Type
fresh = do
    n <- get
    put (n + 1)
    return $ Type.Variable $ "t" <> show n

generalize :: Context -> Type -> Typescheme
generalize context τ = undefined

instantiate :: Context -> Typescheme -> Type
instantiate context typescheme = undefined

infer :: Context -> Expression -> Inferer (Substitution Type, Type)
infer context (Expression.Variable x) = case lookup x context of Nothing -> error "Type error: unbound variable"; Just τ -> return ([], τ)
infer context (Application e₁ e₂) = do
    (s₁, t₁) <- infer context e₁
    (s₂, t₂) <- infer (apply s₁ context) e₂
    t₃ <- fresh
    let s₃ = unify (apply s₂ t₁) (t₂ :→: t₃)
    return (compose s₃ $ compose s₂ s₁, apply s₃ t₃)
infer context (Abstraction x e) = do
    α <- fresh
    infer ((x, α) : context) e