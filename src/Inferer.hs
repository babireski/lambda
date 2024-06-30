module Inferer where

import Control.Monad.State
import Expression
import Substitution
import Type
import Unifier

type Inferer a = State Int a
type Context = [Assumption]

fresh :: Inferer Type
fresh = do
    n <- get
    put (n + 1)
    return $ Type.Variable (variables !! n)

extend :: Context -> Assumption -> Context
extend context (x₁, τ₁) = (x₁, τ₁) : [(x, τ) | (x, τ) <- context, x /= x₁]

generalize :: Context -> Type -> Typescheme
generalize context τ = Universal [α | α <- free τ, α ∉ free context] τ

instantiate :: Typescheme -> Inferer Type
instantiate (Universal σ τ) = do
    ϕ <- traverse (const fresh) σ
    let s = zip σ ϕ
    return $ apply s τ

infer :: Context -> Expression -> Inferer (Type, Substitution)
infer context (Constant (Boolean b)) = return (Type.Constructor "Boolean", [])
infer context (Constant (Integer n)) = return (Type.Constructor "Integer", [])
infer context (Expression.Variable x) = case lookup x context of
    Nothing -> error ("Type error: unbound variable " ++ x)
    Just τ₁ -> do
        τ₂ <- instantiate τ₁
        return (τ₂, [])
infer context (Expression.Application e₁ e₂) = do
    (τ₁, s₁) <- infer context e₁
    (τ₂, s₂) <- infer (apply s₁ context) e₂
    α <- fresh
    let s₃ = unify (apply s₂ τ₁) (τ₂ :→: α)
    return (apply s₃ α, s₃ · s₂ · s₁)
infer context (Abstraction x e) = do
    α <- fresh
    (τ, s) <- infer (extend context (x, Universal [] α)) e
    return (apply s (α :→: τ), s)
infer context (Expression.Constructor c) = case lookup c context of
    Nothing -> error ("Type error: undefined constructor " ++ c)
    Just τ₁ -> do
        τ₂ <- instantiate τ₁
        return (τ₂, [])
infer context (If e₁ e₂ e₃) = do
    (τ₁, s₁) <- infer context e₁
    (τ₂, s₂) <- infer (apply s₁ context) e₂
    (τ₃, s₃) <- infer (apply (s₂ · s₁) context) e₃
    let s₄ = unify τ₁ (Type.Constructor "Boolean")
        s₅ = unify (apply s₄ τ₂) (apply s₄ τ₃)
    return (apply s₅ τ₃, s₅ · s₄ · s₃ · s₂ · s₁)
infer context (Let x e₁ e₂) = do
    (τ₁, s₁) <- infer context e₁
    (τ₂, s₂) <- infer (apply s₁ (extend context (x, generalize context τ₁))) e₂
    return (τ₂, s₂ · s₁)

format :: Type -> Type
format τ = apply s τ where s = zip (free τ) (map Type.Variable variables)

typing :: Expression -> Type
typing e = format $ fst $ evalState (infer [] e) 0


precontext :: Context
precontext = [
                ("Pair",    Universal ["a", "b"] (Type.Variable "a" :→: (Type.Variable "b" :→: Type.Application (Type.Application (Type.Constructor "Pair") (Type.Variable "a")) (Type.Variable "b")))),
                ("Left",    Universal ["a", "b"] (Type.Variable "a" :→: Type.Application (Type.Application (Type.Constructor "Either") (Type.Variable "a")) (Type.Variable "b"))),
                ("Right",   Universal ["a", "b"] (Type.Variable "b" :→: Type.Application (Type.Application (Type.Constructor "Either") (Type.Variable "a")) (Type.Variable "b"))),
                ("Just",    Universal ["a"] (Type.Variable "a" :→: Type.Application (Type.Constructor "Maybe") (Type.Variable "a"))),
                ("Nothing", Universal ["a"] (Type.Application (Type.Constructor "Maybe") (Type.Variable "a"))),
                ("True",    Universal [] (Type.Constructor "Boolean")),
                ("False",   Universal [] (Type.Constructor "Boolean"))
            ]
