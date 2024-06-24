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
infer context (Expression.Variable x) = case lookup x context of 
    Nothing -> error ("Type error: unbound variable " ++ x)
    Just τ₁ -> do
        τ₂ <- instantiate τ₁
        return (τ₂, [])
infer context (Application e₁ e₂) = do
    (τ₁, s₁) <- infer context e₁
    (τ₂, s₂) <- infer (apply s₁ context) e₂
    α <- fresh
    let s₃ = case unify (apply s₂ τ₁) (τ₂ :→: α) of Nothing -> error ("Type error: unable to unify " ++ show (apply s₂ τ₁) ++ " with " ++ show (τ₂ :→: α)); Just s -> s
    return (apply s₃ α, s₃ · s₂ · s₁)
infer context (Abstraction x e) = do
    α <- fresh
    (τ, s) <- infer (extend context (x, Universal [] α)) e
    return (apply s (α :→: τ), s)

format :: Type -> Type
format τ = apply s τ where s = zip (free τ) (map Type.Variable variables)

typing :: Expression -> Type
typing e = format $ fst $ evalState (infer [] e) 0
