module Inferer where

import Expression (Expression (Abstraction, Application, Variable))
import Substitution (Substitution)
import Type (Type, Typescheme)

type Context = [(String, Typescheme)]

-- fresh :: State Integer String
-- fresh = do
--     n <- get
--     put (n + 1)
--     return $ "t" <> show n

generalize :: Context -> Type -> Typescheme
generalize context τ = undefined

instantiate :: Context -> Typescheme -> Type
instantiate context typescheme = undefined

infer :: Context -> Expression -> (Substitution, Typescheme)
infer context (Expression.Variable x) = case lookup x context of Nothing -> error "Type error: unbound variable"; Just τ -> ([], τ)
infer context (Application e₁ e₂) = undefined
infer context (Abstraction x e) = undefined