module Inferer where

data Context = [(String, Typescheme)]

generalize :: Context -> Type -> Typescheme
generalize context type = undefined

infer :: Context -> Expression -> (Substitution, Typescheme)
infer context (Variable x) = case lookup x context of Nothing -> error "Type error: unbound variable"; Just τ -> ([], τ)
infer context (Application e₁ e₂) =  
infer context (Abstraction x e) = 