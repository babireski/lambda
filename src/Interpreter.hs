module Interpreter where

import Expression
import Substitution

-- Not working properly
-- reduce :: Expression -> Expression
-- reduce (Abstraction x e) = Abstraction x (reduce e)
-- reduce (Application (Abstraction x e₁) e₂) = apply [x ↦ e₂] e₁
-- reduce (Application e₁ e₂) = Application e₁ (reduce e₂)
-- reduce e = e

normalize :: Expression -> Expression
normalize (Abstraction x e) = Abstraction x (normalize e)
normalize (Application (Abstraction x e₁) e₂) = normalize (apply [x ↦ e₂] e₁)
normalize (Application e₁ e₂) = let e₃ = normalize e₁ in case e₃ of
    Abstraction _ _ -> normalize (Application e₃ e₂)
    _ -> Application e₃ (normalize e₂)
normalize e = e
