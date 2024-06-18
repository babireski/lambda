module Interpreter where

import Expression
import Substitution

reduce :: Expression -> Expression
reduce (Application (Abstraction x e₁) e₂) = apply [x ↦ e₂] e₁
reduce e = e