module Expression where

data Expression = Abstraction String Expression | Application Expression Expression | Variable String

instance Show Expression where
    show (Variable x) = x
    show (Abstraction x e) = "λ" ++ x ++ "." ++ show e
    show (Application e₁ e₂) = show e₁ ++ show e₂