module Type where

data Type = Variable String | Type :→: Type | Type :+: Type | Type :×: Type

instance Show Type where
    show (Variable α) = α
    show (τ₁ :→: τ₂)  = show τ₁ ++ " → " ++ show τ₂
    show (τ₁ :+: τ₂)  = show τ₁ ++ " + " ++ show τ₂
    show (τ₁ :×: τ₂)  = show τ₁ ++ " × " ++ show τ₂