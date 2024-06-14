module Type where

data Type = Variable String | Type :→: Type | Type :+: Type | Type :×: Type
data Typescheme = Type Type | Universal String Typescheme

instance Show Type where
    show (Variable α) = α
    show (τ₁ :→: τ₂)  = show τ₁ ++ " → " ++ show τ₂
    show (τ₁ :+: τ₂)  = show τ₁ ++ " + " ++ show τ₂
    show (τ₁ :×: τ₂)  = show τ₁ ++ " × " ++ show τ₂

instance Show Typescheme where
    show (Type τ) = show τ
    show (Universal α σ) = "∀" ++ α ++ "." ++ show σ