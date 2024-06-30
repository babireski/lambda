module Type where

type Identifier = String
data Type = Application Type Type
          | Constructor Identifier
          | Variable Identifier  
          | Type :→: Type
          deriving Eq
data Typescheme = Universal [Identifier] Type deriving Eq

instance Show Type where
    show :: Type -> String
    show (Constructor γ) = γ
    show (Variable α) = α
    show (Application τ₁ τ₂) = show τ₁ ++ " " ++ show τ₂ 
    show ((τ₁ :→: τ₂) :→: τ₃)  = "(" ++ show τ₁ ++ " → " ++ show τ₂ ++ ")" ++ " → " ++ show τ₃
    show (τ₁ :→: τ₂)  = show τ₁ ++ " → " ++ show τ₂

instance Show Typescheme where
    show :: Typescheme -> String
    show (Universal [] τ) = show τ
    show (Universal (i:r) τ) = "∀" ++ i ++ "." ++ show (Universal r τ)

variables :: [Identifier]
variables = map (: []) ['α' .. 'ω']
