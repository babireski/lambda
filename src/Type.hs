module Type where

type Identifier = String
data Type = Variable Identifier | Type :→: Type | Type :+: Type | Type :×: Type
data Typescheme = Universal [Identifier] Type

instance Show Type where
    show :: Type -> String
    show (Variable α) = α
    show ((τ₁ :→: τ₂) :→: τ₃)  = "(" ++ show τ₁ ++ " → " ++ show τ₂ ++ ")" ++ " → " ++ show τ₂
    show (τ₁ :→: τ₂)  = show τ₁ ++ " → " ++ show τ₂
    show (τ₁ :+: τ₂)  = show τ₁ ++ " + " ++ show τ₂
    show (τ₁ :×: τ₂)  = show τ₁ ++ " × " ++ show τ₂

instance Show Typescheme where
    show :: Typescheme -> String
    show (Universal [] τ) = show τ
    show (Universal (i:r) τ) = "∀" ++ i ++ "." ++ show (Universal r τ)

variables :: [Identifier]
variables = map (: []) ['a' .. 'z']
-- variables = map (: []) ['α' .. 'ω']

(→) :: Typescheme -> Typescheme -> Typescheme
(→) (Universal b₁ τ₁) (Universal b₂ τ₂) = Universal (b₁ <> b₂) (τ₁ :→: τ₁) -- Treat common bound variables