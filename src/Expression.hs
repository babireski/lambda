module Expression where

import Control.Applicative (some)
import Text.Parsec
import Text.Parsec.String (Parser)
import Type (Identifier)

data Expression = Abstraction Identifier Expression 
                | Application Expression Expression
                -- | Case Expression [(P)]
                | Condition Expression Expression Expression
                | Constant Constant
                | Variable Identifier
                | Naming Identifier Expression Expression
                deriving (Eq, Show)

data Constant = Integer Integer | Boolean Bool deriving (Eq, Show)

-- instance Show Expression where
--     show :: Expression -> String
--     show (Variable x) = x
--     show (Abstraction x e) = "λ" ++ x ++ "." ++ show e
--     show (Application e₁ e₂) = helper₁ e₁ ++ " " ++ helper₂ e₂ where
--         helper₁ :: Expression -> String
--         helper₁ e@(Application _ _) = show e
--         helper₁ e = show e

--         helper₂ :: Expression -> String
--         helper₂ e@(Abstraction _ _) = "(" ++ show e ++ ")"
--         helper₂ e@(Application _ _) = "(" ++ show e ++ ")"
--         helper₂ e = show e

variable :: Parser Expression
variable = do 
    x <- some letter
    return (Variable x)

abstraction :: Parser Expression
abstraction = do
    char 'λ'
    x <- some letter
    char '.'
    e <- expression
    return (Abstraction x e)

application :: Parser Expression
application = do
    e₁ <- term
    rest <- some (space *> term)
    return (foldl Application e₁ rest)

naming :: Parser Expression
naming = do
    string "let "
    x <- some letter
    string " = "
    e₁ <- expression
    string " in "
    e₂ <- expression
    return (Naming x e₁ e₂)

condition :: Parser Expression
condition = do
    string "if "
    e₁ <- expression
    string " then "
    e₂ <- expression
    string " else "
    e₃ <- expression
    return (Condition e₁ e₂ e₃)

-- constant :: Parser Expression
-- constant = do
--     return ()

term :: Parser Expression
term = parenthesis <|> abstraction <|> naming <|> variable

expression :: Parser Expression
expression = try application <|> term

parenthesis :: Parser Expression
parenthesis = between (char '(') (char ')') expression

parse :: String -> Expression
parse input = case Text.Parsec.parse (spaces *> expression <* eof) "" input of
    Left e -> error (show e)
    Right λ -> λ
