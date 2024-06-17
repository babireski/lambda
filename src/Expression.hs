module Expression where

import Control.Applicative (some)
import Text.Parsec
import Text.Parsec.String

data Expression = Abstraction String Expression | Application Expression Expression | Variable String

instance Show Expression where
    show :: Expression -> String
    show (Variable x) = x
    show (Abstraction x e) = "λ" ++ x ++ "." ++ show e
    show (Application e₁ e₂) = show e₁ ++ " " ++ show e₂

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
    space
    e₂ <- term
    return (Application e₁ e₂)

term :: Parser Expression
term = parenthesis <|> abstraction <|> variable

expression :: Parser Expression
expression = try application <|> term

parenthesis :: Parser Expression
parenthesis = between (char '(') (char ')') expression

parse :: String -> Expression
parse input = case Text.Parsec.parse (spaces *> expression <* eof) "" input of
    Left e -> error (show e)
    Right λ -> λ
