module Expression where

import Control.Applicative (some)
import Text.Parsec
import Text.Parsec.String (Parser)
import Type (Identifier)

data Constant = Boolean Bool | Integer Int deriving Eq
data Expression = Abstraction Identifier Expression
                | Application Expression Expression
                | Constant Constant
                | Constructor Identifier
                | If Expression Expression Expression
                | Let Identifier Expression Expression
                | Variable Identifier
                deriving Eq

instance Show Expression where
    show :: Expression -> String
    show (Variable x) = x
    show (Abstraction x e) = "λ" ++ x ++ "." ++ show e
    show (Application e₁ e₂) = helper₁ e₁ ++ " " ++ helper₂ e₂ where
        helper₁ :: Expression -> String
        helper₁ e@(Application _ _) = show e
        helper₁ e = show e

        helper₂ :: Expression -> String
        helper₂ e@(Abstraction _ _) = "(" ++ show e ++ ")"
        helper₂ e@(Application _ _) = "(" ++ show e ++ ")"
        helper₂ e = show e

variable :: Parser Expression
variable = do
    h <- lower
    t <- many letter
    return (Variable (h : t))

abstraction :: Parser Expression
abstraction = do
    char 'λ'
    h <- lower
    t <- many letter
    char '.'
    e <- expression
    return (Abstraction (h : t) e)

application :: Parser Expression
application = do
    e₁ <- term
    rest <- some (space *> term)
    return (foldl Application e₁ rest)

constructor :: Parser Expression
constructor = do
    h <- upper
    t <- many letter
    return (Constructor (h : t))

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
