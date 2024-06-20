module Expression where

import Control.Applicative (some)
import Text.Parsec
import Text.Parsec.String (Parser)

data Expression = Abstraction String Expression | Application Expression Expression | Variable String deriving Eq

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

input :: String
input = "(λw.λy.λx.y (w y x)) ((λa.λb.λc.b (a b c)) (λs.λz.z))"
