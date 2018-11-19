module Calculator
  ( calculator
  , evaluate
  ) where

import           Text.Parsec        (char, digit, many1, parse, (<|>))
import           Text.Parsec.String (Parser)

data Expression
  = Plus Expression Expression
  | Minus Expression Expression
  | Times Expression Expression
  | Divide Expression Expression
  | Value Float

calculator :: String -> String
calculator input =
  case parse expression "Calculator" input of
    Left error ->
      show error

    Right formula ->
      show $ evaluate formula

evaluate :: Expression -> Float
evaluate (Plus x y)   = evaluate x + evaluate y
evaluate (Minus x y)  = evaluate x - evaluate y
evaluate (Times x y)  = evaluate x * evaluate y
evaluate (Divide x y) = evaluate x / evaluate y
evaluate (Value x)    = x

expression :: Parser Expression
expression = do
  t <- term
  addition t <|> subtraction t <|> return t
  where
    addition t = do
      char '+'
      e <- expression
      return $ Plus t e
    subtraction t = do
      char '-'
      e <- expression
      return $ Minus t e

term :: Parser Expression
term = do
  f <- factor
  multiplication f <|> division f <|> return f
  where
    multiplication f = do
      char '*'
      t <- term
      return $ Times f t
    division f = do
      char '/'
      t <- term
      return $ Divide f t

factor :: Parser Expression
factor =
  bracket <|> number
  where
    bracket = do
      char '('
      e <- expression
      char ')'
      return e
    number = do
      xs <- many1 digit
      return $ Value $ read xs
