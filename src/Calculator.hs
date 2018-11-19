module Calculator
  ( calculator
  ) where

import           Control.Applicative (pure, (*>), (<$>), (<*))
import           Text.Parsec         (char, digit, many1, parse, (<|>))
import           Text.Parsec.String  (Parser)

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
  Plus t <$> (char '+' *> expression)
    <|> Minus t <$> (char '-' *> expression)
    <|> pure t

term :: Parser Expression
term = do
  f <- factor
  Times f <$> (char '*' *> term)
    <|> Divide f <$> (char '/' *> term)
    <|> pure f

factor :: Parser Expression
factor =
  (char '(' *> expression <* char ')')
    <|> number
  where
    number = do
      xs <- many1 digit
      pure $ Value . read $ xs
