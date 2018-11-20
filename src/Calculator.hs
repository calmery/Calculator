module Calculator
  ( calculator
  ) where

import           Text.Parsec          (eof, parse, (<|>))
import           Text.Parsec.Expr     (Assoc (AssocLeft), Operator (Infix),
                                       buildExpressionParser)
import           Text.Parsec.Language (emptyDef)
import           Text.Parsec.String   (Parser)
import           Text.Parsec.Token    (TokenParser, makeTokenParser,
                                       reservedOpNames)
import qualified Text.Parsec.Token    as Token

data Expression
  = Plus Expression Expression
  | Minus Expression Expression
  | Times Expression Expression
  | Divide Expression Expression
  | Value Float

calculator :: String -> String
calculator input =
  case parse parser "Calculator" input of
    Left error ->
      show error

    Right formula ->
      show $ evaluate formula
  where
    parser = do
      whiteSpace
      x <- expression
      eof
      return x

evaluate :: Expression -> Float
evaluate (Plus x y)   = evaluate x + evaluate y
evaluate (Minus x y)  = evaluate x - evaluate y
evaluate (Times x y)  = evaluate x * evaluate y
evaluate (Divide x y) = evaluate x / evaluate y
evaluate (Value x)    = x

lexer :: TokenParser ()
lexer = makeTokenParser $ emptyDef
  { reservedOpNames = ["*","/","+","-"]
  }

whiteSpace = Token.whiteSpace lexer
integer    = Token.integer lexer
parens     = Token.parens lexer
reservedOp = Token.reservedOp lexer

expression :: Parser Expression
expression = buildExpressionParser table term
  where
    table =
      [ [ Infix (reservedOp "*" >> return Times) AssocLeft
        , Infix (reservedOp "/" >> return Divide) AssocLeft
        ]
      , [ Infix (reservedOp "+" >> return Plus) AssocLeft
        , Infix (reservedOp "-" >> return Minus) AssocLeft
        ]
      ]

term :: Parser Expression
term = parens expression <|> number
  where
    number = do
      xs <- integer
      return $ Value . read $ show xs
