{-# LANGUAGE OverloadedStrings #-}

module Examples.Equations.Parser where

{-
  Aim is to parse basic equations like the following examples

    y = -3x + 4
    y = x^2 - 4x + 1
    a = (b / 2) * c
    y = (x + 2)(x - 3)

-}
import           Examples.Equations.Parser.AST

import           Control.Applicative           ((*>), (<$>), (<*), (<*>))

import           Text.Parsec
import           Text.Parsec.Expr
import           Text.Parsec.Language
import           Text.Parsec.Token

type MathParser e = Parsec String () e

exprDef :: LanguageDef ()
exprDef =
  emptyDef
  { opStart = oneOf "^*/+-"
  , opLetter = oneOf "^*/+-"
  , reservedOpNames = ["^", "*", "/", "+", "-"]
  }

TokenParser { parens = m_parens
            , float = m_float
            , integer = m_integer
            , reservedOp = m_reservedOp
            , whiteSpace = m_whiteSpace
            } = makeTokenParser exprDef

exprParser :: MathParser Expression
exprParser = buildExpressionParser table atom <?> "expression"

table =
  [ [Infix (m_reservedOp "^" >> return EExp) AssocLeft]
  , [ Infix (m_reservedOp "" >> return EMult) AssocLeft
    , Infix (m_reservedOp "*" >> return EMult) AssocLeft
    , Infix (m_reservedOp "/" >> return EDiv) AssocLeft
    ]
  , [ Infix (m_reservedOp "+" >> return EAdd) AssocLeft
    , Infix (m_reservedOp "-" >> return ESub) AssocLeft
    ]
  ]

atom :: MathParser Expression
atom =
  m_parens exprParser <|> fmap ENum (m_intToFloat <|> m_float) <|>
  fmap EVar (letter <* m_whiteSpace)
  where
    m_intToFloat = fmap fromIntegral m_integer

eqParser :: MathParser Equation
eqParser = cleanSurroundings $ Equation <$> exprParser <* equals <*> exprParser
  where
    cleanSurroundings p = m_whiteSpace *> p <* eof
    equals = m_whiteSpace *> char '=' *> m_whiteSpace

readEquation :: String -> Either ParseError Equation
readEquation input = parse eqParser "equation" input
