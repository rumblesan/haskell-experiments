module Experiments.StateParser where

import           Control.Monad.State.Lazy

data Token
  = OpenParen
  | CloseParen
  | Letter Char
  deriving (Eq, Show)

data AST
  = Group [AST]
  | Expression [Char]
  deriving (Eq, Show)

type ParserState = State [Token] AST

charLex :: Char -> Token
charLex c =
  case c of
    '(' -> OpenParen
    ')' -> CloseParen
    _   -> Letter c

myLex :: String -> [Token]
myLex text = fmap charLex text

myParse :: ParserState
myParse = do
  tokens <- get
  case tokens of
    OpenParen:rest -> do
      put rest
      myParse
    CloseParen:rest -> do
      put rest
      return $ Expression []
    (Letter c):rest -> do
      put rest
      parsed <- myParse
      case parsed of
        Expression chars -> return $ Expression (c : chars)
        Group _          -> error "shouldn'g get an AST here"
    [] -> error "shouldn't hit this"
