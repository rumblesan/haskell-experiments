module Experiments.MonadT.StateParser where

import Control.Monad.State.Strict
import Control.Monad.Trans.Except

data Token = OpenParen | CloseParen | Letter Char deriving (Eq, Show)

data AST = Group [AST] | Expression [Char] deriving (Eq, Show)

type ParserError = Either String
type ParserState = State [Token]
type Parser = ExceptT String ParserState AST

charLex :: Char -> Token
charLex c = case c of
  '(' -> OpenParen
  ')' -> CloseParen
  _   -> Letter c

myLex :: String -> [Token]
myLex text = fmap charLex text


myParse :: Parser
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
        Expression chars -> return $ Expression (c:chars)
        Group _ -> throwE "shouldn'g get an AST here"
    [] -> throwE "shouldn't hit this"

