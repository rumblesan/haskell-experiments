module Experiments.MonadT.ExceptWriterStateT where

import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict

data Token
  = OpenParen
  | CloseParen
  | Letter Char
  deriving (Eq, Show)

data AST
  = Group [AST]
  | Expression [Char]
  deriving (Eq, Show)

type ParserState = State [Token]

type ParserLog = WriterT [String] ParserState

type ParserMonad a = ExceptT String ParserLog a

type Parser = ParserMonad AST

type ParserOutput = Either String AST

charLex :: Char -> Token
charLex c =
  case c of
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
      lift $ tell ["open paren"]
      myParse
    CloseParen:rest -> do
      put rest
      lift $ tell ["close paren"]
      return $ Expression []
    (Letter c):rest -> do
      put rest
      parsed <- myParse
      case parsed of
        Expression chars -> return $ Expression (c : chars)
        Group _          -> throwError "shouldn'g get an AST here"
    [] -> throwError "shouldn't hit this"

parse :: String -> (Either String AST, [String])
parse input = evalState (runWriterT $ runExceptT myParse) (myLex input)
