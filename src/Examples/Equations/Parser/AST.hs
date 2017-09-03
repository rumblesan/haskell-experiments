module Examples.Equations.Parser.AST where

data Equation =
  Equation Expression
           Expression
  deriving (Show)

data Expression
  = EAdd Expression
         Expression
  | ESub Expression
         Expression
  | EMult Expression
          Expression
  | EDiv Expression
         Expression
  | EExp Expression
         Expression
  | ENum Double
  | EVar Char
  deriving (Eq, Show)
