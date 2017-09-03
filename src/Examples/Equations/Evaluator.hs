module Examples.Equations.Evaluator where

{-

  Evaluate an expression when potentially given variable values

-}
import           Examples.Equations.Parser.AST

import qualified Data.Map.Lazy                 as M

type ExpressionAST = Expression -> Expression -> Expression

type MathFunc = Double -> Double -> Double

varSub :: M.Map Char Expression -> Expression -> Expression
varSub subs (EAdd l r)  = EAdd (varSub subs l) (varSub subs r)
varSub subs (ESub l r)  = ESub (varSub subs l) (varSub subs r)
varSub subs (EMult l r) = EMult (varSub subs l) (varSub subs r)
varSub subs (EDiv l r)  = EDiv (varSub subs l) (varSub subs r)
varSub subs (EExp l r)  = EExp (varSub subs l) (varSub subs r)
varSub subs v@(EVar c)  = M.findWithDefault v c subs
varSub _ num            = num

reduceExpr :: Expression -> Expression
reduceExpr n@(ENum _)  = n
reduceExpr v@(EVar _)  = v
reduceExpr (EAdd l r)  = evalAST EAdd (+) l r
reduceExpr (ESub l r)  = evalAST ESub (-) l r
reduceExpr (EMult l r) = evalAST EMult (*) l r
reduceExpr (EDiv l r)  = evalAST EDiv (/) l r
reduceExpr (EExp l r)  = evalAST EExp (**) l r

evalAST :: ExpressionAST -> MathFunc -> Expression -> Expression -> Expression
evalAST exprAST f l r =
  case (reduceExpr l, reduceExpr r) of
    (ENum v1, ENum v2)    -> ENum $ f v1 v2
    (leftExpr, rightExpr) -> exprAST leftExpr rightExpr

reduce :: M.Map Char Expression -> Equation -> Equation
reduce subs (Equation l r) =
  let f = reduceExpr . varSub subs
  in Equation (f l) (f r)
