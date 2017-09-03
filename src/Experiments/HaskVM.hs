{-# LANGUAGE GADTs #-}

module Experiments.HaskVM where

import           Control.Monad.Operational

data VMInstruction a where
  Push :: Int -> VMInstruction ()
  Pop :: VMInstruction Int
  Add :: VMInstruction ()

data VMState = VMState
  { vmStack :: [Int]
  }

type VMProgram a = Program VMInstruction a

interpret :: VMProgram a -> (VMState -> a)
interpret program = eval $ view program
  where
    eval :: ProgramView VMInstruction a -> (VMState -> a)
    eval (Push value :>>= p) (VMState stack) =
      interpret (p ()) $ VMState (value : stack)
    eval (Pop :>>= p) (VMState (v:stack)) = interpret (p v) $ VMState stack
    eval (Pop :>>= _) (VMState []) = error "not enough items"
    eval (Add :>>= p) (VMState (v1:v2:stack)) =
      interpret (p ()) $ VMState ((v1 + v2) : stack)
    eval (Add :>>= _) (VMState _) = error "not enough items"
    eval (Return a) _ = a

push :: Int -> Program VMInstruction ()
push v = singleton $ Push v

pop :: Program VMInstruction Int
pop = singleton Pop

add :: Program VMInstruction ()
add = singleton Add
