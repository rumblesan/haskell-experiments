{-# LANGUAGE GADTs #-}

module Experiments.Applicative.CheapParser where

import Control.Applicative
import qualified Data.Map.Strict as M

data ArgParser a where
  ArgParserBranch :: ArgParser (a -> b) -> ArgParser a -> ArgParser b
  ArgParserLeaf   :: a -> ArgParser a
  ArgParserFlag   :: String -> (String -> a) -> ArgParser a

instance Functor ArgParser where
  fmap f (ArgParserLeaf a)      = ArgParserLeaf (f a)
  fmap f (ArgParserFlag flag g) = ArgParserFlag flag (f . g)
  fmap f (ArgParserBranch l r)   = ArgParserBranch (fmap (f .) l) r

  -- also works
  -- doesn't (necessarily) conform to functor laws
  -- but does mean you don't have to change the functor
  -- definition as new type constructors are added
  --
  -- fmap f expr = ArgParserBranch (ArgParserLeaf f) expr

instance Applicative ArgParser where

  pure v = ArgParserLeaf v
  f <*> expr = ArgParserBranch f expr


runParser :: M.Map String String -> ArgParser a -> Maybe a
runParser _ (ArgParserLeaf v) = Just v
runParser args (ArgParserFlag flagName flagFunc) = fmap flagFunc (M.lookup flagName args)
runParser args (ArgParserBranch l r) = (runParser args l) <*> (runParser args r)

strFlag :: String -> ArgParser String
strFlag flagName = ArgParserFlag flagName id

intFlag :: String -> ArgParser Int
intFlag flagName = fmap read (strFlag flagName)

getArgParserFlags :: ArgParser a -> [String]
getArgParserFlags (ArgParserLeaf _) = []
getArgParserFlags (ArgParserFlag flagName _) = [flagName]
getArgParserFlags (ArgParserBranch l r) = (getArgParserFlags l) ++ (getArgParserFlags r)



data ZooKeeperConfig = ZooKeeperConfig { host :: String, port :: Int } deriving Show
data SomeOtherConfig = SomeOtherConfig { foo :: String,  bar :: Int  } deriving Show

data FullConfig = FullConfig { zk :: ZooKeeperConfig, other :: SomeOtherConfig } deriving Show

zkParser :: ArgParser ZooKeeperConfig
zkParser = ZooKeeperConfig <$> (strFlag "-h") <*> (intFlag "-p")

otherParser :: ArgParser SomeOtherConfig
otherParser = SomeOtherConfig <$> (strFlag "-f") <*> (intFlag "-b")

fullParser :: ArgParser FullConfig
fullParser = FullConfig <$> zkParser <*> otherParser

