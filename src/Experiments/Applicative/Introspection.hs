{-# LANGUAGE GADTs #-}

module Experiments.Applicative.Introspection where

import           Control.Applicative

data GAplTree a where
  GAplBranch :: GAplTree (a -> b) -> GAplTree a -> GAplTree b
  GAplLeaf :: a -> GAplTree a
  GAplLabel :: String -> GAplTree a -> GAplTree a

instance Functor GAplTree where
  fmap f (GAplLeaf a)       = GAplLeaf (f a)
  fmap f (GAplLabel s tree) = GAplLabel s (fmap f tree)
  fmap f (GAplBranch l r)   = GAplBranch (fmap (f .) l) r
  -- also works
  -- doesn't (necessarily) conform to functor laws
  -- but does mean you don't have to change the functor
  -- definition as new type constructors are added
  --
  -- fmap f expr = GAplBranch (GAplLeaf f) expr

instance Applicative GAplTree where
  pure v = GAplLeaf v
  f <*> expr = GAplBranch f expr

get :: GAplTree a -> a
get (GAplLeaf v)       = v
get (GAplLabel _ tree) = get tree
get (GAplBranch l r)   = (get l) (get r)

(<?>) :: GAplTree a -> String -> GAplTree a
tree <?> label = GAplLabel label tree

getLabels :: GAplTree a -> [String]
getLabels (GAplLeaf _)           = []
getLabels (GAplLabel label tree) = label : (getLabels tree)
getLabels (GAplBranch l r)       = (getLabels l) ++ (getLabels r)

example :: GAplTree Int
example =
  mathFunc <$> ((GAplLeaf 3) <?> "this is 3") <*> ((GAplLeaf 2) <?> "this is 2") <*>
  ((GAplLeaf 1) <?> "this is 1")
  where
    mathFunc a b c = a + b + c
