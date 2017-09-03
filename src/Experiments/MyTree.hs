module Experiments.MyTree where

data MyTree value
  = MyTreeEmpty
  | MyTreeLeaf value
  | MyTreeTree value
               (MyTree value)
               (MyTree value)
  deriving (Show, Eq)

instance Functor MyTree where
  fmap _ MyTreeEmpty = MyTreeEmpty
  fmap f (MyTreeLeaf v) = MyTreeLeaf $ f v
  fmap f (MyTreeTree v left right) =
    MyTreeTree (f v) (fmap f left) (fmap f right)

add :: (Ord value) => MyTree value -> value -> MyTree value
add MyTreeEmpty newValue = MyTreeLeaf newValue
add l@(MyTreeLeaf oldValue) newValue
  | newValue < oldValue = MyTreeTree oldValue (MyTreeLeaf newValue) MyTreeEmpty
  | newValue > oldValue = MyTreeTree oldValue MyTreeEmpty (MyTreeLeaf newValue)
  | otherwise = l
add t@(MyTreeTree oldValue left right) newValue
  | newValue < oldValue = MyTreeTree oldValue (add left newValue) right
  | newValue > oldValue = MyTreeTree oldValue left (add right newValue)
  | otherwise = t

toList :: MyTree value -> [value]
toList MyTreeEmpty               = []
toList (MyTreeLeaf v)            = [v]
toList (MyTreeTree v left right) = (toList right) ++ [v] ++ (toList left)

isEmpty :: MyTree value -> Bool
isEmpty tree =
  case tree of
    MyTreeEmpty -> True
    _           -> False
