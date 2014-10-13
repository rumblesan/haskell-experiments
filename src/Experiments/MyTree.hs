module Experiments.MyTree where

data MyTree value = MyTreeEmpty | MyTreeLeaf value | MyTreeTree value (MyTree value) (MyTree value) deriving (Show, Eq)

instance Functor MyTree where

  fmap _ MyTreeEmpty = MyTreeEmpty
  fmap f (MyTreeLeaf v) = MyTreeLeaf $ f v
  fmap f (MyTreeTree v left right) = MyTreeTree (f v) (fmap f left) (fmap f right)



addValue :: (Ord value) => MyTree value -> value -> MyTree value
addValue MyTreeEmpty newValue = MyTreeLeaf newValue
addValue l@(MyTreeLeaf oldValue) newValue | newValue < oldValue = MyTreeTree oldValue (MyTreeLeaf newValue) MyTreeEmpty
                                          | newValue > oldValue = MyTreeTree oldValue MyTreeEmpty (MyTreeLeaf newValue)
                                          | otherwise           = l
addValue t@(MyTreeTree oldValue left right) newValue | newValue < oldValue = MyTreeTree oldValue (addValue left newValue) right
                                                     | newValue > oldValue = MyTreeTree oldValue left (addValue right newValue)
                                                     | otherwise           = t

isEmpty :: MyTree value -> Bool
isEmpty tree = case tree of
  MyTreeEmpty -> True
  _ -> False



