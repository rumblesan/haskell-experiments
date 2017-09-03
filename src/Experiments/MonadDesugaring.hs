module Experiments.MonadDesugaring where

-- Use
-- `ghc -c src/Experiments/MonadDesugaring.hs -ddump-ds`
-- to see the desugared version
desugar :: Int -> Maybe Int
desugar i = do
  j <- errIfOdd i
  k <- add1 j
  return (k + j)

{-

  Cleaned up

desugar :: Int -> Maybe Int
desugar =
  (i :: Int) ->
    (errIfOdd i) >>= (
      (j :: Int) ->
        (add1 j) >>= (
          (k :: Int) ->
            return (+ k j)
        )
    )


  Raw output

desugar :: Int -> Maybe Int
[LclIdX, Str=DmdType]
desugar =
  \ (i_alH :: Int) ->
    >>=
      @ Maybe
      GHC.Base.$fMonadMaybe
      @ Int
      @ Int
      (errIfOdd i_alH)
      (\ (j_alI :: Int) ->
         >>=
           @ Maybe
           GHC.Base.$fMonadMaybe
           @ Int
           @ Int
           (add1 j_alI)
           (\ (k_alJ :: Int) ->
              return
                @ Maybe
                GHC.Base.$fMonadMaybe
                @ Int
                (+ @ Int GHC.Num.$fNumInt k_alJ j_alI)))

-}
errIfOdd :: Int -> Maybe Int
errIfOdd i =
  if (i `mod` 2) == 1
    then Nothing
    else Just i

add1 :: Int -> Maybe Int
add1 i = Just (i + 1)
