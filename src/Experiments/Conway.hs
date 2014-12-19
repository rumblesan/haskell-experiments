module Experiments.Conway where

import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Monoid
import qualified Data.Map.Strict as M
import qualified Data.List as L

data Cell = Alive | Dead deriving (Eq, Ord)
type Index = Int
type Coordinate = (Index, Index)
type Board = Map Coordinate Cell

data GameData = GameData {
  gameWidth  :: Index,
  gameHeight :: Index,
  gameBoard  :: Board
} deriving (Eq)

type Game v = State GameData v

instance Show GameData where
  show (GameData width height board) =
    let
      heading = mconcat ["Board ", show width, " * ", show height]
      cells = do
        x <- [0..(height - 1)]
        let rowCells = fmap (\y -> show $ fromMaybe Dead (M.lookup (x, y) board)) [0..(width - 1)]
        return $ mconcat rowCells
      shownCells = mconcat $ L.intersperse "\n" cells
    in
      mconcat [heading, "\n", shownCells]


instance Show Cell where
  show Alive = "X"
  show Dead  = "o"

createBoard :: Int -> Int -> Board
createBoard width height = M.fromList $ do
      w <- [0..(width - 1)]
      h <- [0..(height - 1)]
      let c = (w, h)
      return (c, Dead)

createGame :: Int -> Int -> GameData
createGame width height = GameData width height (createBoard width height)

getCoordinates :: Game [Coordinate]
getCoordinates = gets (\g ->
    do
      x <- [0..((gameWidth  g) - 1)]
      y <- [0..((gameHeight g) - 1)]
      return (x, y)
  )

calcNeighbours :: Coordinate -> [Coordinate]
calcNeighbours (x, y) = do
  (xOffset, yOffset) <- [
    (-1, -1), (-1, 0), (-1, 1),
    (0,  -1),          (0,  1),
    (1,  -1), (1,  0), (1,  1)]
  return (x + xOffset, y + yOffset)

setCellValue :: Coordinate -> Cell -> Game ()
setCellValue coord value = modify (\g -> g { gameBoard = M.insert coord value (gameBoard g) })

getCellValue :: Coordinate -> Game Cell
getCellValue coord = gets $ (\g -> fromMaybe Dead (M.lookup coord (gameBoard g)))

getNeighbours :: Coordinate -> Game [Cell]
getNeighbours coordinate = mapM getCellValue (calcNeighbours coordinate)

calculateNewCellValue :: Coordinate -> Game Cell
calculateNewCellValue coord = do
    current <- getCellValue coord
    neighbours <- getNeighbours coord
    return $ evolutionRules current neighbours

evolutionRules :: Cell -> [Cell] -> Cell
evolutionRules current neighbours =
  let livingNeighbours = L.length $ L.filter ( == Alive) neighbours
  in case current of
    Dead | livingNeighbours == 3 -> Alive
    Dead -> Dead
    Alive | livingNeighbours < 2 -> Dead
    Alive | livingNeighbours > 3 -> Dead
    Alive -> Alive

simulateBoard :: Game ()
simulateBoard = do
  coordinates <- getCoordinates
  newCells <- mapM (\c -> do
      newValue <- calculateNewCellValue c
      return (c, newValue)
    ) coordinates
  modify (\g -> g { gameBoard = M.fromList newCells })

simulateBoardNTimes :: Int -> Game ()
simulateBoardNTimes times = replicateM_ times simulateBoard

