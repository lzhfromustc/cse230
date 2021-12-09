{-# LANGUAGE LambdaCase #-}

module Game
  ( Cell(..)
  , Row
  , Grid
  , Game(..)
  , Direction(..)
  , mkGame
  , moveCursor
  , answerCell
  -- , toggleNoteCell
  -- , eraseCell
  , snapshotGame
  , resetGame
  , gameProgress
  , gameSolved
  , getRegion
  ) where

import Data.Function ((&))
import Data.List (isInfixOf)
import Data.List.Split (chunksOf)
import Lens.Micro (ix, (%~))

data Cell
  = Given Int
  | Input Int
  | Note [Int]
  | Empty
  deriving (Eq, Read, Show)

type Row = [Cell]

type Grid = [Row]

data Game = Game
  { cursor :: (Int, Int)
  , grid :: Grid
  , previous :: Maybe Game
  } deriving (Read, Show)

data Direction
  = North
  | South
  | East
  | West
  deriving (Read, Show)

mkGame :: [Int] -> Game
mkGame xs = Game
  { cursor = (4, 4)
  , grid = chunksOf 9 $ mkCell <$> xs
  , previous = Nothing
  }
  where mkCell 0 = Empty
        mkCell n = Given n

moveCursor :: Direction -> Int -> Game -> Game
moveCursor direction distance game =
  (\c -> game { cursor = c }) $ case direction of
    North -> (x, wrap (y - distance))
    South -> (x, wrap (y + distance))
    East  -> (wrap (x + distance), y)
    West  -> (wrap (x - distance), y)
  where
    (x, y) = cursor game
    wrap n
      | n >= 9    = n - 9
      | n < 0     = n + 9
      | otherwise = n

-- TODO: Remove need for lenses
transformCell :: (Cell -> Cell) -> Game -> Game
transformCell f game = game { grid = grid game & ix y . ix x %~ f }
  where (x, y) = cursor game

answerCell :: Int -> Game -> Game
answerCell number = transformCell $ \case
  Given n -> Given n
  _       -> Input number

-- toggleNoteCell :: Int -> Game -> Game
-- toggleNoteCell number = transformCell $ \case
--   Given n -> Given n
--   Note ns
--     | ns == [number]   -> Empty
--     | number `elem` ns -> Note (filter (/= number) ns)
--     | otherwise        -> Note (number : ns)
--   _       -> Note [number]

-- eraseCell :: Game -> Game
-- eraseCell = transformCell $ \case
--   Given n -> Given n
--   _       -> Empty

snapshotGame :: Game -> Game
snapshotGame game
  | currentGrid /= lastGrid = game { previous = Just game }
  | otherwise               = game
  where currentGrid = Just $ grid game
        lastGrid    = grid <$> previous game

resetGame :: Game -> Game
resetGame game = game { grid = fmap (fmap f) (grid game) }
  where f = \case
          Given n -> Given n
          _       -> Empty

gameProgress :: Game -> Int
gameProgress game = round ((completed / total :: Float) * 100)
  where
    cells     = concat $ grid game
    completed = fromIntegral $ length $ filter hasValue cells
    total     = fromIntegral $ length cells
    hasValue  = \case
      Given _ -> True
      Input _ -> True
      _       -> False

gameSolved :: Game -> Bool
gameSolved game = rowsSolved || columnsSolved || diagonalSolved
  where
    rowsSolved    = solved $ grid game
    columnsSolved = solved $ getColumns game
    diagonalSolved = solved $ getDiagonalFlat game
    solved = any listCellHas5InRow

listCellHas5InRow :: [Cell] -> Bool
listCellHas5InRow list = listMaybeIntHas5InRow (listCell2listInt list)

listMaybeIntHas5InRow :: [Maybe Int] -> Bool
listMaybeIntHas5InRow [] = False
listMaybeIntHas5InRow xs = (isInfixOf [Just 1, Just 1, Just 1, Just 1, Just 1] xs) || (isInfixOf [Just 0, Just 0, Just 0, Just 0, Just 0] xs)

listCell2listInt :: [Cell] -> [Maybe Int]
listCell2listInt [] = []
listCell2listInt (c:rest) = [cell2MaybeInt c] ++ listCell2listInt rest 
  where
    cell2MaybeInt cell = case cell of
      Given n -> Just n
      Input n -> Just n
      _       -> Nothing

getColumns :: Game -> [[Cell]]
getColumns game =
  [[grid game !! row !! column | row <- [0..8]] | column <- [0..8]]

getDiagonal :: Int -> Game -> [[Cell]]
getDiagonal b game =
  -- diagonal lines `y = -x + b` where b is from 0 to 8
  [[grid game !! row !! (b - row) | row <- [0..b]] ]             ++     

  -- diagonal lines `y = -x + b` where b is from 8 to 16. Will have a duplicated line `y = -x + 8`, but this should be fine
  [[grid game !! (8 - (b - row)) !! (8 - row) | row <- [0..b]] ] ++     

  -- diagonal lines `y = x + b` where b is from -8 to 0
  [[grid game !! (8 - row) !! (b - row) | row <- [0..b]] ]       ++  

  -- diagonal lines `y = x + b` where b is from 0 to 8. Will have a duplicated line `y = x`, but this should be fine
  [[grid game !! (b - row) !! (8 - row) | row <- [0..b]] ]            


getRegion :: Int -> Game -> [[Cell]]
getRegion number game =
  [[grid game !! row !! col | col <- [x..x+2]] | row <- [y..y+2]]
  where x = (number `mod` 3) * 3
        y = number - (number `mod` 3)

getDiagonalFlat :: Game -> [[Cell]]
getDiagonalFlat game = [concat $ getDiagonal x game | x <- [0..8]]