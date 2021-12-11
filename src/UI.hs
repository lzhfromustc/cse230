module UI where

import FileIO
import Game

import Brick
import Brick.Widgets.Border (border, borderWithLabel, hBorderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode, unicodeBold, unicodeRounded)
import Brick.Widgets.Center (center, centerLayer)
import Data.List (intersperse)
import Data.List.Split (chunksOf)
import qualified Graphics.Vty as V
import Lens.Micro

styleCursor, styleCellGiven, styleCellInput, styleCellNote :: AttrName
styleWhite, styleBlack :: AttrName
styleSolved, styleUnsolved :: AttrName
styleCursor    = attrName "styleCursor"
styleCellGiven = attrName "styleCellGiven"
styleCellInput = attrName "styleCellInput"
-- define color attributes for white and black 
styleWhite = attrName "styleWhite"
styleBlack = attrName "styleBlack"
styleCellNote  = attrName "styleCellNote"
styleSolved    = attrName "styleSolved"
styleUnsolved  = attrName "styleUnsolved"

attributes :: AttrMap
attributes = attrMap V.defAttr
  [ (styleCursor    , bg V.brightBlack)
  , (styleCellGiven , V.defAttr)
  , (styleCellInput , fg V.blue)
  , (styleWhite , fg V.white)
  , (styleBlack , fg V.black)
  , (styleCellNote  , fg V.yellow)
  , (styleSolved    , fg V.green)
  , (styleUnsolved  , fg V.red)
  ]

handleEvent :: Game -> BrickEvent () e -> EventM () (Next Game)
handleEvent game (VtyEvent (V.EvKey key [V.MCtrl])) =
  case key of
    -- Quit
    V.KChar 'c' -> halt game
    -- Reset
    V.KChar 'r' -> continue . snapshotGame . resetGame $ game
    -- Other
    _           -> continue game
handleEvent game (VtyEvent (V.EvKey key [])) =
  continue $ case key of
    -- Move by cell
    V.KUp       -> moveCursor North 1 game
    V.KDown     -> moveCursor South 1 game
    V.KLeft     -> moveCursor West 1 game
    V.KRight    -> moveCursor East 1 game

    V.KChar 'w' -> moveCursor North 1 game
    V.KChar 's' -> moveCursor South 1 game
    V.KChar 'a' -> moveCursor West 1 game
    V.KChar 'd' -> moveCursor East 1 game
    
    -- Enter number
    V.KChar ' ' -> answerCell (player game) . snapshotGame $ game
    -- Other
    _           -> game
handleEvent game _ = continue game

highlightCursor :: Game -> [[[[Widget ()]]]] -> [[[[Widget ()]]]]
highlightCursor game widgets =
  widgets & ix bigRow
          . ix bigCol
          . ix smallRow
          . ix smallCol
          %~ withDefAttr styleCursor
  where (x, y) = cursor game
        bigRow   = y `div` 3
        bigCol   = x `div` 3
        smallRow = y `mod` 3
        smallCol = x `mod` 3

drawCell :: Cell -> Widget ()
drawCell cell = center $ case cell of
-- white for player 0, and black for player 1 
  Input 0 -> withAttr styleWhite . str $ "⬤"       -- " ◍\n◍ ◍ ◍\n ◍"
  Input 1 -> withAttr styleBlack . str $ "⬤"
  Empty   -> str " "
  _       -> undefined

drawGrid :: Game -> Widget ()
drawGrid game =
  fmap (`getRegion` game) [0..8]
  & chunksOf 3
  & fmap (fmap (fmap (fmap drawCell)))
  & highlightCursor game
  & fmap (fmap (fmap (intersperse (withBorderStyle unicode vBorder))))
  & fmap (fmap (fmap hBox))
  & fmap (fmap (intersperse (withBorderStyle unicode (hBorderWithLabel (str "─┼───────┼─")))))
  & fmap (fmap vBox)
  & fmap (intersperse (withBorderStyle unicode vBorder))
  & fmap hBox
  & intersperse (withBorderStyle unicode (hBorderWithLabel (str "┼───────────────────────┼")))
  & vBox
  & border
  & withBorderStyle unicodeBold
  & setAvailableSize (73, 37)
  & padRight (Pad 1)

drawHelp :: Widget ()
drawHelp =
  [ "Move Cursor:    ←↓↑→ / wasd"
  , "Place a piece:   `Space`"
  , ""
  , "Rest Game:   ctrl + r"
  , "Quit Game:    ctrl + c"
  ]
  & unlines
  & str
  & padLeftRight 1
  & borderWithLabel (str " Help ")
  & withBorderStyle unicodeBold
  & setAvailableSize (31, 12)

drawDebug :: Game -> Widget ()
drawDebug game =
  [ "Cursor:    (" <> show x <> ", " <> show y <> ")"
  , "Current Player:  " <> show (player game)
  , "Have a winner:    " <> show (gameSolved game)
  ]
  & unlines
  & str
  & padRight Max
  & padLeftRight 1
  & borderWithLabel (str " Debug ")
  & withBorderStyle unicodeBold
  & hLimit 31
  where (x, y) = cursor game

drawSolved :: Game -> Widget ()
drawSolved game
  | completed && solved =
    str "SOLVED" & withAttr styleSolved & commonModifier
  | completed && not solved =
    str "INCORRECT" & withAttr styleUnsolved & commonModifier
  | otherwise = emptyWidget
  where
    completed = gameProgress game == 100
    solved    = gameSolved game
    commonModifier
      = setAvailableSize (31, 3)
      . withBorderStyle unicodeBold
      . border
      . center

drawUI :: Game -> [Widget ()]
drawUI game =
  if (gameSolved game) 
    then [ centerLayer $
      (withBorderStyle unicodeBold
      . withBorderStyle unicodeRounded
      . borderWithLabel (str " Winner ")
      . border) $ str ("\n\n          Player " <> (show winner) <> " wins!          \n\n")
    , drawUIMain game]
    else [drawUIMain game]
    where winner = if (player game) == 0 then 1 else 0

drawUIMain :: Game -> Widget ()
drawUIMain game =
  drawGrid game <+> ( drawHelp
                <=>   drawDebug game
                    )

app :: App Game e ()
app = App
  { appDraw         = \x -> (drawUI x)
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure
  , appAttrMap      = const attributes
  }

main :: IO ()
main = do
  do
      endGame <- defaultMain app (mkGame demo)
      saveGame "autosave.gomoku" endGame

-- main :: IO ()
-- main = do
--   putStr $ unlines
--     [ "GOMOKU"
--     , "  1: Enter 1 to play GOMOKU! "
--     , "  Otherwise: Press any character to quit the demo"
--     ]
--   response <- prompt "> "
--   case head' response of
--     '1' -> do
--       endGame <- defaultMain app (mkGame demo)
--       saveGame "autosave.gomoku" endGame
--     _   -> putStrLn "Quitting..."
--   where head' [] = ' '
--         head' x  = head x

demo :: [Int]
demo = let z = 0 in
  [ z, z, z, z, z, z, z, z, z
  , z, z, z, z, z, z, z, z, z
  , z, z, z, z, z, z, z, z, z
  , z, z, z, z, z, z, z, z, z
  , z, z, z, z, z, z, z, z, z
  , z, z, z, z, z, z, z, z, z
  , z, z, z, z, z, z, z, z, z
  , z, z, z, z, z, z, z, z, z
  , z, z, z, z, z, z, z, z, z
  ]

