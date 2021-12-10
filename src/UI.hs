-- -- Learnt from the Snake game

-- -- Types

-- -- Named resources. Not used in the Snake game, but our game may need it if we want to know where is the gamer focused on. 
--     -- We may also not use it, if we can implement cursor without it.
-- type Name = ()

-- data Cell = Piece | Empty -- The Piece here is probably not the Piece we defined in Main.hs?

-- app :: App Game Unknown Name -- I don't think our game is driven by event Tick in Snake game
-- app = App { appDraw = drawUI
--           , appChooseCursor = showCursor?
--           , appHandleEvent = handleEvent
--           , appStartEvent = return
--           , appAttrMap = const theMap?
--           }

module UI where

import FileIO
import Game

import Brick
import Brick.Widgets.Border (border, borderWithLabel, hBorderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode, unicodeBold)
import Brick.Widgets.Center (center)
import Data.List (intersperse)
import Data.List.Split (chunksOf)
import qualified Graphics.Vty as V
import Lens.Micro

styleCursor, styleCellGiven, styleCellInput, styleCellNote :: AttrName
styleCellWhite, styleCellBlack :: AttrName
styleSolved, styleUnsolved :: AttrName
styleCursor    = attrName "styleCursor"
styleCellGiven = attrName "styleCellGiven"
styleCellInput = attrName "styleCellInput"
styleCellWhite = attrName "styleCellWhite"
styleCellBlack = attrName "styleCellBlack"
styleCellNote  = attrName "styleCellNote"
styleSolved    = attrName "styleSolved"
styleUnsolved  = attrName "styleUnsolved"

attributes :: AttrMap
attributes = attrMap V.defAttr
  [ (styleCursor    , bg V.brightBlack)
  , (styleCellGiven , V.defAttr)
  , (styleCellInput , fg V.blue)
  , (styleCellWhite , fg V.white)
  , (styleCellBlack , fg V.black)
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
handleEvent game (VtyEvent (V.EvKey key [V.MShift])) =
  continue $ case key of
    V.KUp    -> moveCursor North 3 game
    V.KDown  -> moveCursor South 3 game
    V.KLeft  -> moveCursor West 3 game
    V.KRight -> moveCursor East 3 game
    _        -> game
handleEvent game (VtyEvent (V.EvKey key [])) =
  continue $ case key of
    -- Move by cell
    V.KUp       -> moveCursor North 1 game
    V.KDown     -> moveCursor South 1 game
    V.KLeft     -> moveCursor West 1 game
    V.KRight    -> moveCursor East 1 game
    V.KChar 'k' -> moveCursor North 1 game
    V.KChar 'j' -> moveCursor South 1 game
    V.KChar 'h' -> moveCursor West 1 game
    V.KChar 'l' -> moveCursor East 1 game
    V.KChar 'w' -> moveCursor North 1 game
    V.KChar 's' -> moveCursor South 1 game
    V.KChar 'a' -> moveCursor West 1 game
    V.KChar 'd' -> moveCursor East 1 game
    -- Move by region
    V.KChar 'K' -> moveCursor North 3 game
    V.KChar 'J' -> moveCursor South 3 game
    V.KChar 'H' -> moveCursor West 3 game
    V.KChar 'L' -> moveCursor East 3 game
    V.KChar 'W' -> moveCursor North 3 game
    V.KChar 'S' -> moveCursor South 3 game
    V.KChar 'A' -> moveCursor West 3 game
    V.KChar 'D' -> moveCursor East 3 game
    -- Enter number
    V.KChar '0' -> answerCell 0 . snapshotGame $ game
    V.KChar '1' -> answerCell 1 . snapshotGame $ game
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
  Given x -> withAttr styleCellGiven . str $ show x
  Input 0 -> withAttr styleCellWhite . str $ show 0
  Input 1 -> withAttr styleCellBlack . str $ show 1
  Input x -> withAttr styleCellInput . str $ show x
  Note xs -> fmap str xs'
          & chunksOf 3
          & fmap hBox
          & vBox
          & withAttr styleCellNote
    where xs' = fmap f [1..9]
          f x = if x `elem` xs then show x else " "
  Empty   -> str " "

drawGrid :: Game -> Widget ()
drawGrid game =
  fmap (`getRegion` game) [0..8]
  & chunksOf 3
  & fmap (fmap (fmap (fmap drawCell)))
  & highlightCursor game
  & fmap (fmap (fmap (intersperse (withBorderStyle unicode vBorder))))
  & fmap (fmap (fmap hBox))
  & fmap (fmap (intersperse (withBorderStyle unicode (hBorderWithLabel (str "┼───────┼")))))
  & fmap (fmap vBox)
  & fmap (intersperse (withBorderStyle unicodeBold vBorder))
  & fmap hBox
  & intersperse (withBorderStyle unicodeBold (hBorderWithLabel (str "╋━━━━━━━━━━━━━━━━━━━━━━━╋")))
  & vBox
  & border
  & withBorderStyle unicodeBold
  & setAvailableSize (73, 37)
  & padRight (Pad 1)

drawHelp :: Widget ()
drawHelp =
  [ "move:    ←↓↑→ / wasd / hjkl"
  , "reset:   ctrl + r"
  , "quit:    ctrl + c"
  ]
  & unlines
  & str
  & padLeftRight 1
  & borderWithLabel (str " Help ")
  & withBorderStyle unicodeBold
  & setAvailableSize (31, 12)

drawDebug :: Game -> Widget ()
drawDebug game =
  [ "cursor:    (" <> show x <> ", " <> show y <> ")"
  -- , "progress:  " <> show (gameProgress game) <> "%"
  , "solved:    " <> show (gameSolved game)
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

drawUI :: Game -> Widget ()
drawUI game =
  drawGrid game <+> ( drawHelp
                <=>   drawDebug game
                -- <=>   drawSolved game
                    )

app :: App Game e ()
app = App
  { appDraw         = \x -> [drawUI x]
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure
  , appAttrMap      = const attributes
  }

main :: IO ()
main = do
  putStr $ unlines
    [ "GOMOKU"
    , "  1: Enter 1 to play GOMOKU! "
    , "  Otherwise: Press any character to quit the demo"
    ]
  response <- prompt "> "
  case head' response of
    '1' -> do
      endGame <- defaultMain app (mkGame demo)
      saveGame "autosave.gomoku" endGame
    _   -> putStrLn "Quitting..."
  where head' [] = ' '
        head' x  = head x

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

