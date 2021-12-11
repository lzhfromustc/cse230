module UI where

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
handleEvent game (VtyEvent (V.EvKey key [])) =
  case key of
    -- Move cursor
    V.KUp       -> continue $ moveCursor North 1 game
    V.KDown     -> continue $ moveCursor South 1 game
    V.KLeft     -> continue $ moveCursor West 1 game
    V.KRight    -> continue $ moveCursor East 1 game

    V.KChar 'w' -> continue $ moveCursor North 1 game
    V.KChar 's' -> continue $ moveCursor South 1 game
    V.KChar 'a' -> continue $ moveCursor West 1 game
    V.KChar 'd' -> continue $ moveCursor East 1 game


    
    -- Place a piece and do all the checking
    V.KChar ' ' -> continue $ if (end game) then game else answerCell (player game) . snapshotGame $ game

    -- Reset or End game
    V.KChar 'c' -> halt game
    V.KChar 'r' -> continue . snapshotGame . resetGame $ game

    -- Other: continue
    _           -> continue $ game

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

drawHelp :: Game -> Widget ()
drawHelp game =
  [ "Current Player:  Player "      <> show (player game)
  , "Current Color :  "             <> if (player game) == 0 then "White" else "Black"
  , "==========================================="
  , "Move Cursor   :  ←↓↑→ / wasd"
  , "Place a piece :  `Space`"
  , ""
  , "Rest Game     :  r"
  , "Quit Game     :  c"
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


drawUI :: Game -> [Widget ()]
drawUI game =
  if (gameSolved game) 
    then [ centerLayer $
      (withBorderStyle unicodeBold
      . withBorderStyle unicodeRounded
      . borderWithLabel (str " Winner ")
      . border) $ str ("\n\n                 Player " <> (show winner) <> " wins!          \n\n\n       Press `r` to reset or `c` to exit       \n")
    , drawUIMain game]
    else [drawUIMain game]
    where winner = if (player game) == 0 then 1 else 0

drawUIMain :: Game -> Widget ()
drawUIMain game =
    ( drawHelp game
    -- <=>   drawDebug game
    )
    <+>                drawGrid game 

app :: App Game e ()
app = App
  { appDraw         = \x -> (drawUI x)
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure
  , appAttrMap      = const attributes
  }

main :: IO Game
main = do
  do
      defaultMain app (mkGame demo)
      

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

