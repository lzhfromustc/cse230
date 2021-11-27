-- This style is learnt from https://github.com/jtdaugherty/brick/blob/master/docs/samtay-tutorial.md

import Data.Sequence (Seq(..))
import Linear.V2 (V2(..), _x, _y)


-- Data types for Game, which is a high level abstract of the game, and will be used by the state in Brick

data Game = Game
  { _pieces  :: VecPiece        -- ^ a vector of all pieces currently on the board
  , _cursor   :: Coord        -- ^ location of the Cursor. We need to learn how to use it by other demo programs
  , _winned   :: Bool         -- ^ game over flag
  , _warnned :: Bool           -- ^ a flag indicating whether the game is displaying a warning message (for illegal place)
  , _player :: Int              -- ^ 1 if player1 is about to move; 2 if player2 is about to move
  , _paused :: Bool         -- ^ paused flag. We may not need this. 
  } deriving (Show)

type Coord = V2 Int -- coordinator

type VecPiece = Seq Piece

type Piece = (Int, Coord) -- Int indicates who owns this piece, and Coord indicates the location of the piece


-- Constants. Currently we are using fixed size (10X10) board, and 5 pieces in line to win

height, width, winPieces :: Int
height = 10
width = 10
winPieces = 5

-- Functions. Learnt from the Snake game. 

--TODO:Need to finish

-- A player is free to move the cursor, if no one wins or warnned
moveCursor :: Game -> Game
moveCursor = undefined

-- A player can place a piece on an empty Cell, if no one wins or warnned
-- Some one wins if one's `winPieces` continuous pieces are in line. When this happens, displays winner.
    -- TODO: halt the game when someone wins
placePiece :: Game -> Game
placePiece = undefined

-- A player can rotate 1/4 of the board, if no one wins or warnned
rotateQuarter :: Game -> Game
rotateQuarter = undefined

-- After someone wins, press Esc to exit. We may not need a function for this. We can just call `halt g`

-- The player can't place a piece at a Cell if there is already a piece, if no one wins or warnned. 
    -- When this happens, displays a message
illegalPlace :: Game -> Game
illegalPlace = undefined

-- Only used right after illegalPlace. The player needs to press a key to close the message
closeMsg :: Game -> Game
illegalPlace = undefined

-- Initialize a game with no pieces and player 1 is about to move. We may not need this
initGame :: IO Game
initGame = undefined