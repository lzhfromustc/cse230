module FileIO
  ( loadGame
  , saveGame
  , prompt
  ) where

import Game

import System.IO (hFlush, stdout)

exportGame :: Game -> String
exportGame = show . grid

importGame :: String -> Game
importGame = (\g -> Game (4, 4) 0 g Nothing) . read

loadGame :: FilePath -> IO Game
loadGame filename = importGame <$> readFile filename

saveGame :: FilePath -> Game -> IO ()
saveGame filename game = writeFile filename (exportGame game)

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

