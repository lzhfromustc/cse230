-- Learnt from the Snake game

-- Types

-- Named resources. Not used in the Snake game, but our game may need it if we want to know where is the gamer focused on. 
    -- We may also not use it, if we can implement cursor without it.
type Name = ()

data Cell = Piece | Empty -- The Piece here is probably not the Piece we defined in Main.hs?

app :: App Game Unknown Name -- I don't think our game is driven by event Tick in Snake game
app = App { appDraw = drawUI
          , appChooseCursor = showCursor?
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap?
          }

