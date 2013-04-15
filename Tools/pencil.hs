{-# LANGUAGE PatternGuards #-}
-- | Simple picture drawing application. 
--   Like MSPaint, but you can only draw lines.
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Data.Maybe (maybe)
import Debug.Trace


main 
 = do   let state = State Nothing []
        play    (InWindow "Draw" (600, 600) (0,0))
                white 100 state
                makePicture handleEvent stepWorld


-- | The game state.
data State      
        = State (Maybe Path)    -- The current line being drawn.
                [Picture]       -- All the lines drawn previously.


-- | Convert our state to a picture.
makePicture :: State -> Picture
makePicture (State m xs)
        = Pictures (maybe xs (\x -> Line x : xs) m)


-- | Handle mouse click and motion events.
handleEvent :: Event -> State -> State
handleEvent event state
        -- If the mouse has moved, then extend the current line.
        | EventMotion (x, y)    <- event
        , State (Just ps) ss    <- state
        = State (Just ((x, y):ps)) ss 

        -- Start drawing a new line.
        | EventKey (MouseButton LeftButton) Down _ pt@(x,y) <- event
        , State Nothing ss       <- state
        = State (Just [pt])  ss

        -- Finish drawing a line, and add it to the picture.
        | EventKey (MouseButton LeftButton) Up _ pt@(x,y)      <- event
        , State (Just ps) ss    <- state
        = State Nothing ( Line (pt:ps) : ss)

        | otherwise
        = state


stepWorld :: Float -> State -> State
stepWorld _ = id

