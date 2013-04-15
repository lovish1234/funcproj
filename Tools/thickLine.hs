{-# LANGUAGE PatternGuards #-}
-- | Simple picture drawing application. 
--   Like MSPaint, but you can only draw straight lines.
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
        = Pictures (maybe xs (\x -> (Polygon (retPoints x)) : xs) m)


-- | Handle mouse click and motion events.
handleEvent :: Event -> State -> State
handleEvent event state
        -- If the mouse has moved, then extend the current line.
        | EventMotion (x, y)    <- event
        , State (Just [a,_]) ss    <- state
        = State (Just [a,(x, y)]) ss 

        -- Start drawing a new line.
        | EventKey (MouseButton LeftButton) Down _ pt@(x,y) <- event
        , State Nothing ss       <- state
        = State (Just [(x,y),(x,y)]) ss

        -- Finish drawing a line, and add it to the picture.
        | EventKey (MouseButton LeftButton) Up _ pt@(x,y)      <- event
        , State (Just [(a,b),_]) ss    <- state
        = State Nothing $ (Polygon (retPoints [(a, b), (x, y)])):ss

        | otherwise
        = state

retPoints :: [(Float,Float)] -> [(Float,Float)]
retPoints h
        -- The end points of the thick line.
        = case h of 
                [(a, b), (x, y)] -> let sinterm = (y-b)/(sqrt $ (y-b)*(y-b) + (x-a)*(x-a))
                                        costerm = (x-a)/(sqrt $ (y-b)*(y-b) + (x-a)*(x-a)) 
                                     in
                                      [ ( a - 20*sinterm/2, b + 20*costerm/2),
                                        ( a + 20*sinterm/2, b - 20*costerm/2),
                                        ( x + 20*sinterm/2, y - 20*costerm/2),
                                        ( x - 20*sinterm/2, y + 20*costerm/2)]

stepWorld :: Float -> State -> State
stepWorld _ = id


