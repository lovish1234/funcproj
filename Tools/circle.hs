{-# LANGUAGE PatternGuards #-}
-- | Simple picture drawing application. 
--   Like MSPaint, but you can only draw circles.
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
        = State (Maybe (Float,Float,Float))    -- The current circle being drawn.
                [Picture]       -- All the circles drawn previously.

-- | Convert our state to a picture.
makePicture :: State -> Picture
makePicture (State m xs)
        = Pictures (maybe xs (\(a,b,c) -> (Translate a b (Circle c)) : xs) m)   -- "Circle c" draws a circle of radius c at origin
																				-- "Translate a b Picture" translates a picture to a b


-- | Handle mouse click and motion events.
handleEvent :: Event -> State -> State
handleEvent event state
        -- If the mouse has moved, then update the radius of the circle.
        | EventMotion (x, y)    <- event
        , State (Just (a,b,_)) ss    <- state
        = State (Just (a,b,(sqrt((a-x)*(a-x) + (b-y)*(b-y))))) ss

        -- Start drawing a new circle.
        | EventKey (MouseButton LeftButton) Down _ pt@(x,y) <- event
        , State Nothing ss       <- state
        = State (Just (x,y,0.0)) ss

        -- Finish drawing a circle, and add it to the picture.
        | EventKey (MouseButton LeftButton) Up _ pt@(x,y)      <- event
        , State (Just (a,b,_)) ss    <- state
        = State Nothing ((Translate a b (Circle (sqrt((a-x)*(a-x) + (b-y)*(b-y))))):ss)

        | otherwise
        = state


stepWorld :: Float -> State -> State
stepWorld _ = id


