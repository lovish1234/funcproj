{-# LANGUAGE PatternGuards #-}
-- | Simple picture drawing application. 
--   Like MSPaint, but you can only draw straight lines.
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Data.Maybe (maybe)
import Debug.Trace
import Data.List


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
        = let
			dr :: [(Float,Float)] -> Picture
			dr l
				| ((length l) <= 1) = Blank
				| ((length l) >=2) = Polygon l
			in
			Pictures (maybe xs (\x -> (Color aquamarine (dr x)) : xs) m)


-- | Handle mouse click and motion events.
handleEvent :: Event -> State -> State
handleEvent event state
       -- If the mouse has moved, then extend the current line.
        | EventMotion (x, y)    <- event
        , State (Just (_:as)) ss    <- state
        = State (Just ((x,y):as)) ss 

        -- Start drawing a new line.
        | EventKey (MouseButton LeftButton) Down _ pt@(x,y) <- event
        , State Nothing ss       <- state
        = State (Just [(x,y),(x,y)]) ss
        
        -- Start drawing a new line.
        | EventKey (MouseButton LeftButton) Down _ pt@(x,y) <- event
        , State (Just (_:as)) ss       <- state
        = State (Just ((x,y):(x,y):as)) ss

        -- Finish drawing the polygon, and the final line to the picture.
        | EventKey (MouseButton RightButton) Down _ pt@(x,y) <- event
        , State (Just a) ss       <- state
        = State Nothing ((Color aquamarine $ Polygon a):ss)

        | otherwise
        = state


stepWorld :: Float -> State -> State
stepWorld _ = id


