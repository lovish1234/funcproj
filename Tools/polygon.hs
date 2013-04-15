{-# LANGUAGE PatternGuards #-}
-- | Simple picture drawing application. 
--   Like MSPaint, but you can only draw straight lines.
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Data.Maybe (maybe)
import Debug.Trace


main 
 = do   let state = State Nothing Nothing []
        play    (InWindow "Draw" (600, 600) (0,0))
                white 100 state
                makePicture handleEvent stepWorld


-- | The game state.
data State      
        = State (Maybe Path)    -- The current line being drawn.
				(Maybe (Float, Float))	-- The first point of the polygon.
                [Picture]       -- All the lines drawn previously.

-- | Convert our state to a picture.
makePicture :: State -> Picture
makePicture (State m n xs)
        = Pictures (maybe xs (\x -> Line x : xs) m)


-- | Handle mouse click and motion events.
handleEvent :: Event -> State -> State
handleEvent event state
        -- If the mouse has moved, then extend the current line.
        | EventMotion (x, y)    <- event
        , State (Just [a,_]) (Just (p,q)) ss    <- state
        = State (Just [a,(x, y)]) (Just (p,q)) ss 

        -- Start drawing a new line.
        | EventKey (MouseButton LeftButton) Down _ pt@(x,y) <- event
        , State Nothing _ ss       <- state
        = State (Just [(x,y),(x,y)]) (Just (x,y)) ss
        
        -- Start drawing a new line.
        | EventKey (MouseButton LeftButton) Down _ pt@(x,y) <- event
        , State (Just [a]) (Just (p,q)) ss       <- state
        = State (Just [a,(x,y)]) (Just (p,q)) ss

        -- Finish drawing the polygon, and the final line to the picture.
        | EventKey (SpecialKey KeyEnter) Down _ pt@(x,y) <- event
        , State (Just [a]) (Just (p,q)) ss       <- state
        = State Nothing Nothing ((Line [a,(p,q)]):ss)

		-- Finish drawing this line.
        | EventKey (MouseButton LeftButton) Up _ pt@(x,y)      <- event
        , State (Just [a,_]) (Just (p,q)) ss    <- state
        = State (Just [(x,y)]) (Just (p,q)) ((Line [a,(x,y)]):ss)

        | otherwise
        = state


stepWorld :: Float -> State -> State
stepWorld _ = id


