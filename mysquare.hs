{-# LANGUAGE PatternGuards #-}
-- | Simple picture drawing application.
--   Like MSPaint, but you can only draw squares.
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Data.Maybe (maybe)
import Debug.Trace


main
 = do   let state = State Nothing False Nothing []
        play    (InWindow "Draw" (600, 600) (0,0))
			white 100 state
			makePicture handleEvent stepWorld


-- | The game state.
data State
        = State (Maybe (Float,Float,Float,Float)) Bool (Maybe (Float, Float)) [Picture]

-- | Convert our state to a picture.
makePicture :: State -> Picture
makePicture (State m n r xs)
        = case n of
			False -> Pictures (maybe xs (\(a,b,c,d) -> ((Translate a b $ Line [(((min (abs (c-a)) (abs (d-b)))/2),((min (abs (c-a)) (abs (d-b)))/2)),(((min (abs (c-a)) (abs (d-b)))/2),-((min (abs (c-a)) (abs (d-b)))/2)),(-((min (abs (c-a)) (abs (d-b)))/2),-((min (abs (c-a)) (abs (d-b)))/2)),(-((min (abs (c-a)) (abs (d-b)))/2),((min (abs (c-a)) (abs (d-b)))/2)),(((min (abs (c-a)) (abs (d-b)))/2),((min (abs (c-a)) (abs (d-b)))/2))]):xs)) m)
			True -> case r of
						(Just (u,v)) -> Pictures (maybe xs (\(a,b,c,d) -> (Translate a b $ rotate (degrees a b c d u v ) $ Line [(((min (abs (c-a)) (abs (d-b)))/2),((min (abs (c-a)) (abs (d-b)))/2)),(((min (abs (c-a)) (abs (d-b)))/2),-((min (abs (c-a)) (abs (d-b)))/2)),(-((min (abs (c-a)) (abs (d-b)))/2),-((min (abs (c-a)) (abs (d-b)))/2)),(-((min (abs (c-a)) (abs (d-b)))/2),((min (abs (c-a)) (abs (d-b)))/2)),(((min (abs (c-a)) (abs (d-b)))/2),((min (abs (c-a)) (abs (d-b)))/2))]) : xs) m)


-- | Handle mouse click and motion events.
handleEvent :: Event -> State -> State
handleEvent event state
	        -- If the mouse has moved, then update the edge of square.
        	| EventMotion (x, y)    <- event , State (Just (a,b,c,d)) u v ss  <- state
	        = case u of
				False -> State (Just (a,b,x,y)) False v ss	-- Still not clicked the square
				True -> State (Just (a,b,c,d)) True (Just (x, y)) ss	-- The square has been clicked

    	    -- Start drawing a new square.
	        | EventKey (MouseButton LeftButton) Down _ pt@(x,y) <- event
	        , State Nothing False Nothing ss       <- state
	        = State (Just (x,y,x,y)) False Nothing ss
	        -- Finish drawing a square, and add it to the picture.
	        | EventKey (MouseButton LeftButton) Up _ pt@(x,y)      <- event
	        , State (Just (a,b,c,d)) u v ss    <- state
	        = case u of
					False -> State (Just (a,b,x,y)) True (Just (x,y)) ((Color red (Line [(a-10,b-25),(a,b-20),(a-10,b-10)])):(Color red (Translate a b (Arc 0 270 20))):ss)
					True -> case v of
								Just (a1, b1) -> State Nothing False Nothing ((Translate a b $ rotate (degrees a b c d a1 b1) $ Line [(((min (abs (c-a)) (abs (d-b)))/2),((min (abs (c-a)) (abs (d-b)))/2)),(((min (abs (c-a)) (abs (d-b)))/2),-((min (abs (c-a)) (abs (d-b)))/2)),(-((min (abs (c-a)) (abs (d-b)))/2),-((min (abs (c-a)) (abs (d-b)))/2)),(-((min (abs (c-a)) (abs (d-b)))/2),((min (abs (c-a)) (abs (d-b)))/2)),(((min (abs (c-a)) (abs (d-b)))/2),((min (abs (c-a)) (abs (d-b)))/2))]):(drop 2 ss))

	        | otherwise
	       	= state

degrees :: Float -> Float -> Float -> Float -> Float -> Float -> Float
degrees x1 y1 x2 y2 x3 y3
			= atan((s1-s2)/(1+s1*s2))*(180/pi)
				where
					s2= (y3-y1)/(x3-x1)
					s1= (y2-y1)/(x2-x1)

stepWorld :: Float -> State -> State
stepWorld _ = id


scaleNormalizer :: Float -> Float -> Float
scaleNormalizer x y
		    = sqrt(x*x+y*y)
