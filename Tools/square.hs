{-# LANGUAGE PatternGuards #-}
-- | Simple picture drawing application. 
--   Like MSPaint, but you can only draw circles.
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Data.Maybe (maybe)
import Debug.Trace


main 
 = do   let state = State Nothing Nothing Nothing []
        play    (InWindow "Draw" (600, 600) (0,0))
                white 100 state
                makePicture handleEvent stepWorld


-- | The game state.
data State      
        = State (Maybe (Float,Float,Float))    -- The current square being drawn with center and edge length.
				(Maybe (Float, Float))			-- The point where we clicked the square
				(Maybe (Float, Float))			-- The current coordinates
                [Picture]       -- All the circles drawn previously.

-- | Convert our state to a picture.
makePicture :: State -> Picture
makePicture (State m n r xs)
        = case n of 
			Nothing -> Pictures (maybe xs (\(a,b,c) -> (Translate a b $ Line [(c/2,c/2),(c/2,-c/2),(-c/2,-c/2),(-c/2,c/2),(c/2,c/2)]) : xs) m)
																				-- "Translate a b Picture" translates a picture to a b
			Just (p, q) -> case r of
							Just (u,v) -> Pictures (maybe xs (\(a,b,c) -> (Translate a b $ rotate (degrees a b p q u v ) $ Line [(c/2,c/2),(c/2,-c/2),(-c/2,-c/2),(-c/2,c/2),(c/2,c/2)]) : xs) m)


-- | Handle mouse click and motion events.
handleEvent :: Event -> State -> State
handleEvent event state
        -- If the mouse has moved, then update the edge of square.
        | EventMotion (x, y)    <- event
        , State (Just (a,b,c)) u v ss    <- state
        = case v of 
			Nothing -> State (Just (a,b,sqrt(abs(x-a)*abs(x-a) + abs(y-b)*abs(y-b)) )) u v ss	-- Still not clicked the square
			Just (p, q) -> State (Just (a,b,c)) u (Just (x, y)) ss	-- The square has been clicked

        -- Start drawing a new square.
        | EventKey (MouseButton LeftButton) Down _ pt@(x,y) <- event
        , State Nothing Nothing Nothing ss       <- state
        = State (Just (x,y,0.0)) Nothing Nothing ss

        -- Finish drawing a square, and add it to the picture.
        | EventKey (MouseButton LeftButton) Up _ pt@(x,y)      <- event
        , State (Just (a,b,c)) u v ss    <- state
        = case u of 
			Nothing -> State (Just (a,b,sqrt(abs(x-a)*abs(x-a) + abs(y-b)*abs(y-b)) )) (Just (x,y)) (Just (x,y)) 
				((Color red (Line [(a-10,b-25),(a,b-20),(a-10,b-10)])):(((Color red (Translate a b (Arc 0 270 20)))):ss))		-- Just clicked the square
			Just (p, q) -> case v of
							Just (a1, b1) -> State Nothing Nothing Nothing 
								((Translate a b $ rotate (degrees a b p q a1 b1) $ Line [(c/2,c/2),(c/2,-c/2),(-c/2,-c/2),(-c/2,c/2),(c/2,c/2)]):(drop 2 ss))

        | otherwise
        = state

degrees :: Float -> Float -> Float -> Float -> Float -> Float -> Float
degrees x_cen y_cen l1x l1y l2x l2y
		-- The angle (in degrees) between two lines l1 and l2, connected by (x_cen, y_cen)
		= let
			s1=(l1y-y_cen)/(l1x-x_cen)
			s2=(l2y-y_cen)/(l2x-x_cen)
		   in
		    atan((s1-s2)/(1+s1*s2))*180/pi

stepWorld :: Float -> State -> State
stepWorld _ = id


scaleNormalizer :: Float -> Float -> Float
scaleNormalizer x y
    = sqrt(x*x+y*y)
