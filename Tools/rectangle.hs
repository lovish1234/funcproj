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
        = State (Maybe (Float,Float,Float,Float))    -- The current rectangle being drawn on coordinates of a diagonal .
				(Maybe (Float, Float))			-- The point where we clicked the rectangle
				(Maybe (Float, Float))			-- The current coordinates
                [Picture]       -- All the circles drawn previously.

-- | Convert our state to a picture.
makePicture :: State -> Picture
makePicture (State m n r xs)
        = case n of 
			Nothing -> Pictures (maybe xs (\(a,b,p,q) -> (Translate ((a+p)/2) ((b+q)/2) $ Line [((p-a)/2,(b-q)/2),((p-a)/2,(q-b)/2),((a-p)/2,(q-b)/2),((a-p)/2,(b-q)/2),((p-a)/2,(b-q)/2)]) : xs) m)
																				-- "Translate a b Picture" translates a picture to a b
			Just (p1, q1) -> case r of
							Just (u,v) -> Pictures (maybe xs (\(a,b,p,q) -> (Translate ((a+p)/2) ((b+q)/2) $ rotate (degrees ((a+p)/2) ((b+q)/2) p1 q1 u v ) $ 
								Line [((p-a)/2,(b-q)/2),((p-a)/2,(q-b)/2),((a-p)/2,(q-b)/2),((a-p)/2,(b-q)/2),((p-a)/2,(b-q)/2)]) : xs) m)


-- | Handle mouse click and motion events.
handleEvent :: Event -> State -> State
handleEvent event state
        -- If the mouse has moved, then update the edges of rectangle.
        | EventMotion (x, y)    <- event
        , State (Just (a,b,c,d)) u v ss    <- state
        = case v of 
			Nothing -> State (Just (a,b,x,y) ) u v ss	-- Still not clicked the rectangle
			Just (p, q) -> State (Just (a,b,c,d)) u (Just (x, y)) ss	-- The rectangle has been clicked

        -- Start drawing a new rectangle.
        | EventKey (MouseButton LeftButton) Down _ pt@(x,y) <- event
        , State Nothing Nothing Nothing ss       <- state
        = State (Just (x,y,x,y)) Nothing Nothing ss

        -- Finish drawing a rectangle, and add it to the picture.
        | EventKey (MouseButton LeftButton) Up _ pt@(x,y)      <- event
        , State (Just (a,b,p,q)) u v ss    <- state
        = case u of 
			Nothing -> State (Just (a,b,x,y)) (Just (x,y)) (Just (x,y)) 
				((Color red (Line [(((a+x)/2)-10,((b+y)/2)-25),(((a+x)/2),((b+y)/2)-20),(((a+x)/2)-10,((b+y)/2)-10)])):(((Color red (Translate ((a+x)/2) ((b+y)/2) (Arc 0 270 20)))):ss))		-- Just clicked the rectangle
			Just (p1, q1) -> case v of
							Just (a1, b1) -> State Nothing Nothing Nothing 
								((Translate ((a+p)/2) ((b+q)/2) $ rotate (degrees ((a+p)/2) ((b+q)/2) p1 q1 a1 b1) $ 
									Line [((p-a)/2,(b-q)/2),((p-a)/2,(q-b)/2),((a-p)/2,(q-b)/2),((a-p)/2,(b-q)/2),((p-a)/2,(b-q)/2)]):(drop 2 ss))

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
