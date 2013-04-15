{-# LANGUAGE PatternGuards #-}
-- | Simple picture drawing application. 
--   Like MSPaint, but you can only draw circles.
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Data.Maybe (maybe)
import Debug.Trace
import Graphics.Gloss.Data.Color

main 
 = do   let state = State Nothing True []
        play    (InWindow "Draw" (600, 600) (0,0))
                white 100 state
                makePicture handleEvent stepWorld


-- | The game state.
data State      
        = State (Maybe (Float,		-- Center_X
						Float,		-- Center_Y
						Float,		-- Radius
						Float,		-- coordinate X at which first click was made  	
						Float,		-- coordinate Y at which first click was made  
						Float,		-- current coordinate X
						Float		-- current coordinate Y
						))     
                Bool            -- Toggle click releases
                [Picture]       -- All the circles drawn previously.

-- | Convert our state to a picture.
makePicture :: State -> Picture
makePicture (State m count xs)
        = Pictures (maybe 
				xs 
				(\(a,b,c,x,y,f,g) -> ( Translate a b $ rotate (degrees a b x y f g) $ 
					(scale (abs(x-a)/(scaleNormalizer (x-a) (y-b))) (abs(y-b)/(scaleNormalizer (x-a) (y-b))) (Circle c)) : xs) 
				m
				)

-- | Handle mouse click and motion events.
handleEvent :: Event -> State -> State
handleEvent event state
        -- If the mouse has moved, then update the radius of the circle.
        | EventMotion (x, y)    <- event
        , State (Just (a,b,c,d,e,f,g)) count ss    <- state
        = case count of        
            True -> State (Just (a,b,(sqrt((a-x)*(a-x) + (b-y)*(b-y))),x,y,x,y)) count ss
            False -> State (Just (a,b,c,d,e,x,y)) count ss
            
        -- Start drawing a new circle.
        | EventKey (MouseButton LeftButton) Down _ pt@(x,y) <- event
        , State Nothing True ss       <- state
        = State (Just (x,y,0.0,x,y,x,y)) True ss
            
        -- Finish drawing a circle, and add it to the picture.
        | EventKey (MouseButton LeftButton) Up _ pt@(x,y)      <- event
        , State (Just (a,b,c,d,e,f,g)) count ss    <- state
        = case count of 
            True -> State (Just (a,b,(sqrt((a-x)*(a-x) + (b-y)*(b-y))),x,y,x,y)) False ((Color red (Line [(a-10,b-25),(a,b-20),(a-10,b-10)])):(((Color red (Translate a b (Arc 0 270 20)))):ss))
            False -> State 
						Nothing
						True 
						((Translate a b
						(rotate (degrees a b d e x y)  
						  (scale (abs(d-a)/(scaleNormalizer (d-a) (e-b))) (abs(e-b)/(scaleNormalizer (d-a) (e-b))) (Circle (sqrt((a-d)*(a-d) + (b-e)*(b-e))))))):(drop 2 $ ss))
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
