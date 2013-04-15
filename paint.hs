{-# LANGUAGE PatternGuards #-}
-- | Simple picture drawing application.
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Data.Maybe (maybe)
import Debug.Trace


main 
 = do   let state = Phi []
        play    (InWindow "Draw" (600, 600) (0,0))
                white 100 state
                makePicture handleEvent stepWorld



data State      
        = Phi [Picture]
		| MyCircle (Maybe (Float,Float,Float)) [Picture]
		| MyPencil (Maybe Path) [Picture]
		| MyPolyline (Maybe Path) [Picture]
		| MySquare (Maybe (Float,Float,Float,Float)) Bool (Maybe (Float, Float)) [Picture]
		| MyRectangle (Maybe (Float,Float,Float,Float)) (Maybe (Float, Float)) (Maybe (Float, Float)) [Picture]
		| MyEllipse (Maybe (Float,Float,Float,Float,Float,Float,Float)) Bool [Picture]
		| Inter (Maybe (Float,Float,Float,Float)) [Picture]
		| Surround (Maybe (Float,Float)) [Picture]

makePicture :: State -> Picture
makePicture shape
		| (Phi piclist)	<- shape
		= Pictures piclist
		| (Surround m xs) <- shape
		= Pictures (maybe xs (\(x,y) -> (map marker (surround (x,y) xs)) ++ xs) m)
		| (Inter m xs) <- shape
		= Pictures (maybe xs (\(x1,y1,x2,y2) -> (map marker (concat (map (intersections (x1,y1) (x2,y2)) xs))) ++ ((Line [(x1,y1),(x2,y2)]):xs)) m)
		| (MyCircle m xs) <- shape
		= Pictures (maybe xs (\(a,b,c) -> (Translate a b (Circle c)) : xs) m)
		| (MyPencil m xs)	<- shape
		= Pictures (maybe xs (\x -> Line x : xs) m)
		| (MyPolyline m xs) <- shape
		= Pictures (maybe xs (\x -> Line x : xs) m)
		| (MySquare m n r xs) <- shape
		= case n of
			False -> Pictures (maybe xs (\(a,b,c,d) -> ((Translate a b $ Line [(((min (abs (c-a)) (abs (d-b)))/2),((min (abs (c-a)) (abs (d-b)))/2)),(((min (abs (c-a)) (abs (d-b)))/2),-((min (abs (c-a)) (abs (d-b)))/2)),(-((min (abs (c-a)) (abs (d-b)))/2),-((min (abs (c-a)) (abs (d-b)))/2)),(-((min (abs (c-a)) (abs (d-b)))/2),((min (abs (c-a)) (abs (d-b)))/2)),(((min (abs (c-a)) (abs (d-b)))/2),((min (abs (c-a)) (abs (d-b)))/2))]):xs)) m)
			True -> case r of
						(Just (u,v)) -> Pictures (maybe xs (\(a,b,c,d) -> (Translate a b $ rotate (sdegrees a b c d u v ) $ Line [(((min (abs (c-a)) (abs (d-b)))/2),((min (abs (c-a)) (abs (d-b)))/2)),(((min (abs (c-a)) (abs (d-b)))/2),-((min (abs (c-a)) (abs (d-b)))/2)),(-((min (abs (c-a)) (abs (d-b)))/2),-((min (abs (c-a)) (abs (d-b)))/2)),(-((min (abs (c-a)) (abs (d-b)))/2),((min (abs (c-a)) (abs (d-b)))/2)),(((min (abs (c-a)) (abs (d-b)))/2),((min (abs (c-a)) (abs (d-b)))/2))]) : xs) m)
		| (MyRectangle m n r xs) <- shape
		= case n of 
			Nothing -> Pictures (maybe xs (\(a,b,p,q) -> (Translate ((a+p)/2) ((b+q)/2) $ Line [((p-a)/2,(b-q)/2),((p-a)/2,(q-b)/2),((a-p)/2,(q-b)/2),((a-p)/2,(b-q)/2),((p-a)/2,(b-q)/2)]) : xs) m)
			Just (p1, q1) -> case r of
							Just (u,v) -> Pictures (maybe xs (\(a,b,p,q) -> (Translate ((a+p)/2) ((b+q)/2) $ rotate (edegrees ((a+p)/2) ((b+q)/2) p1 q1 u v ) $ 
								Line [((p-a)/2,(b-q)/2),((p-a)/2,(q-b)/2),((a-p)/2,(q-b)/2),((a-p)/2,(b-q)/2),((p-a)/2,(b-q)/2)]) : xs) m)
		| (MyEllipse m count xs) <- shape
		= Pictures (maybe xs (\(a,b,c,x,y,f,g) -> ( Translate a b (rotate (edegrees a b x y f g) (scale (abs(x-a)/(scaleNormalizer (x-a) (y-b))) (abs(y-b)/(scaleNormalizer (x-a) (y-b))) (Circle c)))) : xs) m)


handleEvent :: Event -> State -> State
handleEvent event state
		| EventMotion (x, y)   <- event, Inter (Just (x1,y1,_,_)) ss    <- state
        = Inter (Just (x1,y1,x,y)) ss


        | EventKey (MouseButton LeftButton) Down _ pt@(x,y) <- event, Phi ss       <- state
        = Inter (Just (x,y,x,y)) ss

        | EventKey (MouseButton LeftButton) Up _ pt@(x,y)      <- event, Inter (Just (x1,y1,_,_)) ss    <- state
		= Phi ss
--        = Phi ((map marker (concat (map (intersections (x1,y1) (x,y)) ss))) ++ ss)


        | EventMotion (x, y) <- event , MyCircle (Just (a,b,_)) ss <- state
        = MyCircle (Just (a,b,(sqrt((a-x)*(a-x) + (b-y)*(b-y))))) ss

		| EventMotion (x,y) <- event, Surround _ xs <- state
		= Surround (Just (x,y)) xs

		| EventKey (Char 'f') Down _ pt@(x,y) <- event , Phi ss <- state
		= Surround (Just (x,y)) ss

		| EventKey (Char 'f') Up _ pt@(x,y) <- event , Surround (Just (x,y)) ss <- state
		= Phi ((triangleFill (x,y) (surround (x,y) ss)) ++ ss)


        | EventKey (Char 'a') Down _ pt@(x,y) <- event , Phi ss <- state
        = MyCircle (Just (x,y,0.0)) ss


        | EventKey (Char 'a') Up _ pt@(x,y) <- event , MyCircle (Just (a,b,_)) ss <- state
        = Phi ((Translate a b (Circle (sqrt((a-x)*(a-x) + (b-y)*(b-y))))):ss)

		| EventMotion (x, y) <- event , MyPencil (Just ps) ss  <- state
        = MyPencil (Just ((x, y):ps)) ss


        | EventKey (Char 'p') Down _ pt@(x,y) <- event , Phi ss <- state
        = MyPencil (Just [pt])  ss


        | EventKey (Char 'p') Up _ pt@(x,y)      <- event , MyPencil (Just ps) ss <- state
        = Phi ((Line (pt:ps)):ss)


        | EventMotion (x, y)    <- event , MyPolyline (Just [a,_]) ss <- state
        = MyPolyline (Just [a,(x, y)]) ss 

        | EventKey (Char 'l') Down _ pt@(x,y) <- event , Phi ss <- state
        = MyPolyline (Just [(x,y),(x,y)]) ss
        
        | EventKey (Char 'l') Down _ pt@(x,y) <- event , MyPolyline (Just [a]) ss       <- state
        = MyPolyline (Just [a,(x,y)]) ss

        | EventKey (SpecialKey KeyEnter) Down _ pt@(x,y) <- event , MyPolyline (Just [a]) ss       <- state
        = Phi ss

        | EventKey (Char 'l') Up _ pt@(x,y) <- event , MyPolyline (Just [a,_]) ss    <- state
        = MyPolyline (Just [(x,y)]) ((Line [a,(x,y)]):ss)

		| EventMotion (x, y) <- event , MySquare (Just (a,b,c,d)) u v ss  <- state
		= case u of
				False -> MySquare (Just (a,b,x,y)) False v ss
				True -> MySquare (Just (a,b,c,d)) True (Just (x, y)) ss

		| EventKey (Char 's') Down _ pt@(x,y) <- event , Phi ss <- state
		= MySquare (Just (x,y,x,y)) False Nothing ss
	        
		| EventKey (Char 's') Up _ pt@(x,y) <- event , MySquare (Just (a,b,c,d)) u v ss <- state
		= case u of
				False -> MySquare (Just (a,b,x,y)) True (Just (x,y)) ((Color red (Line [(a-10,b-25),(a,b-20),(a-10,b-10)])):(Color red (Translate a b (Arc 0 270 20))):ss)
				True -> case v of
							Just (a1, b1) -> Phi ((Translate a b $ rotate (sdegrees a b c d a1 b1) $ Line [(((min (abs (c-a)) (abs (d-b)))/2),((min (abs (c-a)) (abs (d-b)))/2)),(((min (abs (c-a)) (abs (d-b)))/2),-((min (abs (c-a)) (abs (d-b)))/2)),(-((min (abs (c-a)) (abs (d-b)))/2),-((min (abs (c-a)) (abs (d-b)))/2)),(-((min (abs (c-a)) (abs (d-b)))/2),((min (abs (c-a)) (abs (d-b)))/2)),(((min (abs (c-a)) (abs (d-b)))/2),((min (abs (c-a)) (abs (d-b)))/2))]):(drop 2 ss))

        | EventMotion (x, y)    <- event , MyRectangle (Just (a,b,c,d)) u v ss    <- state
        = case v of 
			Nothing -> MyRectangle (Just (a,b,x,y) ) u v ss
			Just (p, q) -> MyRectangle (Just (a,b,c,d)) u (Just (x, y)) ss

        | EventKey (Char 'r') Down _ pt@(x,y) <- event , Phi ss       <- state
        = MyRectangle (Just (x,y,x,y)) Nothing Nothing ss

        | EventKey (Char 'r') Up _ pt@(x,y)      <- event , MyRectangle (Just (a,b,p,q)) u v ss    <- state
        = case u of 
			Nothing -> MyRectangle (Just (a,b,x,y)) (Just (x,y)) (Just (x,y)) 
				((Color red (Line [(((a+x)/2)-10,((b+y)/2)-25),(((a+x)/2),((b+y)/2)-20),(((a+x)/2)-10,((b+y)/2)-10)])):(((Color red (Translate ((a+x)/2) ((b+y)/2) (Arc 0 270 20)))):ss))		-- Just clicked the rectangle
			Just (p1, q1) -> case v of
							Just (a1, b1) -> Phi 
								((Translate ((a+p)/2) ((b+q)/2) $ rotate (edegrees ((a+p)/2) ((b+q)/2) p1 q1 a1 b1) $ 
									Line [((p-a)/2,(b-q)/2),((p-a)/2,(q-b)/2),((a-p)/2,(q-b)/2),((a-p)/2,(b-q)/2),((p-a)/2,(b-q)/2)]):(drop 2 ss))

		
		| EventMotion (x, y) <- event , MyEllipse (Just (a,b,c,d,e,f,g)) count ss <- state
        = case count of        
            True -> MyEllipse (Just (a,b,(sqrt((a-x)*(a-x) + (b-y)*(b-y))),x,y,x,y)) count ss
            False -> MyEllipse (Just (a,b,c,d,e,x,y)) count ss
            
        | EventKey (Char 'e') Down _ pt@(x,y) <- event , Phi ss <- state
        = MyEllipse (Just (x,y,0.0,x,y,x,y)) True ss

        | EventKey (Char 'e') Up _ pt@(x,y)      <- event , MyEllipse (Just (a,b,c,d,e,f,g)) count ss <- state
        = case count of 
            True -> MyEllipse (Just (a,b,(sqrt((a-x)*(a-x) + (b-y)*(b-y))),x,y,x,y)) False ((Color red (Line [(a-10,b-25),(a,b-20),(a-10,b-10)])):(((Color red (Translate a b (Arc 0 270 20)))):ss))
            False -> Phi ((Translate a b (rotate (edegrees a b d e x y) (scale (abs(d-a)/(scaleNormalizer (d-a) (e-b))) (abs(e-b)/(scaleNormalizer (d-a) (e-b))) (Circle (sqrt((a-d)*(a-d) + (b-e)*(b-e))))))):(drop 2 $ ss))

        | otherwise
        = state


stepWorld :: Float -> State -> State
stepWorld _ = id

sdegrees :: Float -> Float -> Float -> Float -> Float -> Float -> Float
sdegrees x1 y1 x2 y2 x3 y3
			= atan((s1-s2)/(1+s1*s2))*(180/pi)
				where
					s2= (y3-y1)/(x3-x1)
					s1= (y2-y1)/(x2-x1)

scaleNormalizer :: Float -> Float -> Float
scaleNormalizer x y
    = sqrt(x*x+y*y)

edegrees :: Float -> Float -> Float -> Float -> Float -> Float -> Float
edegrees x_cen y_cen l1x l1y l2x l2y
		= let
			s1=(l1y-y_cen)/(l1x-x_cen)
			s2=(l2y-y_cen)/(l2x-x_cen)
		   in
		    atan((s1-s2)/(1+s1*s2))*180/pi

marker :: (Maybe (Float,Float)) -> Picture
marker (Just (a,b)) = Translate a b $ (ThickCircle 0 5)
marker Nothing = Blank

dist :: (Float,Float) -> (Float,Float) -> Float
dist (x1,y1) (x2,y2) = sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1))

between :: (Float,Float) -> (Float,Float) -> (Float,Float) -> Bool
between (x1,y1) (x2,y2) (x3,y3)
		| ((((x3-x1)*(x3-x2))+((y3-y1)*(y3-y2))) > 0) = False
		| ((((x3-x1)*(x3-x2))+((y3-y1)*(y3-y2))) <= 0) = True
		| otherwise = False

closerPoint :: (Float,Float) -> (Maybe (Float,Float)) -> (Maybe (Float,Float)) -> (Maybe (Float,Float))
closerPoint _ Nothing (Just p) = Just p
closerPoint _ (Just p) Nothing = Just p
closerPoint (x,y) (Just (x1,y1)) (Just (x2,y2))
		| ((dist (x,y) (x1,y1)) >= (dist (x,y) (x2,y2))) = (Just (x2,y2))
		| ((dist (x,y) (x2,y2)) > (dist (x,y) (x1,y1))) = (Just (x1,y1))

closestPoint :: (Float,Float) -> [(Maybe (Float,Float))] -> (Maybe (Float,Float))
closestPoint (x,y) mps
		=let
			m = foldl (closerPoint (x,y)) (Just (x+1000,y+1000)) mps
			in
			case m of
				(Just (a,b)) -> if ((a == x+1000) && (b == y+1000))
									then Nothing
									else (Just (a,b))

rotateAndShiftPoint :: (Float,Float) -> Float -> Float -> Float -> (Float,Float)
rotateAndShiftPoint (x,y) ang a b = ((((x*(cos deg))-(y*(sin deg)))+a),(((x*(sin deg))+(y*(cos deg)))+b))
										where
											deg = ang*(pi/180)

lineInter :: (Float,Float) -> (Float,Float) -> (Float,Float) -> (Float,Float) -> (Maybe (Float,Float))
lineInter (x1,y1) (x2,y2) (x3,y3) (x4,y4)
		| (((y1-y2)*(x4-x3)-(y3-y4)*(x2-x1)) == 0) = Nothing
		
		| otherwise
		=let (i1,i2) = ((((y4*x3 - x4*y3)*(x2 - x1)-(y2*x1 - x2*y1)*(x4 - x3))/((y1-y2)*(x4-x3)-(y3-y4)*(x2-x1))),(((y4*x3 - x4*y3)*(y1 - y2)-(y2*x1 - x2*y1)*(y3 - y4))/((y3-y4)*(x2-x1)-(y1-y2)*(x4-x3))))
		in
			case ((between (x1,y1) (x2,y2) (i1,i2)) && (between (x3,y3) (x4,y4) (i1,i2))) of
				True -> Just (i1,i2)
				False -> Nothing

intersections :: (Float,Float) -> (Float,Float) -> Picture -> [(Maybe (Float,Float))]
intersections p1 p2 shape
		| (Translate x y (Circle r)) <- shape, (x1,y1) <- p1, (x2,y2) <- p2
		= let
			(x3,y3) = closestPointOnLine (x1,y1) (x2,y2) (x,y)
			in
			let
				dc = sqrt((r*r)-((x3-x)*(x3-x))-((y3-y)*(y3-y)))
				d1 = sqrt(((x3-x1)*(x3-x1)) + ((y3-y1)*(y3-y1)))
				d2 = sqrt(((x3-x2)*(x3-x2)) + ((y3-y2)*(y3-y2)))
				(xv1,yv1) = (x1,y1)-(x3,y3)
				(xv2,yv2) = (x2,y2)-(x3,y3)
				in
				filter (\(Just (a,b)) -> between (x1,y1) (x2,y2) (a,b)) [Just ((x3+(xv1*dc/d1)),(y3+(yv1*dc/d1))) , Just ((x3+(xv2*dc/d2)),(y3+(yv2*dc/d2)))]

		| (Line []) <- shape, p1 <- p1, p2 <- p2
		= []

		| (Line [(x1,y1)]) <- shape, (x2,y2) <- p1, (x3,y3) <- p2
		= []
		
		| (Line [(x1,y1),(x2,y2)]) <- shape, (x3,y3) <- p1, (x4,y4) <- p2
		= (lineInter (x1,y1) (x2,y2) (x3,y3) (x4,y4)):[]

		| (Line ((x1,y1):(x2,y2):linepath)) <- shape, (x3,y3) <- p1, (x4,y4) <- p2
		= (intersections (x3,y3) (x4,y4) (Line [(x1,y1),(x2,y2)])) ++ (intersections (x3,y3) (x4,y4) (Line ((x2,y2):linepath)))

		| (Translate a b (Rotate deg (Line [(x1,y1),(x2,y2),(x3,y3),(x4,y4),(x5,y5)]))) <- shape, (x6,y6) <- p1, (x7,y7) <- p2
		= let
			(r1,s1) = (rotateAndShiftPoint (x1,y1) (0-deg) a b)
			(r2,s2) = (rotateAndShiftPoint (x2,y2) (0-deg) a b)
			(r3,s3) = (rotateAndShiftPoint (x3,y3) (0-deg) a b)
			(r4,s4) = (rotateAndShiftPoint (x4,y4) (0-deg) a b)
			(r5,s5) = (rotateAndShiftPoint (x5,y5) (0-deg) a b)
			in
			[(lineInter (r1,s1) (r2,s2) (x6,y6) (x7,y7)),(lineInter (r2,s2) (r3,s3) (x6,y6) (x7,y7)),(lineInter (r3,s3) (r4,s4) (x6,y6) (x7,y7)),(lineInter (r4,s4) (r5,s5) (x6,y6) (x7,y7))]

		| something <- shape, q1 <- p1, q2 <- p2 
		= [(Just p2)]

surround :: (Float,Float) -> [Picture] -> [(Maybe (Float,Float))]
surround (x,y) ps 
		= let
			radius = 100000
			surrounding = map ((*) (pi/180)) [0,5 .. 360]
			in
			let
				emerges = map (\d -> ((x,y),((x+(radius*(cos d))),(y+(radius*(sin d)))))) surrounding
				in
				let
					funclist = (map (\(p1,p2) -> (intersections p1 p2)) emerges)
					in
					[closestPoint (x,y) (concat (map f ps)) | f <- funclist]


triangleFill :: (Float,Float) -> [(Maybe (Float,Float))] -> [Picture]
triangleFill (x,y) [] = []
triangleFill (x,y) [a] = []
triangleFill (x,y) (Nothing:ss) = triangleFill (x,y) ss
triangleFill (x,y) ((Just (x1,y1)):(Nothing):ss) = triangleFill (x,y) ((Just (x1,y1)):ss)
triangleFill (x,y) ((Just (x1,y1)):(Just (x2,y2)):ss)
		= (Color aquamarine (Polygon [(x,y),(x1,y1),(x2,y2)])):(triangleFill (x,y) ((Just (x2,y2)):ss))
-- lets say we made this change
