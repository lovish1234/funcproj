{-# LANGUAGE PatternGuards #-}
module Main
( main,
  makePicture,
  makePicture',
  picRead,
  showLn,
  handleEvent,
  State,
  stepWorld,
  sdegrees ,
  scaleNormalizer,
  edegrees,
  marker,
  dist,
  between,
  closerPoint,
  closestPoint,
  rotateAndShiftPoint,
  lineInter,
  intersections,
  surround,
  triangleFill,
  colorMaker) 
where
        
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Data.Maybe (maybe)
import Data.List
import Debug.Trace
import System.IO
import Parser (parseIO)
import System.Environment

-- | The main function.
-- [OPTIONAL] Can take an input file from the command line to start with.
main 
 = do   
        args <- getArgs     
        --  putStrLn $ show args
        if (length args) == 0
          then 
            let             
             shapes = []
                in
                let state = Phi black ((Pictures ([(Color red (Polygon [(-270,260),(-270,240),(-290,240),(-290,260),(-270,260)])),(Color blue (Polygon [(-270,240),(-270,220),(-290,220),(-290,240),(-270,240)])),(Color green (Polygon [(-270,220),(-270,200),(-290,200),(-290,220),(-270,220)])),(Color yellow (Polygon [(-270,200),(-270,180),(-290,180),(-290,200),(-270,200)]))])):(foldl picRead [] shapes))
                    in
                    play (InWindow "Draw" (600, 600) (0,0)) white 100 state makePicture' handleEvent stepWorld
          else
           do
            filecont <- readFile (args!!0)
            let shapes = lines filecont
                in
                let state = Phi black ((Pictures ([(Color red (Polygon [(-270,260),(-270,240),(-290,240),(-290,260),(-270,260)])),(Color blue (Polygon [(-270,240),(-270,220),(-290,220),(-290,240),(-270,240)])),(Color green (Polygon [(-270,220),(-270,200),(-290,200),(-290,220),(-270,220)])),(Color yellow (Polygon [(-270,200),(-270,180),(-290,180),(-290,200),(-270,200)]))])):(foldl picRead [] shapes))
                    in
                    play (InWindow "Draw" (600, 600) (0,0)) white 100 state makePicture' handleEvent stepWorld

-- | Show with a new line
showLn :: Show a => a -> String
showLn a = show a ++ "\n"

-- | This function is used to display pictures mentioned ithe input file. Converts strings read from files to actual pictures.
picRead :: [Picture] -> [Char] -> [Picture]
picRead p [] = Blank:p
picRead [] shape
        | (Just restOfString) <- (stripPrefix "Circle " shape) = let (a,b,c) = (read restOfString :: (Float,Float,Float))
                                                                                        in
                                                                                        [(Translate a b (Circle c))]
        | (Just restOfString) <- (stripPrefix "Square " shape) = let (a,b,c,d) = (read restOfString :: (Float,Float,Float,Float))
                                                                                        in
                                                                                        [(Translate a b (Rotate c (Line [((a+d/2),(b+d/2)),((a+d/2),(b-d/2)),((a-d/2),(b-d/2)),((a-d/2),(b+d/2)),((a+d/2),(b+d/2))])))]
        | (Just restOfString) <- (stripPrefix "Rectangle " shape) = let (a,b,c,d,e) = (read restOfString :: (Float,Float,Float,Float,Float))
                                                                                        in
                                                                                        [(Translate a b (Rotate c (Line [((a+d/2),(b+e/2)),((a+d/2),(b-e/2)),((a-d/2),(b-e/2)),((a-d/2),(b+e/2)),((a+d/2),(b+e/2))])))]
        | (Just restOfString) <- (stripPrefix "Fill " shape) = [Blank]
        | (Just restOfString) <- (stripPrefix "Polyline " shape) = let pointList = (read restOfString :: [(Float,Float)])
                                                                                        in
                                                                                        [(Line pointList)]
        |(Just restOfString) <- (stripPrefix "Ellipse " shape) = let (a,b,c,d,e) = (read restOfString :: (Float,Float,Float,Float,Float))
                                                                                        in
                                                                                        [(Translate a b (Rotate d (scale (e/c) 1 (Circle c))))]
        |(Just restOfString) <- (stripPrefix "Color " shape) = let (r,g,b,a) = (read restOfString :: (Float,Float,Float,Float))
                                                                    in
                                                                    [(Color (makeColor r g b a) Blank)]
        
        | a <- shape = [Blank]

picRead shapes shape
        | (Just restOfString) <- (stripPrefix "Circle " shape) = let (a,b,c) = (read restOfString :: (Float,Float,Float))
                                                                                        in
                                                                                        (Translate a b (Circle c)):shapes
        | (Just restOfString) <- (stripPrefix "Square " shape) = let (a,b,c,d) = (read restOfString :: (Float,Float,Float,Float))
                                                                                        in
                                                                                        (Translate a b (Rotate c (Line [((a+d/2),(b+d/2)),((a+d/2),(b-d/2)),((a-d/2),(b-d/2)),((a-d/2),(b+d/2)),((a+d/2),(b+d/2))]))):shapes
        | (Just restOfString) <- (stripPrefix "Rectangle " shape) = let (a,b,c,d,e) = (read restOfString :: (Float,Float,Float,Float,Float))
                                                                                        in
                                                                                        (Translate a b (Rotate c (Line [((a+d/2),(b+e/2)),((a+d/2),(b-e/2)),((a-d/2),(b-e/2)),((a-d/2),(b+e/2)),((a+d/2),(b+e/2))]))):shapes
        | (Just restOfString) <- (stripPrefix "Fill " shape), (s:ss) <- shapes = let (a,b) = (read restOfString :: (Float,Float))
                                                                                        in
                                                                                        case s of
                                                                                            (Color rgb Blank) -> ((Color rgb (Pictures (triangleFill (a,b) (surround (a,b) shapes)))) : shapes)
                                                                                            _->((triangleFill (a,b) (surround (a,b) shapes)) ++ shapes)
        | (Just restOfString) <- (stripPrefix "Polyline " shape), (s:ss) <- shapes = let pointList = (read restOfString :: [(Float,Float)])
                                                                                        in
                                                                                        (Line pointList):shapes
        |(Just restOfString) <- (stripPrefix "Ellipse " shape) = let (a,b,c,d,e) = (read restOfString :: (Float,Float,Float,Float,Float))
                                                                                        in
                                                                                        (Translate a b (Rotate d (scale (e/c) 1 (Circle c)))):shapes
        |(Just restOfString) <- (stripPrefix "Color " shape) = let (RGBA r g b a) = (read restOfString :: Color)
                                                                    in
                                                                    (Color (makeColor r g b a) Blank):shapes
        
        | a <- shape = Blank:shapes

-- | The game state
data State      
        = Phi Color [Picture] 								-- | The paint is ready to take a new state
        | MyCircle (Maybe (Float,Float,Float)) [Picture]	-- Circle. Arguments:  (xcen, ycen, radius)
        | MyPencil (Maybe Path) [Picture]					-- Pencil. Arguments:  (A set of points)
        | MyPolyline (Maybe Path) [Picture]					-- Polyline. Arguments:  (A set of points)
        | MySquare (Maybe (Float,Float,Float,Float)) Bool (Maybe (Float, Float)) [Picture]									-- Square. Arguments:  (xcen, ycen, edgelength,)
        | MyRectangle (Maybe (Float,Float,Float,Float)) (Maybe (Float, Float)) (Maybe (Float, Float)) [Picture]				-- current rectangle being drawn on coordinates of a diagonal, The point where we clicked the rectangle, The current coordinates
        | MyEllipse (Maybe (Float,Float,Float,Float,Float,Float,Float)) Bool [Picture]										-- (x_cen, y_cen, radius, firstclick_x, firstclick_y, x_cur, y_cur)
        | Inter (Maybe (Float,Float,Float,Float)) [Picture]	-- Testing purposes
        | Surround Color (Maybe (Float,Float)) [Picture]	-- guide markers fo coloue fill

-- | A wrapper for calling makePicture function.
makePicture' :: State -> IO Picture
makePicture' state
        = case state of
                (Phi rgba piclist) ->         
                    do
                        writeFile "Gloss_lang.txt" (concatMap showLn piclist)
                        parseIO     
                        return $ makePicture state
                _ ->
                    return $ makePicture state  
-- | Convert our state to a picture.
makePicture :: State -> Picture
makePicture shape
        | (Phi rgba piclist) <- shape
        = Pictures piclist
        | (Surround rgba m xs) <- shape
        = Pictures (maybe xs (\(x,y) -> (Color rgba (Pictures (map marker (surround (x,y) xs)))):xs) m)
        | (MyCircle m xs) <- shape
        = Pictures (maybe xs (\(a,b,c) -> (Translate a b (Circle c)) : xs) m)
        | (MyPencil m xs)   <- shape
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

-- | Handle mouse click and motion events.
handleEvent :: Event -> State -> State
handleEvent event state
        | EventMotion (x, y)   <- event, Phi rgba ss <- state
        = Phi rgba ss


        | EventKey (MouseButton LeftButton) Down _ pt@(x,y) <- event, Phi rgba ss       <- state
        = Phi (colorMaker (x,y)) ss

        | EventKey (MouseButton LeftButton) Up _ pt@(x,y)      <- event, Phi rgba ss    <- state
        = Phi rgba ss
--        = Phi ((map marker (concat (map (intersections (x1,y1) (x,y)) ss))) ++ ss)


        | EventMotion (x, y) <- event , MyCircle (Just (a,b,_)) ss <- state
        = MyCircle (Just (a,b,(sqrt((a-x)*(a-x) + (b-y)*(b-y))))) ss

        | EventMotion (x,y) <- event, Surround rgba _ xs <- state
        = Surround rgba (Just (x,y)) xs

        | EventKey (Char 'f') Down _ pt@(x,y) <- event , Phi rgba ss <- state
        = Surround rgba (Just (x,y)) ss

        | EventKey (Char 'f') Up _ pt@(x,y) <- event , Surround rgba (Just (x,y)) ss <- state
        = Phi black (((Color rgba (Pictures (triangleFill (x,y) (surround (x,y) ss)))):[]) ++ ((Translate x y (Blank)):ss))


        | EventKey (Char 'a') Down _ pt@(x,y) <- event , Phi rgba ss <- state
        = MyCircle (Just (x,y,0.0)) ss


        | EventKey (Char 'a') Up _ pt@(x,y) <- event , MyCircle (Just (a,b,_)) ss <- state
        = Phi black ((Translate a b (Circle (sqrt((a-x)*(a-x) + (b-y)*(b-y))))):ss)

        | EventMotion (x, y) <- event , MyPencil (Just ps) ss  <- state
        = MyPencil (Just ((x, y):ps)) ss


        | EventKey (Char 'p') Down _ pt@(x,y) <- event , Phi rgba ss <- state
        = MyPencil (Just [pt])  ss


        | EventKey (Char 'p') Up _ pt@(x,y)      <- event , MyPencil (Just ps) ss <- state
        = Phi black ((Line (pt:ps)):ss)


        | EventMotion (x, y)    <- event , MyPolyline (Just [a,_]) ss <- state
        = MyPolyline (Just [a,(x, y)]) ss 

        | EventKey (Char 'l') Down _ pt@(x,y) <- event , Phi rgba ss <- state
        = MyPolyline (Just [(x,y),(x,y)]) ss
        
        | EventKey (Char 'l') Down _ pt@(x,y) <- event , MyPolyline (Just [a]) ss       <- state
        = MyPolyline (Just [a,(x,y)]) ss

        | EventKey (SpecialKey KeyEnter) Down _ pt@(x,y) <- event , MyPolyline (Just [a]) ss       <- state
        = Phi black ss

        | EventKey (Char 'l') Up _ pt@(x,y) <- event , MyPolyline (Just [a,_]) ss    <- state
        = MyPolyline (Just [(x,y)]) ((Line [a,(x,y)]):ss)

        | EventMotion (x, y) <- event , MySquare (Just (a,b,c,d)) u v ss  <- state
        = case u of
                False -> MySquare (Just (a,b,x,y)) False v ss
                True -> MySquare (Just (a,b,c,d)) True (Just (x, y)) ss

        | EventKey (Char 's') Down _ pt@(x,y) <- event , Phi rgba ss <- state
        = MySquare (Just (x,y,x,y)) False Nothing ss
            
        | EventKey (Char 's') Up _ pt@(x,y) <- event , MySquare (Just (a,b,c,d)) u v ss <- state
        = case u of
                False -> MySquare (Just (a,b,x,y)) True (Just (x,y)) ((Color red (Line [(a-10,b-25),(a,b-20),(a-10,b-10)])):(Color red (Translate a b (Arc 0 270 20))):ss)
                True -> case v of
                            Just (a1, b1) -> Phi black ((Translate a b $ Rotate (sdegrees a b c d a1 b1) $ Line [(((min (abs (c-a)) (abs (d-b)))/2),((min (abs (c-a)) (abs (d-b)))/2)),(((min (abs (c-a)) (abs (d-b)))/2),-((min (abs (c-a)) (abs (d-b)))/2)),(-((min (abs (c-a)) (abs (d-b)))/2),-((min (abs (c-a)) (abs (d-b)))/2)),(-((min (abs (c-a)) (abs (d-b)))/2),((min (abs (c-a)) (abs (d-b)))/2)),(((min (abs (c-a)) (abs (d-b)))/2),((min (abs (c-a)) (abs (d-b)))/2))]):(drop 2 ss))

        | EventMotion (x, y)    <- event , MyRectangle (Just (a,b,c,d)) u v ss    <- state
        = case v of 
            Nothing -> MyRectangle (Just (a,b,x,y) ) u v ss
            Just (p, q) -> MyRectangle (Just (a,b,c,d)) u (Just (x, y)) ss

        | EventKey (Char 'r') Down _ pt@(x,y) <- event , Phi rgba ss       <- state
        = MyRectangle (Just (x,y,x,y)) Nothing Nothing ss

        | EventKey (Char 'r') Up _ pt@(x,y)      <- event , MyRectangle (Just (a,b,p,q)) u v ss    <- state
        = case u of 
            Nothing -> MyRectangle (Just (a,b,x,y)) (Just (x,y)) (Just (x,y)) 
                ((Color red (Line [(((a+x)/2)-10,((b+y)/2)-25),(((a+x)/2),((b+y)/2)-20),(((a+x)/2)-10,((b+y)/2)-10)])):(((Color red (Translate ((a+x)/2) ((b+y)/2) (Arc 0 270 20)))):ss))       -- Just clicked the rectangle
            Just (p1, q1) -> case v of
                            Just (a1, b1) -> Phi black 
                                ((Translate ((a+p)/2) ((b+q)/2) $ rotate (edegrees ((a+p)/2) ((b+q)/2) p1 q1 a1 b1) $ 
                                    Line [((p-a)/2,(b-q)/2),((p-a)/2,(q-b)/2),((a-p)/2,(q-b)/2),((a-p)/2,(b-q)/2),((p-a)/2,(b-q)/2)]):(drop 2 ss))

        
        | EventMotion (x, y) <- event , MyEllipse (Just (a,b,c,d,e,f,g)) count ss <- state
        = case count of        
            True -> MyEllipse (Just (a,b,(sqrt((a-x)*(a-x) + (b-y)*(b-y))),x,y,x,y)) count ss
            False -> MyEllipse (Just (a,b,c,d,e,x,y)) count ss
            
        | EventKey (Char 'e') Down _ pt@(x,y) <- event , Phi rgba ss <- state
        = MyEllipse (Just (x,y,0.0,x,y,x,y)) True ss

        | EventKey (Char 'e') Up _ pt@(x,y)      <- event , MyEllipse (Just (a,b,c,d,e,f,g)) count ss <- state
        = case count of 
            True -> MyEllipse (Just (a,b,(sqrt((a-x)*(a-x) + (b-y)*(b-y))),x,y,x,y)) False ((Color red (Line [(a-10,b-25),(a,b-20),(a-10,b-10)])):(((Color red (Translate a b (Arc 0 270 20)))):ss))
            False -> Phi black ((Translate a b (Rotate (edegrees a b d e x y) (scale (abs(d-a)/(scaleNormalizer (d-a) (e-b))) (abs(e-b)/(scaleNormalizer (d-a) (e-b))) (Circle (sqrt((a-d)*(a-d) + (b-e)*(b-e))))))):(drop 2 $ ss))

        | otherwise
        = state

-- ! Time progression of states.
stepWorld :: Float -> State -> State
stepWorld _ = id

-- | Computes the angle (in degrees) between two lines , connected by a given point. Used for square
sdegrees :: Float -> Float -> Float -> Float -> Float -> Float -> Float
sdegrees x1 y1 x2 y2 x3 y3
            = atan((s1-s2)/(1+s1*s2))*(180/pi)
                where
                    s2= (y3-y1)/(x3-x1)
                    s1= (y2-y1)/(x2-x1)

-- | root mean square of the two floats.
scaleNormalizer :: Float -> Float -> Float
scaleNormalizer x y
    = sqrt(x*x+y*y)

-- | Computes the angle (in degrees) between two lines l1 and l2, connected by (x_cen, y_cen)
edegrees :: Float -> Float -> Float -> Float -> Float -> Float -> Float
edegrees x_cen y_cen l1x l1y l2x l2y
        = let
            s1=(l1y-y_cen)/(l1x-x_cen)
            s2=(l2y-y_cen)/(l2x-x_cen)
           in
            atan((s1-s2)/(1+s1*s2))*180/pi

-- | Used in Surround function
marker :: (Maybe (Float,Float)) -> Picture
marker (Just (a,b)) = Translate a b $ (ThickCircle 0 5)
marker Nothing = Blank

-- | Euclidean distance between two points.
dist :: (Float,Float) -> (Float,Float) -> Float
dist (x1,y1) (x2,y2) = sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1))

-- | If a point lies between two other points.
between :: (Float,Float) -> (Float,Float) -> (Float,Float) -> Bool
between (x1,y1) (x2,y2) (x3,y3)
        | ((((x3-x1)*(x3-x2))+((y3-y1)*(y3-y2))) > 0) = False
        | ((((x3-x1)*(x3-x2))+((y3-y1)*(y3-y2))) <= 0) = True
        | otherwise = False

-- | Return the point closer to the first argument.
closerPoint :: (Float,Float) -> (Maybe (Float,Float)) -> (Maybe (Float,Float)) -> (Maybe (Float,Float))
closerPoint _ Nothing (Just p) = Just p
closerPoint _ (Just p) Nothing = Just p
closerPoint (x,y) (Just (x1,y1)) (Just (x2,y2))
        | ((dist (x,y) (x1,y1)) >= (dist (x,y) (x2,y2))) = (Just (x2,y2))
        | ((dist (x,y) (x2,y2)) > (dist (x,y) (x1,y1))) = (Just (x1,y1))

-- | Finds the closest point in a list of points.
closestPoint :: (Float,Float) -> [(Maybe (Float,Float))] -> (Maybe (Float,Float))
closestPoint (x,y) mps
        =let
            m = foldl (closerPoint (x,y)) (Just (x+1000,y+1000)) mps
            in
            case m of
                (Just (a,b)) -> if ((a == x+1000) && (b == y+1000))
                                    then Nothing
                                    else (Just (a,b))

-- | Rotates a given point by a given angle and then translates it by a given distance.
rotateAndShiftPoint :: (Float,Float) -> Float -> Float -> Float -> (Float,Float)
rotateAndShiftPoint (x,y) ang a b = ((((x*(cos deg))-(y*(sin deg)))+a),(((x*(sin deg))+(y*(cos deg)))+b))
                                        where
                                            deg = ang*(pi/180)

-- | The function that calls intersections to compute line intersections with any shape.
lineInter :: (Float,Float) -> (Float,Float) -> (Float,Float) -> (Float,Float) -> (Maybe (Float,Float))
lineInter (x1,y1) (x2,y2) (x3,y3) (x4,y4)
        | (((y1-y2)*(x4-x3)-(y3-y4)*(x2-x1)) == 0) = Nothing
        
        | otherwise
        =let (i1,i2) = ((((y4*x3 - x4*y3)*(x2 - x1)-(y2*x1 - x2*y1)*(x4 - x3))/((y1-y2)*(x4-x3)-(y3-y4)*(x2-x1))),(((y4*x3 - x4*y3)*(y1 - y2)-(y2*x1 - x2*y1)*(y3 - y4))/((y3-y4)*(x2-x1)-(y1-y2)*(x4-x3))))
        in
            case ((between (x1,y1) (x2,y2) (i1,i2)) && (between (x3,y3) (x4,y4) (i1,i2))) of
                True -> Just (i1,i2)
                False -> Nothing

-- | Given a line, returns a set of points intersecting with any shape.
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
        | (Color rgb (Translate x y (Circle r))) <- shape, (x1,y1) <- p1, (x2,y2) <- p2
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
        
        | (Color rgb (Line [])) <- shape, p1 <- p1, p2 <- p2
        = []

        | (Line [(x1,y1)]) <- shape, (x2,y2) <- p1, (x3,y3) <- p2
        = []
        
        | (Color rgb (Line [(x1,y1)])) <- shape, (x2,y2) <- p1, (x3,y3) <- p2
        = []
        
        | (Line [(x1,y1),(x2,y2)]) <- shape, (x3,y3) <- p1, (x4,y4) <- p2
        = (lineInter (x1,y1) (x2,y2) (x3,y3) (x4,y4)):[]
        
        | (Color rgb (Line [(x1,y1),(x2,y2)])) <- shape, (x3,y3) <- p1, (x4,y4) <- p2
        = (lineInter (x1,y1) (x2,y2) (x3,y3) (x4,y4)):[]

        | (Line ((x1,y1):(x2,y2):linepath)) <- shape, (x3,y3) <- p1, (x4,y4) <- p2
        = (intersections (x3,y3) (x4,y4) (Line [(x1,y1),(x2,y2)])) ++ (intersections (x3,y3) (x4,y4) (Line ((x2,y2):linepath)))
        
        | (Color rgb (Line ((x1,y1):(x2,y2):linepath))) <- shape, (x3,y3) <- p1, (x4,y4) <- p2
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
        
        | (Color rgb (Translate a b (Rotate deg (Line [(x1,y1),(x2,y2),(x3,y3),(x4,y4),(x5,y5)])))) <- shape, (x6,y6) <- p1, (x7,y7) <- p2
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

-- | Used for user ease during color fill. It marks the boundary of to fileed by marker dots
surround :: (Float,Float) -> [Picture] -> [(Maybe (Float,Float))]
surround (x,y) ps 
        = let
            radius = 100000
            surrounding = map ((*) (pi/180)) [0,2 .. 360]
            in
            let
                emerges = map (\d -> ((x,y),((x+(radius*(cos d))),(y+(radius*(sin d)))))) surrounding
                in
                let
                    funclist = (map (\(p1,p2) -> (intersections p1 p2)) emerges)
                    in
                    [closestPoint (x,y) (concat (map f ps)) | f <- funclist]

-- | Fills colors using triangles projecting out from the point of colour filling tap. Tha is why this fills only the region visible from the point of origin/fill command. THIS IS NOT FLOOD FILLING.
triangleFill :: (Float,Float) -> [(Maybe (Float,Float))] -> [Picture]
triangleFill (x,y) [] = []
triangleFill (x,y) [a] = []
triangleFill (x,y) (Nothing:ss) = triangleFill (x,y) ss
triangleFill (x,y) ((Just (x1,y1)):(Nothing):ss) = triangleFill (x,y) ((Just (x1,y1)):ss)
triangleFill (x,y) ((Just (x1,y1)):(Just (x2,y2)):ss)
        = (Polygon [(x,y),(x1,y1),(x2,y2)]):(triangleFill (x,y) ((Just (x2,y2)):ss))

-- | This function assigns colour to fill when user clicks on a colour in the colour pallette.
colorMaker :: (Float,Float) -> Color
colorMaker (x,y) = if (x>(-290.0) && x<(-270.0))
                    then
                        if (y>180.0 && y<200.0 )
                         then trace "yellow" yellow 
                         else
                            if (y>200.0 && y<220.0) 
                                then trace "green" green
                                else
                                    if (y>220.0 && y<240.0) 
                                      then trace "blue" blue
                                      else
                                        if (y>240.0 && y<260.0)
                                          then trace "red" red
                                          else trace "black" black
                    else
                        trace "black at the end" black
