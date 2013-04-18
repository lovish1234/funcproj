-- Multiple circles dynamically
import Data.Maybe (maybe)

import Control.Monad
import Data.Binary
import System.IO
import qualified Data.ByteString as Byte
import Data.List




main :: IO()

main = foo []
{-
main = 
    let foo = []    in
    forever $ do   
        name <- getLine
        let temp = [name]
        print (name ++ foo)
-}

filename1 = "out.bmp"
--filename2 = "
foo :: [Char] -> IO()
foo xs = 
    forever $ do
        x <- getLine
        print x
        let y = (read x :: Picture)
        print y
        h <- openFile "temp_out.bmp" ReadMode
        content <- Byte.hGetContents h
        --print content
        let n1 = 54
        let imageData1 = Byte.unpack $ Byte.drop (n1) (content)
        hClose h
        print $ Prelude.length imageData1
        print $ Byte.length content
        --print y
        y1 <- outputBMP [(read x :: Picture)] imageData1
        print y
        

data Point = Point (Float,Float) deriving (Read, Show)

data Picture = Circle Float Float Float
				| Square Float Float Float Float
                | Line (Float,Float) (Float,Float)
                | Polyline [Point]
                deriving (Show,Read)


    
    
        

{-
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
-}
rotateAndShiftPoint :: (Float,Float) -> Float -> Float -> Float -> (Float,Float)
rotateAndShiftPoint (x,y) ang a b = ((((x*(cos deg))-(y*(sin deg)))+a),(((x*(sin deg))+(y*(cos deg)))+b))
                                        where
                                            deg = ang*(pi/180)

shiftAndRotatePoint :: (Float,Float) -> Float -> Float -> Float -> (Float,Float)
shiftAndRotatePoint (x,y) theta x0 y0 = (((y-y0)*(sin theta) + (x - x0)*(cos theta)), ((y-y0)*(cos theta) - (x - x0)*(sin theta)))
										
{-
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
        {-| (Translate a b (rotate theta (scale n1 n2 (Circle r)))) <- shape, (x8,y8) <- p1, (x9,y9) <- p2
        ={-


            YOLO


        -}-}
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

-}

foo :: (Float, Float) -> Float -> Float -> Float -> (Float, Float)
foo (x0,y0) x1 y1 x2 y2= ((x1 + (fromIntegral ( ceiling (sqrt $ (x0-x1)*(x0-x1) + (y0-y1)*(y0-y1))))*(cos (atan ((y2-y1)/(x2-x1))))),(y1 + (fromIntegral $ ceiling( sqrt ( (x0-x1)*(x0-x1) + (y0-y1)*(y0-y1))))*(sin (atan ((y2-y1)/(x2-x1)))))) )

-- Code to generate .bmp file given the [Picture] array
-- The drawing area and hence the size of the bmp image is assumed to be 600*600 pixels
generatePoints :: Picture -> [(Float,Float)]
generatePoints pict
    | (Circle a b r) <- pict
    = concat[if (((x-191-b)*(x-191-b) + (y-283-a)*(y-283-a) >= r*r) && ((x-191-b)*(x-191-b) + (y-283-a)*(y-283-a) <= (r+1)*(r+1)) &&(x<382)&&(y<586)&&(x>=0)&&(y>=0)) then  [(x,y)] else [] | x <- [(191+b-(r+1))..(191+b + (r+1))], y<- [(283+a - (r+1))..(283+a +(r+1))]]
    | (Square x y deg side) <- pict
    = map (\(x0,y0) -> shiftAndRotatePoint (x0,y0) deg (x+191) (y+283)) $ concat [if ( ((x == side/2.0 ) && ( y <= side/2.0) && (y >= (0-side/2.0))) || ((x == (0-side/2.0) ) && ( y <= side/2.0) && (y >= (0-side/2.0))) || ((y == side/2.0 ) && ( x <= side/2.0) && (x >= (0-side/2.0))) || ((y == (0-side/2.0) ) && ( x <= side/2.0) && (x >= (0-side/2))) ) then [(x,y)] else []  | x <- [ ((0-1) - (fromIntegral (ceiling (side/2)) ))..(0+1 +(fromIntegral (ceiling (side/2))) )], y <- [ (0-1 -(fromIntegral (ceiling (side/2))) )..(0+1 +(fromIntegral (ceiling  (side/2))) )]]
    | (Line (y1,x1) (y2,x2)) <- pict
    = if (x1 == x2) then [(x1,y) | y <- [(min y1 y2) .. (max y1 y2)] ] 
		else ( if (y1 == y2) then [(x,y1) | x <- [(min x1 x2) .. (max x1 x2)]] 
			else (
					[(foo (x,0) x1 y1 x2 y2) | x <-[0..(0 + (fromIntegral ( ceiling ( sqrt ((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1))))))]] ))    --(shiftAndRotatePoint (x0,y0) ((atan ((y2-y1)/(x2-x1)))) (x1) (y1))
    {-
    |(Translate x0 y0 (Circle r)) <- pict
    = [(x+x0,y+y0) | x <- [0..r], y<- [0..r], x*x + y*y == r*r]
    
    |(Line []) <- pict
    = []
    |(Line [(x1,y1)]) <- pict
    = [(x1,y1)]
    | Line ((x1,y1):(x2,y2):xs) <- pict
    = [(x,y) | x <- [x1..x2],y <- [y1..y2], (y1-y)*(x2-x) == (y2-y)*(x1-x)] ++ (generatePoints (Line ((x2,y2):xs)))
    | (Translate a b (Rotate deg (Line [(x1,y1),(x2,y2),(x3,y3),(x4,y4),(x5,y5)]))) <- pict
    = let
            (r1,s1) = (rotateAndShiftPoint (x1,y1) (0-deg) a b)
            (r2,s2) = (rotateAndShiftPoint (x2,y2) (0-deg) a b)
            (r3,s3) = (rotateAndShiftPoint (x3,y3) (0-deg) a b)
            (r4,s4) = (rotateAndShiftPoint (x4,y4) (0-deg) a b)
            (r5,s5) = (rotateAndShiftPoint (x5,y5) (0-deg) a b)
            in
            generatePoints (Line [(r1,s1),(r2,s2),(r3,s3),(r4,s4),(r5,s5)])-}

--import qualified Data.List as D
getBinaryArray :: [Picture] -> [Word8]
getBinaryArray xs = let
            points = (concat (map generatePoints xs))
            in
            concat [if (elem (x,y) points) then [0,0,0] else [255,255,255] | x<-[0..382],y<-[0..567]]
            --concat [if  ((x-191)*(x-191) + (y-283)*(y-283) <= 100)&&((x-300)*(x-300) + (y-300)*(y-300) >= 81) then [0,0,0] else [255,255,255] | x<-[0..382],y<-[0..567]]


zeroArray :: Int -> [Word8]
zeroArray 0 = []
zeroArray i = 0:(zeroArray (i-1))



-- created a sample 600*600 image to get the header and copied it here
-- import Data.ByteString as Byte  in the beginning
outputBMP :: [Picture] -> [Word8] -> IO()
outputBMP xs imageData1 =   
        let     
            imageData2 = getBinaryArray xs 
            --imageData = [if ((imageData1 !! i) == 255) then (imageData2!! i) else (imageData1 !! i) | i <-[0..((Prelude.length imageData2) -1)] ]                       
            --imageData = [min (imageData1 !! i) (imageData2 !! i)  | i <-[0..((Prelude.length imageData2) -1)] ]                       
            imageData = zipWith (min) imageData1 imageData2
            header =[66,77,144,245,9,0,0,0,0,0,54,0,0,0,40,0,0,0,56,2,0,0,127,1,0,0,1,0,24,0,0,0,0,0,90,245,9,0,18,11,0,0,18,11,0,0,0,0,0,0,0,0,0,0]
            image = Byte.concat [Byte.pack header, Byte.pack imageData]
                in do
					--print (generatePoints (xs !! 0))
                    --print imageData                  
                    Byte.writeFile "temp_out.bmp" image
                    
