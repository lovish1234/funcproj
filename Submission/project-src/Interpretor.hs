module Interpreter 
( foo,
  Commands,
  Point,
  Picture,
  rotateAndShiftPoint,
  shiftAndRotatePoint,
  foo1,
  foo2,
  generatePoints,
  fillColor,
  filter_f_1,
  filter_f_2, 
  outputBMP,
  zeroArray,
  getBinaryArray)
where 
import Data.Maybe (maybe)
import Control.Monad
import Data.Binary
import System.IO
import qualified Data.ByteString as Byte
import Data.List


import High_pass as H
import Low_pass as L

main :: IO()

main = foo []

filename1 = "temp.bmp"

-- | 'foo' accepts commands till no familiar pattern match at which it gives outputimage
foo :: [Char] -> IO()
foo xs = 
    forever $ do
        x <- getLine
        print x
        h <- openFile "temp.bmp" ReadMode
        content <- Byte.hGetContents h
        let n1 = 54
        let imageData1 = Byte.unpack $ Byte.drop (n1) (content)
        hClose h
        if ((take 4 x) == "Edge")  
            then (H.run 3 "a.bmp" "temp.bmp") 
            else  if ((take 4 x) == "Blur")
                      then (L.run 5 "a.bmp" "temp.bmp")
                      else if ((take 5 x) == "Flood") 
                               then (fillColor (read x :: Commands)) 
                               else (outputBMP [(read x :: Picture)] imageData1)


-- | Handle Operations on the data        
data Commands = Flood Float Float deriving (Read, Show)

-- | Point as a new data type
data Point = Point (Float,Float) deriving (Read, Show)

-- | The Picture Data Type
-- Circle - Center-x, Center-y, Radius (x,y are cartesian co-ordinates)
-- Line - Two tuples of floats (The line between them)
-- Polyline - Line linking a list of points - Can be used to build Square, Rectangle, Triangle or any general Polygon
data Picture = Circle Float Float Float
                | Square Float Float Float Float
                | Line (Float,Float) (Float,Float)
                | Polyline [(Float,Float)]
                | Ellipse Float Float Float Float
                deriving (Show,Read)

-- | Handling Rotations and shift of axes - Function #1
rotateAndShiftPoint :: (Float,Float) -> Float -> Float -> Float -> (Float,Float)
rotateAndShiftPoint (x,y) ang a b = ((((x*(cos deg))-(y*(sin deg)))+a),(((x*(sin deg))+(y*(cos deg)))+b))
                                        where
                                            deg = ang*(pi/180)

-- | Handling Rotations and shift of axes - Function #2
shiftAndRotatePoint :: (Float,Float) -> Float -> Float -> Float -> (Float,Float)
shiftAndRotatePoint (x,y) theta x0 y0 = (((y-y0)*(sin theta) + (x - x0)*(cos theta)), ((y-y0)*(cos theta) - (x - x0)*(sin theta)))
                                        

-- | Supplementary function #1 for generatePoints method - Line case 
foo1 ::  Float -> Float -> Float -> Float -> Float-> (Float, Float)
foo1 y x01 y01 x02 y02 = 
    let
        x1 = if (y01< y02) then x01 else x02
        y1 = if (y01< y02) then y01 else y02
        x2 = if (y01< y02) then x02 else x01
        y2 = if (y01< y02) then y02 else y01
        in
        ((fromIntegral $ round (x1 + (( y))*(sin (atan ((x2-x1)/(y2-y1)))))), (fromIntegral $ round (y1 + (y)*(cos (atan ((x2-x1)/(y2-y1))))) ))

-- | Supplementary function #1 for generatePoints method - Ellipse case
foo2 :: Float -> Float -> Float -> Float ->Float ->Float -> Bool
foo2 a b x0 y0 x y= let
                    r0 = round $ a*a*b*b/(( b*b*(cos (atan ((x-x0)/(y-y0))))*(cos $ atan ((x-x0)/(y-y0)))) + (a*a*(sin $ atan ((x-x0)/(y-y0)))*(sin $ atan ((x-x0)/(y-y0)))))
                    r =  round $ ((x-x0)*(x-x0) + (y-y0)*(y-y0))
                    in
                    ((r <= r0+5)&&(r>=r0-5) )
                    

-- |Supplementary function #1 for outputBMP method
-- Code to generate the array of points in a picture given the picture as input
generatePoints :: Picture -> [(Float,Float)]
generatePoints pict
    | (Circle a b r) <- pict
    = concat[if (((x-b)*(x-b) + (y-a)*(y-a) >= r*r) && ((x-b)*(x-b) + (y-a)*(y-a) <= (r+1)*(r+1)) &&(x<382)&&(y<586)&&(x>=0)&&(y>=0)) then  [(x,y)] else [] | x <- [(b-(r+1))..(b + (r+1))], y<- [(a - (r+1))..(a +(r+1))]]
    | (Square x y deg side) <- pict
    = map (\(x0,y0) -> shiftAndRotatePoint (x0,y0) deg (x+191) (y+283)) $ concat [if ( ((x == side/2.0 ) && ( y <= side/2.0) && (y >= (0-side/2.0))) || ((x == (0-side/2.0) ) && ( y <= side/2.0) && (y >= (0-side/2.0))) || ((y == side/2.0 ) && ( x <= side/2.0) && (x >= (0-side/2.0))) || ((y == (0-side/2.0) ) && ( x <= side/2.0) && (x >= (0-side/2))) ) then [(x,y)] else []  | x <- [ ((0-1) - (fromIntegral (round (side/2)) ))..(0+1 +(fromIntegral (round (side/2))) )], y <- [ (0-1 -(fromIntegral (round (side/2))) )..(0+1 +(fromIntegral (round  (side/2))) )]]
    | (Line (y1,x1) (y2,x2)) <- pict
    = if (x1 == x2) then [(x1,y) | y <- [(min y1 y2) .. (max y1 y2)] ] 
        else ( if (y1 == y2) then [(x,y1) | x <- [(min x1 x2) .. (max x1 x2)]] 
            else (
                    [(foo1 y x1 y1 x2 y2) | y <-[0..(0 + (fromIntegral ( round ( sqrt ((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1))))))]]    ))    --(shiftAndRotatePoint (x0,y0) ((atan ((y2-y1)/(x2-x1)))) (x1) (y1))
    | (Polyline (x:xs)) <- pict
    = if (xs == []) then [x]
        else (concat [generatePoints (Line x (head xs)), generatePoints (Polyline xs)])
    | (Ellipse a b y0 x0) <- pict
    = concat [ if (foo2 a b x0 y0 x y) then [(x,y)] else [] | x <- [(x0 + (0-b))..(x0+b)], y<-[(y0+(0-a))..(y0+a)] ]
    
-- |Supplementary function #2 for outputBMP method
-- Take a list of Pictures and output the vector of raster image data
getBinaryArray :: [Picture] -> [Word8]
getBinaryArray xs = let
            points = (concat (map generatePoints xs))
            in
            concat [if (elem (x,y) points) then [0,0,0] else [255,255,255] | x<-[0..382],y<-[0..567]]

-- |Give an empty array of given length
-- Used to create a blank image
zeroArray :: Int -> [Word8]
zeroArray 0 = []
zeroArray i = 255:(zeroArray (i-1))


-- |Function to write the final bmp image of all the pictures fed by the user
-- The drawing area and hence the size of the bmp image is assumed to be 600*600 pixels
-- Fixed sized image 568 pixels *383 pixels
outputBMP :: [Picture] -> [Word8] -> IO()
outputBMP xs imageData1 =   
        let     
            imageData2 = getBinaryArray xs 
            temp = zeroArray (Prelude.length imageData2)
            imageData = zipWith (min) imageData1 imageData2
            -- to blank the image
            --imageData = zipWith (max) imageData1 temp
            header =[66,77,144,245,9,0,0,0,0,0,54,0,0,0,40,0,0,0,56,2,0,0,127,1,0,0,1,0,24,0,0,0,0,0,90,245,9,0,18,11,0,0,18,11,0,0,0,0,0,0,0,0,0,0]
            image = Byte.concat [Byte.pack header, Byte.pack imageData]
                in do
                    Byte.writeFile "temp.bmp" image


-- | Supplementary function #1 for fillColor method
foo3 :: Commands -> [Word8] ->[(Float, Float)]
foo3 xs imageData
	| (Flood y x) <- xs
	= if ((imageData !! (ceiling (x*568 + y))) == 0 ) then [] else concat[[(x,y)], foo3 (Flood (y) (x-1)) imageData, foo3 (Flood (y) (x+1)) imageData, foo3 (Flood (y+1) (x-1)) imageData,foo3 (Flood (y+1) (x)) imageData,foo3 (Flood (y+1) (x+1)) imageData,foo3 (Flood (y-1) (x-1)) imageData,foo3 (Flood (y-1) (x)) imageData,foo3 (Flood (y-1) (x+1)) imageData]

-- | Supplementary function #2 for fillColor method
filter_f_1 :: (Float,Float) -> [(Float, Float)] ->[Word8]
filter_f_1  (x,y) points =
		if  elem (x,y) points then [0,0,0]
		else [0,0,0]

-- | Supplementary function #3 for fillColor method
filter_f_2 :: (Float,Float) -> [(Float, Float)] ->[Word8]
filter_f_2 (x,y) points =
		if  elem (x,y) points then [0,0,0]
		else [1,1,1]

-- | Splits a list into groups of a given length
-- The output is a list os lists of the given length
groups :: Int -> [a] -> [[a]]
groups n = map (take n) . takeWhile (not . null) . iterate (drop n)		


-- | fillColor fills the figure starting at the point given to it until it encounters a black pixel barrier
-- Uses the "flood fill" algorithm to fill up blank space in the figure from the starting point
-- DFS approach used				
fillColor :: Commands -> IO()
fillColor xs  
    | (Flood y x) <- xs
    =  do
        h <- openFile "temp.bmp" ReadMode
        content <- Byte.hGetContents h
        
        let 
            n1 = 54
            imageData1 = Byte.unpack $ Byte.drop (n1) (content)
            itemp = map (\x -> sum x) (groups 3 imageData1)
            points = foo3 xs itemp
            i1 = concat [filter_f_1 (x,y) points | x<-[0..382],y<-[0..567]]
            i2 = concat [filter_f_2 (x,y) points | x<-[0..382],y<-[0..567]]
            imageData2 = zipWith (+) i1  (zipWith (*) i2 imageData1)
            --imageData2 = concat [if (elem (x,y) points) then [0,255,255] else [(imageData1 !! (ceiling (x*568 + y)*3)),(imageData1 !! (ceiling (x*568 + y)*3+1)),(imageData1 !! (ceiling (x*568 + y)*3 +2))] | x<-[0..382],y<-[0..567]]
            --imageData2 = if (imageData1 !! n2 == 0) then imageData1 else (concat [take n2 imageData1, [255, 0, 0], drop (n2+3) imageData1])
            a = hClose h 
            header =[66,77,144,245,9,0,0,0,0,0,54,0,0,0,40,0,0,0,56,2,0,0,127,1,0,0,1,0,24,0,0,0,0,0,90,245,9,0,18,11,0,0,18,11,0,0,0,0,0,0,0,0,0,0]
            image = Byte.concat [Byte.pack header, Byte.pack imageData2]
            in do
				print imageData2
				print points
				Byte.writeFile "temp.bmp" image
