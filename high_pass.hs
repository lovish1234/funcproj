{-# LANGUAGE PackageImports, BangPatterns, QuasiQuotes, PatternGuards, ScopedTypeVariables #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

import System.Environment
import Prelude				hiding (compare)
import Control.Monad



import Data.Array.Repa 			        as R
import Data.Array.Repa.Algorithms.Pixel         as R
import Data.Array.Repa.Stencil          as R
import Data.Array.Repa.Stencil.Dim2     as R


import Data.Array.Repa.IO.BMP


type Image	= Array U DIM2 Float

-- sobel horizontal edge stencil template
gradientX :: Monad m => Image -> m Image
gradientX img
 	= computeP
 	$ forStencil2 (BoundConst 0) img
	  [stencil2|	-1  0  1
			-2  0  2
			-1  0  1 |]

-- sobel vertical edge stencil emplate
gradientY :: Monad m => Image -> m Image
gradientY img
	= computeP
	$ forStencil2 (BoundConst 0) img
	  [stencil2|	 1  2  1
			 0  0  0
			-1 -2 -1 |] 

-- to calculate modulus of a function
modI :: Float -> Float -> Float 

modI x y
        = sqrt (x * x + y * y)
{-

Helper function
-}
go :: Int -> Image -> IO (Image, Image)
go n img

 = img `deepSeqArray`
   if n == 0
    then return (img, img)
    else do 
	gX      <- gradientX img
	gY	<- gradientY img	
	if (n == 1) 
		then return (gX, gY)
		else go (n - 1) img

main 
 = do	args	<- getArgs
	case args of
	 [iterations, fileIn, fileOut]	
		-> run (read iterations) fileIn fileOut
	 _	-> putStrLn "Use: h_pass itr inp outp"



run iterations fileIn fileOut
 = do	-- Load the source image and convert it to greyscale
        inputImage 	<- liftM (either (error . show) id) 
			$ readImageFromBMP fileIn

        (greyImage :: Array U DIM2 Float)
                        <- computeP
                        $  R.map floatLuminanceOfRGB8 inputImage
		
	(gX, gY)
	               <-  go iterations greyImage

	--putStr $ prettyTime tElapsed
	
	outImage       <- computeUnboxedP
	               $  R.map rgb8OfGreyFloat  
                       $  R.map (/ 3)
	               $  R.zipWith modI gX gY	

	writeImageToBMP fileOut outImage


