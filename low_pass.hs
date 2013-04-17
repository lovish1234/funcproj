{-# LANGUAGE PackageImports, BangPatterns, QuasiQuotes, PatternGuards, ScopedTypeVariables #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}



import Data.List
import Control.Monad
import System.Environment

import Data.Word
import Data.Array.Repa.IO.BMP
import Data.Array.Repa 			        as A
import qualified Data.Array.Repa.Repr.Unboxed   as U
import Data.Array.Repa.Stencil		        as A
import Data.Array.Repa.Stencil.Dim2	        as A
import Prelude				        as P



-- input taken from a file and output to a file (both in bmp format)

main 
 = do	args	<- getArgs
	case args of
	 [iterations, fileIn, fileOut]	-> run (read iterations) fileIn fileOut
	 _				-> usage

usage 	= putStr $ unlines
	[ "./ <iterations::Int> <fileIn.bmp> <fileOut.bmp>" ]


run :: Int -> FilePath -> FilePath -> IO ()
run iterations fileIn fileOut
 = do	arrRGB	<- liftM (either (error . show) id) 
		$  readImageFromBMP fileIn

	arrRGB `deepSeqArray` return ()
	let (arrRed, arrGreen, arrBlue) = U.unzip3 arrRGB
	let comps                       = [arrRed, arrGreen, arrBlue]
		
	(comps')
	 <-  P.mapM (process iterations) comps
	
        let [arrRed', arrGreen', arrBlue'] = comps'
	writeImageToBMP fileOut
	        (U.zip3 arrRed' arrGreen' arrBlue')


-- adopted from repa library {process,promote,demote}
process	:: Monad m => Int -> Array U DIM2 Word8 -> m (Array U DIM2 Word8)
process iterations 
        = promote >=> blur iterations >=> demote


	
promote	:: Monad m => Array U DIM2 Word8 -> m (Array U DIM2 Double)
promote arr
 = computeP $ A.map ffs arr
 where	{-# INLINE ffs #-}
	ffs	:: Word8 -> Double
	ffs x	=  fromIntegral (fromIntegral x :: Int)
{-# NOINLINE promote #-}


demote	:: Monad m => Array U DIM2 Double -> m (Array U DIM2 Word8)
demote arr
 = computeP $ A.map ffs arr

 where	{-# INLINE ffs #-}
	ffs 	:: Double -> Word8
	ffs x	=  fromIntegral (truncate x :: Int)
{-# NOINLINE demote #-}



-- blur keeping in mind the iterations are non-lazy.

blur 	:: Monad m => Int -> Array U DIM2 Double -> m (Array U DIM2 Double)
blur !iterations arrInit
 = go iterations arrInit
 where  go !0 !arr = return arr
        go !n !arr  
 	 = do   arr'    <- computeP
                         $ A.smap (/ 159)
                         $ forStencil2 BoundClamp arr
-- covolution with 2-D Gaussian Stencil

                           [stencil2|   2  4  5  4  2
                                        4  9 12  9  4
                                        5 12 15 12  5
                                        4  9 12  9  4
                                        2  4  5  4  2 |]
                go (n-1) arr'















			
