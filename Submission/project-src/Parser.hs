{-# LANGUAGE PatternGuards #-}
-- | Simple picture drawing application.
module Parser
  (func,
   index,
   parser,
   parseIO)   
where
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Data.Maybe (maybe)
import Data.List
import Debug.Trace
import System.IO

-- | Asserts true if no bracket found in the input char.
func :: Char -> Bool
func x = if ((x=='(') || (x==')'))
            then False
            else True

-- | Index of an element contained in a list
index :: String -> [String] -> Int
index s str = let
                indexAux :: String -> [String] -> Int -> Int
                indexAux s str i = case str of
                                    x:xs -> if (x==s)
                                                then i
                                                else indexAux s xs (i+1)
                in
                    indexAux s str 0
 

-- | Circle -0
--   Line - 1
--   Else - 2
--   
parser :: String -> String
parser str = let 
                list = words $ filter func str      -- removes brackets 
                parserAux list int orig_str =
                    if int == 0
                    then
                        if (("Scale" `elem` list) == True)
                        then
                            -- Ellipse
                            let
                                x = list !! ( (index "Translate" list)+1 )
                                y = list !! ( (index "Translate" list)+2 )
                                rad = list !! ( (index "Circle" list)+1 )
                                sca_x = list !! ( (index "Scale" list)+1 )
                                sca_y = list !! ( (index "Scale" list)+2 )
                                deg = list !! ( (index "Rotate" list)+1 )
                                b = show $ (read rad :: Float) * (read sca_x :: Float)
                            in
                                "Ellipse (" ++ x ++"," ++ y ++"," ++ rad ++ "," ++ deg ++ "," ++ b ++")\n"
                        else
                            -- Circle
                            let
                                x = list !! ( (index "Translate" list)+1 )
                                y = list !! ( (index "Translate" list)+2 )
                                rad = list !! ( (index "Circle" list)+1 )
                            in 
                                "Circle (" ++ x ++"," ++ y ++"," ++ rad ++")\n"
                    else if int == 1
                        then if (("Rotate" `elem` list) == True)
                            then 
                                -- Rectangle
                                let
                                    x = list !! ( (index "Translate" list)+1 )
                                    y = list !! ( (index "Translate" list)+2 )
                                    deg = list !! ( (index "Rotate" list)+1 )
                                    pointlist = read ( list !! ( (index "Line" list)+1 ) ) :: [Float]
                                    width = show $ max ( abs ((pointlist !! 0) - (pointlist !! 2)) ) ( abs ((pointlist !! 1) - (pointlist !! 3)) )
                                    height = show $ max ( abs ((pointlist !! 2) - (pointlist !! 4)) ) ( abs ((pointlist !! 3) - (pointlist !! 5)) )
                                in
                                "Rectangle (" ++ x ++"," ++ y ++"," ++ deg ++ "," ++ width ++ "," ++ height ++")\n"
                            else
                                -- Polyline
                                "Polyline " ++ (drop 5 orig_str) ++ "\n"
                                
                        else str ++ "\n"   -- if (int==2)
                in
                    if (("Circle" `elem` list) == True)
                    then parserAux list 0 str
                    else if (("Line" `elem` list) == True)
                        then parserAux list 1 str
                        else parserAux list 2 str

-- | Reads file contents of "Gloss_lang.txt" and invokes parser on each line.
parseIO :: IO ()
parseIO = do 
           filecont <- readFile "Gloss_lang.txt"
        -- str = lines filecont -- "Translate (-129.0) 100.0 (Rotate 0.0 (Scale 0.98300606 0.18357341 (Circle 168.86977)))"
           writeFile "out.txt" (concat $ map parser (lines filecont))
main = parseIO
