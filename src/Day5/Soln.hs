{-# LANGUAGE OverloadedStrings #-}

module Day5.Soln where 

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR

import Data.Ord (comparing)
import Data.List (intercalate, transpose, sort, isSubsequenceOf, find, (\\), maximumBy)
import Data.Maybe (listToMaybe, catMaybes)
import Control.Arrow (Arrow(first))


inputFile :: FilePath
inputFile = "src/Day5/short-input.txt"


soln :: IO ()
soln = 
  do content <- TIO.readFile inputFile
     let segments = parseInput content
         part1_segs = filter horizOrVertSegment segments
     
     putStrLn "[Segments]"
     mapM_ print segments
     putStrLn ""
     putStrLn "[Segments]"
     mapM_ print part1_segs


type Segment = ((Int, Int), (Int, Int))

horizOrVertSegment :: Segment -> Bool
horizOrVertSegment ((x1,y1),(x2,y2)) = x1 == x2 || y1 == y2

parseInput :: T.Text -> [Segment]
parseInput = map parseSegment . T.lines

parseSegment :: T.Text -> Segment
parseSegment line = 
  let [p1,_,p2] = T.words line
   in (parsePoint p1, parsePoint p2)
  where 
    parsePoint :: T.Text -> (Int, Int)
    parsePoint ptext = 
      let [x,y] = map (read . T.unpack) (T.splitOn "," ptext)
       in (x,y)
