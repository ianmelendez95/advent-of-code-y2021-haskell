{-# LANGUAGE OverloadedStrings #-}

module Day5.Soln where 

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR

import Data.Ord (comparing)
import Data.List (intercalate, transpose, sort, isSubsequenceOf, find, (\\), maximumBy)
import Data.Maybe (listToMaybe, catMaybes)
import Control.Arrow (Arrow(first))

import qualified Data.Map.Strict as Map


inputFile :: FilePath
inputFile = "src/Day5/full-input.txt"


soln :: IO ()
soln = 
  do content <- TIO.readFile inputFile
     let segments = parseInput content
         p1_segs = filter horizOrVertSegment segments
         p1_seg_points = map segmentPoints p1_segs

         point_counts = countPoints (concat p1_seg_points)

         p1_overlap_points = overlapPoints point_counts

    --  putStrLn "[Segments]"
    --  mapM_ print segments
    --  putStrLn ""
    --  putStrLn "[Part 1 Segments]"
    --  mapM_ print p1_segs
    --  putStrLn ""
    --  putStrLn "[Part 1 Segment Points]"
    --  mapM_ print p1_seg_points
    --  putStrLn ""
    --  putStrLn "[Part 1 Overlap Points]"
    --  mapM_ print p1_overlap_points
     putStrLn ""
     putStrLn $ "Count: " ++ show (length p1_overlap_points)



--------------------------------------------------------------------------------
-- Counter

type PointCounter = Map.Map (Int,Int) Int

overlapPoints :: PointCounter -> [(Int,Int)]
overlapPoints = map fst . filter ((> 1) . snd) . Map.toList

countPoints :: [(Int,Int)] -> PointCounter
countPoints = foldr countPoint Map.empty

countPoint :: (Int,Int) -> PointCounter -> PointCounter
countPoint point = Map.insertWith (+) point 1


--------------------------------------------------------------------------------
-- Segments

type Segment = ((Int, Int), (Int, Int))

segmentPoints :: Segment -> [(Int,Int)]
segmentPoints seg@((x1,y1),(x2,y2))
  | x1 == x2 = zip (repeat x1) [(min y1 y2)..(max y1 y2)]
  | y1 == y2 = zip [(min x1 x2)..(max x1 x2)] (repeat y1)
  | otherwise = error $ "Not considering diagonal lines: " ++ show seg 

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
