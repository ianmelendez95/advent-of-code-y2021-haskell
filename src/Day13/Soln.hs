{-# LANGUAGE OverloadedStrings #-}

module Day13.Soln where 

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR

import Data.Ord (comparing)
import Data.List
import Data.Maybe (maybe, fromMaybe, listToMaybe, catMaybes, mapMaybe)
import Data.Char

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Set (Set)
import qualified Data.Set as Set

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Control.Monad.State.Lazy

import Debug.Trace

type Point = (Int, Int)

data Fold = FoldX Int
          | FoldY Int
          deriving Show


inputFile :: FilePath
inputFile = "src/Day13/full-input.txt"

soln :: IO ()
soln = 
  do (points, folds) <- parseInput <$> TIO.readFile inputFile

     let point_set = Set.fromList points
         folded_points = tail $ scanl' (flip foldPoints) point_set folds
         folds_with_points = zip folds (tail folded_points)

    --  putStrLn "[Points]"
    --  mapM_ print points
    --  putStrLn $ "Point Set Count: " ++ show (Set.size point_set)
    --  printPoints point_set

    --  putStrLn "\n[Folds]"
    --  mapM_ print folds

    --  mapM_ printFoldResult (take 1 folds_with_points)
    --  putStrLn $ "First Fold Count: " ++ show (Set.size (head folded_points))
     printFoldResult (last folds_with_points)
  where 
    printFoldResult :: (Fold, Set Point) -> IO ()
    printFoldResult (fold, points)= 
      do putStrLn $ "\n[" ++ show fold ++ "]"
         putStrLn $ "Point Count: " ++ show (Set.size points)
         printPoints points


foldPoints :: Fold -> Set Point -> Set Point
foldPoints fold = Set.map (foldPoint fold)


foldPoint :: Fold -> Point -> Point
foldPoint (FoldX fx) p@(x,y) 
  | x <= fx = p
  | otherwise = (fx - (x - fx), y)
foldPoint (FoldY fy) p@(x,y) 
  | y <= fy = p
  | otherwise = (x, fy - (y - fy))


printPoints :: Set Point -> IO ()
printPoints point_set = 
  let x_bound = maximum (map fst (Set.toList point_set))
      y_bound = maximum (map snd (Set.toList point_set))
   in mapM_ (printLine x_bound) [0..y_bound]
  where 
    printLine :: Int -> Int -> IO ()
    printLine x_bound y = 
      do mapM_ (`printPoint` y) [0..x_bound]
         putStrLn ""

    printPoint :: Int -> Int -> IO ()
    printPoint x y = 
      if Set.member (x,y) point_set 
        then putStr "#"
        else putStr " "


parseInput :: T.Text -> ([Point], [Fold])
parseInput input = 
  let (points, folds) = break T.null (T.lines input)
   in (map parsePoint points, map parseFold (tail folds))
  where 
    parsePoint :: T.Text -> Point
    parsePoint point_line = 
      let [x,y] = map (read . T.unpack) (T.splitOn "," point_line)
       in (x,y)
    
    parseFold :: T.Text -> Fold
    parseFold fold_line = 
      let [comp, val] = T.splitOn "=" (last (T.words fold_line))
       in case comp of 
            "x" -> FoldX (read (T.unpack val))
            "y" -> FoldY (read (T.unpack val))
            c -> error $ "Unknown input: " ++ show c
        

