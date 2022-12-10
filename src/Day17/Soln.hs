{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day17.Soln where 

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR

import Data.Ord
import Data.List
import Data.Maybe
import Data.Char

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Set (Set)
import qualified Data.Set as Set

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Control.Monad.State.Lazy

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void

import Debug.Trace

type Bounds = (Range, Range)
type Range  = (Int, Int)
type Vel    = (Int, Int)
type Point  = (Int, Int)

data MoveResult = Towards | Away | Inside

type Parser = Parsec Void T.Text


inputFile = "src/Day17/short-input.txt"


soln :: FilePath -> IO ()
soln input_file = 
  do ranges@(x_range, y_range) <- parseInput <$> TIO.readFile input_file
     putStrLn "[Target]"
     putStrLn $ "x: " ++ show x_range
     putStrLn $ "y: " ++ show y_range

     let traj = simulateTrajectory (7, 2) (0, 0)
         traj_with_inrange = map (\p -> (p, pointIsPastRanges ranges p)) traj
         traj_in_range = trajectoryInRange ranges traj

     mapM_ print (take 10 traj)
     mapM_ print (take 10 traj_with_inrange)
     mapM_ print (take 1000 traj_in_range)

    --  let (min_vx, max_vx) = xVelRange x_range
    --      (min_vy, max_vy) = yVelRange min_vx y_range
    --  putStrLn $ "x velocity: " ++ show (min_vx, max_vx)
    --  putStrLn $ "y velocity: " ++ show (min_vy, max_vy)

-- New Impl

trajectoryStrikes :: (Range, Range) -> [Point] -> Bool
trajectoryStrikes ranges = any (pointIsInRanges ranges)

trajectoryInRange :: (Range, Range) -> [Point] -> [Point]
trajectoryInRange ranges = takeWhile (not . pointIsPastRanges ranges)

pointIsInRanges :: (Range, Range) -> Point -> Bool
pointIsInRanges ((x_min, x_max), (y_min, y_max)) (x, y) = x > x_min && x < x_max && y > y_min && y < y_max 

pointIsPastRanges :: (Range, Range) -> Point -> Bool
pointIsPastRanges ((_, x_max), (y_min, _)) (x, y) = x > x_max || y < y_min

simulateTrajectory :: Vel -> Point -> [Point]
simulateTrajectory (vx, vy) (x, y) = 
  (x, y) : simulateTrajectory (iterateVelX vx, iterateVelY vy) (x + vx, y + vy)

iterateVelX :: Int -> Int
iterateVelX vx 
  | vx > 0 = vx - 1
  | vx < 0 = vx + 1
  | otherwise = vx

iterateVelY :: Int -> Int
iterateVelY vy = vy - 1
    
-- Old Impl

xVelRange :: Range -> Range
xVelRange (x_beg, x_end) = 
  let trim_x_beg = dropWhile ((< x_beg) . snd) (zip [1..] (scanl (+) 1 [2..]))
      vx_start = head trim_x_beg

      (x_middle, trim_x_end) = span ((<= x_end) . snd) trim_x_beg
      vx_end = last x_middle
   in (fst vx_start, fst vx_end)

yVelRange :: Int -> Range -> Range
yVelRange vx (y_beg, y_end) = 
  let vys = map (yAtXZeroVel vx) [1..]
      
      trim_y_beg = dropWhile ((< y_beg) . snd) (zip [1..] vys)
      vy_start = head trim_y_beg

      (y_middle, trim_y_end) = span ((<= y_end) . snd) trim_y_beg
      vy_end = last y_middle
   in (fst vy_start, fst vy_end)

yAtXZeroVel :: Int -> Int -> Int
yAtXZeroVel vx vy = s vy - s (vy - vx)
  where 
    s :: Int -> Int
    s n
      | n >= 0    = ((n ^ 2) + n) `div` 2 
      | otherwise = s (-(n + 1))

maxY :: Int -> Int -> Int
maxY vx vy = r vy - r (vy - vx)
  where 
    r :: Int -> Int
    r n 
      | n >= 0    = ((n ^ 2) + n) `div` 2 
      | otherwise = 0


parseInput :: T.Text -> (Range,Range)
parseInput input = 
  do case parse rangeInput "" input of 
       Left err -> error (errorBundlePretty  err) 
       Right res -> res

rangeInput :: Parser ((Int,Int), (Int,Int))
rangeInput = 
  do x_start <- string "target area: x=" >> component
     x_end   <- string ".."              >> component
     y_start <- string ", y="            >> component
     y_end   <- string ".."              >> component
     pure ((x_start,x_end),(y_start,y_end))
  where 
    component :: Parser Int
    component = L.signed space L.decimal

     
