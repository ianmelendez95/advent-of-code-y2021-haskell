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


shortInput = "src/Day17/short-input.txt"
fullInput = "src/Day17/full-input.txt"


soln :: FilePath -> IO ()
soln input_file = do 
  bounds@(x_range, y_range) <- parseInput <$> TIO.readFile input_file
  putStrLn "[Target]"
  putStrLn $ "x: " ++ show x_range
  putStrLn $ "y: " ++ show y_range

  let traj = simulateTrajectory (7, 2) (0, 0)
      traj_in_range = trajectoryInRange bounds traj

      init_vs = initialVels bounds
      results = map (initialVelResult bounds) init_vs

  -- mapM_ print (take 10 traj)
  -- mapM_ print (take 1000 traj_in_range)

  -- mapM_ print (take 100 $ zip init_vs results)
  putStrLn $ "(7, 2): " <> show (initialVelResult bounds (7, 2))
  putStrLn $ "(6, 3): " <> show (initialVelResult bounds (6, 3))
  putStrLn $ "(9, 0): " <> show (initialVelResult bounds (9, 0))
  putStrLn $ "(6, 9): " <> show (initialVelResult bounds (6, 9))
  putStrLn $ "(17, -4): " <> show (initialVelResult bounds (17, -4))

  putStrLn $ "Vels Count: " ++ show (length init_vs)

  -- putStrLn "[Hits]"
  -- mapM_ print $ catMaybes results

  -- putStrLn $ "Answer: " <> show (maximum (catMaybes results))
  putStrLn $ "Answer: " <> show (length (catMaybes results))

    --  let (min_vx, max_vx) = xVelRange x_range
    --      (min_vy, max_vy) = yVelRange min_vx y_range
    --  putStrLn $ "x velocity: " ++ show (min_vx, max_vx)
    --  putStrLn $ "y velocity: " ++ show (min_vy, max_vy)

-- New Impl

initialVelResult :: Bounds -> Vel -> Maybe Int
initialVelResult bounds init_v = 
  let traj = trajectoryInRange bounds $ simulateTrajectory init_v (0, 0)
   in if trajectoryStrikes bounds traj 
        then Just $ trajectoryHeight traj
        else Nothing

initialVels :: Bounds -> [Vel]
initialVels ((x_min, x_max), (y_min, y_max)) = 
  let vx_min = ceiling (sqrt (fromIntegral x_min))
      vx_max = x_max + 1

      vy_min = y_min - 1
      vy_max = abs (y_min - 1)
   in [(vx, vy) | vx <- [vx_min..vx_max], vy <- [vy_min..vy_max]]

trajectoryHeight :: [Point] -> Int
trajectoryHeight = maximum . map snd

trajectoryStrikes :: Bounds -> [Point] -> Bool
trajectoryStrikes ranges = any (pointIsInRanges ranges)
  where
    pointIsInRanges :: Bounds -> Point -> Bool
    pointIsInRanges ((x_min, x_max), (y_min, y_max)) (x, y) = x >= x_min && x <= x_max && y >= y_min && y <= y_max 

trajectoryInRange :: Bounds -> [Point] -> [Point]
trajectoryInRange ranges = takeWhile (not . pointIsPastRanges ranges)
  where
    pointIsPastRanges :: Bounds -> Point -> Bool
    pointIsPastRanges ((_, x_max), (y_min, _)) (x, y) = x > x_max || y < y_min

simulateTrajectory :: Vel -> Point -> [Point]
simulateTrajectory (vx, vy) (x, y) = 
  (x, y) : simulateTrajectory (iterateVelX vx, iterateVelY vy) (x + vx, y + vy)
  where
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

     
