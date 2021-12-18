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

type Range = (Int,Int)

type Parser = Parsec Void T.Text


inputFile = "src/Day17/short-input.txt"


soln :: IO ()
soln = 
  do (x_range, y_range) <- parseInput <$> TIO.readFile inputFile
     putStrLn "[Target]"
     putStrLn $ "x: " ++ show x_range
     putStrLn $ "y: " ++ show y_range

     let (min_vx, max_vx) = xVelRange x_range
         (min_vy, max_vy) = yVelRange min_vx y_range
     putStrLn $ "x velocity: " ++ show (min_vx, max_vx)
     putStrLn $ "y velocity: " ++ show (min_vy, max_vy)




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

     
