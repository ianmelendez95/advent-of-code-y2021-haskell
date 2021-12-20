{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day20.Soln where 

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
import Control.Monad.Combinators.Expr 

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

traceMsgId :: (Show a) => String -> a -> a
traceMsgId msg x = traceMsgShow msg x x 

traceMsgShow :: (Show a) => String -> a -> b -> b
traceMsgShow msg x = trace (msg ++ ": " ++ show x)

type Parser = Parsec Void T.Text


type Point = (Int, Int)
type Image = Set Point
type Algo = Vector Bool


inputFile :: FilePath
inputFile = "src/Day20/short-input.txt"


soln :: IO ()
soln = 
  do (algo, image) <- parseInput <$> TIO.readFile inputFile

     putStrLn $ showAlgo algo

     putStrLn ""

     putStrLn $ showImage image
  where 
    showAlgo :: Algo -> String
    showAlgo = map (\on -> if on then '#' else '.') . V.toList

    showImage :: Image -> String
    showImage img = 
      let (low_m, low_n) = Set.findMin img
          (high_m, high_n) = Set.findMax img

          rows :: [String]
          rows = map (\r -> map (\c -> if (r,c) `Set.member` img then '#' else '.') [low_n..high_n]) [low_m..high_m]
       in unlines rows

        


cluster :: Point -> [Point]
cluster (cm, cn) = 
  [ (cm - 1, cn - 1) 
  , (cm - 1, cn)
  , (cm - 1, cn + 1)
  , (cm, cn - 1) 
  , (cm, cn)
  , (cm, cn + 1)
  , (cm + 1, cn - 1) 
  , (cm + 1, cn)
  , (cm + 1, cn + 1)
  ]

parseInput :: T.Text -> (Algo, Image)
parseInput input = 
  let (algo_line:_:image_lines) = T.lines input 
      algo = V.fromList (map (== '#') (T.unpack algo_line))
   in (algo, parseImageLines image_lines)

parseImageLines :: [T.Text] -> Set Point
parseImageLines lines = 
  let rows = zip [0..] lines
   in Set.fromList $ concatMap (uncurry parseLine) rows
  where 
    parseLine :: Int -> T.Text -> [Point]
    parseLine row line = map ((row,) . fst) . filter ((== '#') . snd) $ zip [0..] (T.unpack line)

