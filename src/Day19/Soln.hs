{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day19.Soln where 

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

traceMsgId :: (Show a) => String -> a -> a
traceMsgId msg x = traceMsgShow msg x x 

traceMsgShow :: (Show a) => String -> a -> b -> b
traceMsgShow msg x = trace (msg ++ ": " ++ show x)

type Parser = Parsec Void T.Text


type Point3 = (Int,Int,Int)
type SensorMap = [Point3]

inputFile = "src/Day19/short-input.txt"


soln :: IO ()
soln = 
  do sensor_maps <- parseInput <$> TIO.readFile inputFile
    --  print sensor_maps
     mapM_ (\m -> putStrLn "" >> mapM_ print m) sensor_maps


orientAxes :: Point3 -> [Point3]
orientAxes = concatMap shiftAxes . negateAxes

shiftAxes :: Point3 -> [Point3]
shiftAxes (x,y,z) = 
  [ (x,y,z)
  , (z,x,y)
  , (y,z,x)
  ]

negateAxes :: Point3 -> [Point3]
negateAxes (x,y,z) = 
  [ (x,y,z)
  , (x,y,-z)
  , (x,-y,z)
  , (x,-y,-z)
  , (-x,y,z)
  , (-x,y,-z)
  , (-x,-y,z)
  , (-x,-y,-z)
  ]


--------------------------------------------------------------------------------
-- Parser

parseInput :: T.Text -> [SensorMap]
parseInput input = 
  case parse sensorMaps "" input of 
    Left e -> error (errorBundlePretty e)
    Right r -> r

sensorMaps :: Parser [SensorMap]
sensorMaps = many sensorMap

sensorMap :: Parser SensorMap
sensorMap = (sensorHeader >> some sensorPoint) <* optional eol
     
sensorHeader :: Parser ()
sensorHeader = () <$ (string "--- " >> manyTill anySingle eol)

sensorPoint :: Parser Point3
sensorPoint = 
  do [x,y,z] <- sepBy1 (L.signed space L.decimal) (char ',') <* optional eol
     pure (x,y,z)
     
signedInt :: Parser Int
signedInt = L.signed space L.decimal


