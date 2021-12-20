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
import System.Posix.Internals (o_RDWR)

traceMsgId :: (Show a) => String -> a -> a
traceMsgId msg x = traceMsgShow msg x x 

traceMsgShow :: (Show a) => String -> a -> b -> b
traceMsgShow msg x = trace (msg ++ ": " ++ show x)

type Parser = Parsec Void T.Text


inputFile = "src/Day20/short-input.txt"


soln :: IO ()
soln = 
  do (algo, image) <- readInput <$> TIO.readFile inputFile

     TIO.putStrLn algo
     putStrLn ""
     mapM_ TIO.putStrLn image


triples :: T.Text -> [T.Text]
triples txt = 
  let triple = T.take 3 txt
   in if T.length triple == 3 
        then triple : triples (T.tail txt)
        else []

readInput :: T.Text -> (T.Text, [T.Text])
readInput input = 
  let (algo:_:image) = T.lines input 
   in (algo, image)
