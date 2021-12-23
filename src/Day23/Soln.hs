{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module Day23.Soln where 

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

import Text.Megaparsec hiding (Pos (..), PosState (..))
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


data Letter = A
            | B
            | C
            | D
            deriving Show

data Pos = RAU -- rooms
         | RAL

         | RBU
         | RBL

         | RCU
         | RCL

         | RDU
         | RDL

         | HLL -- hallways
         | HLR

         | HML
         | HMM
         | HMR

         | HRL
         | HRR
         deriving (Eq, Ord, Show)

         
type PosState = Map Pos Letter


inputFile :: FilePath
inputFile = "src/Day23/short-input.txt"


soln :: IO ()
soln = 
  do rooms <- parseInput <$> TIO.readFile inputFile
     let init_state = roomsToState rooms

     putStrLn "\n[Initial Rooms]"
     mapM_ print rooms

     putStrLn "\n[Initial State]"
     putStrLn $ renderState init_state


roomsToState :: [[Letter]] -> PosState
roomsToState rooms = 
  let room_pos = [RAU, RAL, RBU, RBL, RCU, RCL, RDU, RDL]
   in Map.fromList $ zip room_pos (concat rooms)

parseInput :: T.Text -> [[Letter]]
parseInput input = 
  let rooms_ls = take 2 . drop 2 $ T.lines input
      row_cs = map (map parseLetter . T.unpack . T.filter isLetter) rooms_ls
   in transpose row_cs
  where 
    parseLetter 'A' = A
    parseLetter 'B' = B
    parseLetter 'C' = C
    parseLetter 'D' = D
    parseLetter l = error [l]


renderState :: PosState -> String
renderState pos_state = 
  let ls = 
        [ top
        , hallway
        , roomUpper
        , roomLower
        , bottom
        ]
   in unlines ls
  where 
    top = "#############"

    hallway = 
      [ '#'
      , renderPos HLL
      , renderPos HLR
      , '.'
      , renderPos HML
      , '.'
      , renderPos HMM
      , '.'
      , renderPos HMR
      , '.'
      , renderPos HRL
      , renderPos HRR
      , '#'
      ]
    
    roomUpper =
      [ '#'
      , '#'
      , '#'
      , renderPos RAU
      , '#'
      , renderPos RBU
      , '#'
      , renderPos RCU
      , '#'
      , renderPos RDU
      , '#'
      , '#'
      , '#'
      ]

    roomLower =
      [ ' '
      , ' '
      , '#'
      , renderPos RAL
      , '#'
      , renderPos RBL
      , '#'
      , renderPos RCL
      , '#'
      , renderPos RDL
      , '#'
      , ' '
      , ' '
      ]
    
    bottom = "  #########"

    renderPos :: Pos -> Char
    renderPos pos = maybe '.' letterChar (Map.lookup pos pos_state)
    
    letterChar :: Letter -> Char
    letterChar l = 
      case show l of 
        [c] -> c
        cs -> error cs