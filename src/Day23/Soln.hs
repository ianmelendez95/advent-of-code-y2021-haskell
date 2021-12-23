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
            deriving (Eq, Ord, Show)

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

type PosGraph = Map Pos (Set Pos)


inputFile :: FilePath
inputFile = "src/Day23/short-input.txt"


soln :: IO ()
soln = 
  do rooms <- parseInput <$> TIO.readFile inputFile
     let init_state = roomsToState rooms
         (first_pos, first_letter) = head (Map.toList init_state)

     putStrLn "\n[Initial Rooms]"
     mapM_ print rooms

     putStrLn "\n[Initial State]"
     putStrLn $ renderState init_state
     
     putStrLn "\n[Valid Paths]"
     print (first_pos, first_letter) 
     mapM_ print (validLetterPaths first_letter first_pos)


-- nextStates :: PosState -> [PosState]
-- nextStates cur_state = concatMap forPos (letterPositions cur_state)
--   where 
--     forPos :: (Letter, Pos) -> [PosState]
--     forPos (l, p) = _

-- letterPositions :: PosState -> [(Letter, Pos)]
-- letterPositions = map flp . Map.toList
--   where 
--     flp (x,y) = (y,x)

-- letterPaths :: Pos -> Letter -> [(Pos, Int)]
-- letterPaths cur_pos lett = _

-- rawPaths :: Pos -> Letter -> [[Pos]]
-- rawPaths pos lett = _


--------------------------------------------------------------------------------
-- Pos Letter Paths

validLetterPaths :: Letter -> Pos -> [[Pos]]
validLetterPaths lett cur_pos =
  let all_paths = posPaths cur_pos
      cross_paths = filter validCrosses all_paths
   in cross_paths


validCrosses :: [Pos] -> Bool
validCrosses = (== 1) . roomHallCrosses

validLetterPos :: Letter -> PosState -> Set Pos
validLetterPos lett pos_state = 
  let all_possible = possibleLetterPos lett
      unoccupied = Set.filter posUnoccupied all_possible
   in if ownLowerHasWrongLetter 
        then Set.delete (ownRoomUpper lett) unoccupied
        else unoccupied
  where 
    posUnoccupied :: Pos -> Bool
    posUnoccupied = isNothing . (`Map.lookup` pos_state)

    ownLowerHasWrongLetter :: Bool
    ownLowerHasWrongLetter = 
      maybe False (/= lett) (Map.lookup (ownRoomLower lett) pos_state)


roomHallCrosses :: [Pos] -> Int
roomHallCrosses (cur_pos:rest_pos@(next_pos:_))
  | xor cur_is_hall next_is_hall = 1 + roomHallCrosses rest_pos
  | otherwise = roomHallCrosses rest_pos
  where 
    cur_is_hall = Set.member cur_pos hallways
    next_is_hall = Set.member next_pos hallways
roomHallCrosses _ = 0

xor :: Bool -> Bool -> Bool
xor x y = (x || y) && not (x && y)

toSnd :: (a -> b) -> a -> (a,b)
toSnd f x = (x, f x)

--------------------------------------------------------------------------------
-- Pos Predicates

possibleLetterPos :: Letter -> Set Pos
possibleLetterPos = Set.union hallways . ownRooms

ownRooms :: Letter -> Set Pos
ownRooms l = Set.fromList [ownRoomLower l, ownRoomUpper l]

ownRoomLower :: Letter -> Pos
ownRoomLower A = RAL
ownRoomLower B = RBL
ownRoomLower C = RCL
ownRoomLower D = RDL

ownRoomUpper :: Letter -> Pos
ownRoomUpper A = RAU
ownRoomUpper B = RBU
ownRoomUpper C = RCU
ownRoomUpper D = RDU

hallways :: Set Pos
hallways = Set.fromList [HLL, HLR, HML, HMM, HMR, HRL, HRR]

canPartialMoveIntoPos :: Letter -> Pos -> PosState -> Bool
canPartialMoveIntoPos lett pos pos_state = 
  case Map.lookup pos pos_state of 
    Just _ -> False  -- already full, no go
    Nothing -> 
      let is_hall = pos `Set.member` hallways
          is_lower = pos == ownRoomLower lett 
          is_valid_upper = pos == ownRoomUpper lett && ownRoomLowerHasSelfOrEmpty
       in is_hall || is_lower || is_valid_upper
  where 
    ownRoomLowerHasSelfOrEmpty :: Bool
    ownRoomLowerHasSelfOrEmpty =  
      maybe True (== lett) (Map.lookup (ownRoomLower lett) pos_state)


--------------------------------------------------------------------------------
-- Board

posPaths :: Pos -> [[Pos]]
posPaths init_pos = go (Set.singleton init_pos) init_pos
  where 
    go :: Set Pos -> Pos -> [[Pos]]
    go visited pos = 
      let new_neighs = Set.difference (posNeighs pos) visited
       in [pos] : ((pos :) <$> concatMap (go (Set.union new_neighs visited)) new_neighs)

posNeighs :: Pos -> Set Pos
posNeighs = (posGraph Map.!)

posGraph :: PosGraph
posGraph = fromEdges edges
  where 
    fromEdges :: [(Pos, Pos)] -> PosGraph
    fromEdges = foldl' insertEdge Map.empty

    insertEdge :: PosGraph -> (Pos, Pos) -> PosGraph
    insertEdge graph (p1, p2) = 
      let graph' = Map.insertWith Set.union p1 (Set.singleton p2) graph
       in Map.insertWith Set.union p2 (Set.singleton p1) graph'

    edges = 
      [ (RAU, RAL)
      , (RAU, HLR)
      , (RAU, HML)

      , (RBU, RBL)
      , (RBU, HML)
      , (RBU, HMM)

      , (RCU, RCL)
      , (RCU, HMM)
      , (RCU, HMR)

      , (RDU, RDL)
      , (RDU, HMR)
      , (RDU, HRL)

      , (HLL, HLR)
      , (HLR, HML)
      , (HML, HMM)
      , (HMM, HMR)
      , (HMR, HRL)
      , (HRL, HRR)
      ]


--------------------------------------------------------------------------------
-- Parsing

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


--------------------------------------------------------------------------------
-- Render

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