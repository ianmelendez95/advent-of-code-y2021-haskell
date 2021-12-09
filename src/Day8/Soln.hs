{-# LANGUAGE OverloadedStrings #-}

module Day8.Soln where 

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR

import Data.Ord (comparing)
import Data.List (foldl1', intercalate, transpose, sort, isSubsequenceOf, find, (\\), maximumBy, permutations)
import Data.Maybe (fromMaybe, listToMaybe, catMaybes, mapMaybe)
import Data.Char

import qualified Data.Map.Strict as Map

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import qualified Data.Set as Set

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Control.Monad.State.Lazy

type DNum = Int
type DNums = IntSet



type Seg = Int
type Segs = [Int]

type Wire = Int
type Wires = [Int] 
type WireToSeg = IntMap Seg


inputFile :: FilePath
inputFile = "src/Day8/full-input.txt"


soln :: IO ()
soln = 
  do all_wires <- map parseInput . T.lines <$> TIO.readFile inputFile
     let entry1@(input_wires1, output_wires1) = head all_wires

    --  putStrLn $ "Output: " ++ show (entryToValue entry1)
    --  putStrLn $ "Short Result: " ++ show (sum (map entryToValue (tail all_wires)))
    --  mapM_ (print . entryToValue) all_wires
     putStrLn $ "Full Result: " ++ show (sum (map entryToValue all_wires))
     

  where 
    entryToValue :: ([Wires], [Wires]) -> Int
    entryToValue entry = 
      let nums = entryToNums entry
       in sum $ zipWith (*) (reverse nums) (map (10^) [0..])

    entryToNums :: ([Wires], [Wires]) -> [DNum]
    entryToNums (input_wires, output_wires) = 
      let wire_to_seg = identifyWireToSeg input_wires
       in map (wiresToNum wire_to_seg) output_wires


--------------------------------------------------------------------------------
-- Try Approach
  
identifyWireToSeg :: [Wires] -> WireToSeg
identifyWireToSeg wiress = 
  let poss_wires_to_segs = genWiresToSegs
   in head $ mapMaybe (\wires_to_segs -> wires_to_segs <$ mAllWiresToNum wires_to_segs wiress) genWiresToSegs
      

-- wiresToNum :: WireToSeg -> Wires -> DNum
-- wiresToNum map wires = 
--   fromMaybe (error $ "No valid number: " ++ show map ++ " - " ++ show wires) (mWiresToNum map wires)

mAllWiresToNum :: WireToSeg -> [Wires] -> Maybe [DNum]
mAllWiresToNum map = traverse (mWiresToNum map)


wiresToNum :: WireToSeg -> Wires -> DNum
wiresToNum wire_map wires = 
  fromMaybe (error $ "No number for wires: " ++ show wires ++ " - " ++ show wire_map) (mWiresToNum wire_map wires)

mWiresToNum :: WireToSeg -> Wires -> Maybe DNum
mWiresToNum map wires = mSegsToNum (wiresToSegs map wires)

wiresToSegs :: WireToSeg -> Wires -> Segs
wiresToSegs wire_map = map (`usLookup` wire_map)


mSegsToNum :: Segs -> Maybe DNum
mSegsToNum segs = 
  let segs_set = IntSet.fromList segs
      num_to_segs = IntMap.toList numToSegsMap
   in fst <$> find ((== segs_set) . snd) num_to_segs

numToSegsMap :: IntMap IntSet
numToSegsMap = IntMap.fromList (zip [0..] (map charsToSegs alphaSegs))
  where 
    alphaSegs = [ "abcefg"
                , "cf"
                , "acdeg"
                , "acdfg"
                , "bcdf"
                , "abdfg"
                , "abdefg"
                , "acf"
                , "abcdefg"
                , "abcdfg"
                ]

    charsToSegs :: String -> IntSet
    charsToSegs chars = IntSet.fromList $ map segCharToInt chars 


genWiresToSegs :: [WireToSeg]
genWiresToSegs = map (IntMap.fromList . zip [0..]) (permutations (take 7 [0..]))

segIntToChar :: Seg -> Char
segIntToChar seg_int = chr (seg_int + ord 'a')


usLookup :: Int -> IntMap a -> a
usLookup k m = 
  case IntMap.lookup k m of 
    Nothing -> error $ "No value for key: " ++ show k
    Just v -> v

    


countEasydisps :: [T.Text] -> Int
countEasydisps = length . filter id . map isEasySeg

-- [1, 4, 7, 8]
isEasySeg :: T.Text -> Bool
isEasySeg seg = T.length seg `elem` [2, 4, 3, 7]


parseInput :: T.Text -> ([Wires], [Wires])
parseInput line = 
  let [input_wires, output_wires] = map (map parseWires . T.words) (T.splitOn "|" line)
   in (input_wires, output_wires)
  where 
    parseWires :: T.Text -> Wires
    parseWires = map segCharToInt . T.unpack


segCharToInt :: Char -> Int
segCharToInt c = ord c - ord 'a'
