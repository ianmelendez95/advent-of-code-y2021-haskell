{-# LANGUAGE OverloadedStrings #-}

module Day3.Soln where 

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR


import Data.List (foldl', transpose)


inputFile :: FilePath
inputFile = "src/Day3/full-input.txt"


soln :: IO ()
soln = 
  do content <- TIO.readFile inputFile

     let bytes :: [[Bool]]
         bytes = map readByte (T.lines content) 

         -- gamma
         common_bits = map mostCommonBit (transpose bytes)

         -- epsilon
         uncommon_bits = map not common_bits

         byte_trie = trieFromList bytes

         oxygen_bits = trieCommonPath byte_trie
         co2_bits = trieUncommonPath byte_trie

     putStrLn   "[Gamma]"
     putStrLn $ "Binary:  " ++ showBits common_bits
     putStrLn $ "Decimal: " ++ show (bitsToDecimal common_bits)
     putStrLn ""
     putStrLn   "[Epsilon]"
     putStrLn $ "Binary:  " ++ showBits uncommon_bits
     putStrLn $ "Decimal: " ++ show (bitsToDecimal uncommon_bits)
     putStrLn ""
     putStrLn   "[Answer - Part 1]"
     print      (bitsToDecimal uncommon_bits * bitsToDecimal common_bits)
     putStrLn ""
     putStrLn   "[Oxygen]"
     putStrLn $ "Binary:  " ++ showBits oxygen_bits
     putStrLn $ "Decimal: " ++ show (bitsToDecimal oxygen_bits)
     putStrLn ""
     putStrLn   "[CO2]"
     putStrLn $ "Binary:  " ++ showBits co2_bits
     putStrLn $ "Decimal: " ++ show (bitsToDecimal co2_bits)
     putStrLn ""
     putStrLn   "[Answer - Part 2]"
     print      (bitsToDecimal oxygen_bits * bitsToDecimal co2_bits)
    --  putStrLn $ showTrie byte_trie

readBytes :: IO [[Bool]]
readBytes = 
  do content <- TIO.readFile inputFile
     pure $ map readByte (T.lines content)

showBits :: [Bool] -> String
showBits = map (\b -> if b then '1' else '0') 

bitsFromString :: String -> [Bool]
bitsFromString = map (\c -> if c == '0' then False else True)


bitsToDecimal :: [Bool] -> Int
bitsToDecimal bits = 
  let bit_vals = map (\b -> if b then 1 else 0) bits
      pows_2 = map (2^) [0..]
   in sum (zipWith (*) (reverse bit_vals) (map (2^) [0..]))



mostCommonBit :: [Bool] -> Bool
mostCommonBit byte = countTrues byte > (length byte `div` 2)

countTrues :: [Bool] -> Int
countTrues = length . filter id


readByte :: T.Text -> [Bool]
readByte = map readBit . T.unpack
  where 
    readBit :: Char -> Bool
    readBit '0' = False
    readBit '1' = True
    readBit c = error $ "Unrecognized bit char: " ++ show c


--------------------------------------------------------------------------------
-- Byte Trie


data Trie = Node Int Trie Trie
          | Leaf [Bool]
          | Empty

emptyTrie :: Trie
emptyTrie = Empty

singletonTrie :: [Bool] -> Trie
singletonTrie = Leaf

trieFromList :: [[Bool]] -> Trie
trieFromList [] = Empty
trieFromList (x:xs) = foldr insertTrie (singletonTrie x) xs

trieSize :: Trie -> Int
trieSize Empty = 0
trieSize (Leaf _) = 1
trieSize (Node s _ _) = s

trieCommonPath :: Trie -> [Bool]
trieCommonPath Empty = []
trieCommonPath (Leaf bs) = bs
trieCommonPath (Node _ zero one) 
  | trieSize one >= trieSize zero = True : trieCommonPath one
  | otherwise                     = False : trieCommonPath zero

trieUncommonPath :: Trie -> [Bool]
trieUncommonPath Empty = []
trieUncommonPath (Leaf bs) = bs
trieUncommonPath (Node _ zero one) 
  | trieSize zero <= trieSize one = False : trieUncommonPath zero
  | otherwise                     = True : trieUncommonPath one

insertTrie :: [Bool] -> Trie -> Trie
insertTrie bytes = traverse bytes 
  where 
    traverse :: [Bool] -> Trie -> Trie

    traverse bs Empty = Leaf bs

    traverse [] (Node size zero one) = error $ "Fully traversed to node: " ++ showBits bytes
    traverse [] l@(Leaf _) = l

    traverse (b:bs) (Node size zero one) 
      | b         = Node (size + 1) zero (traverse bs one)
      | otherwise = Node (size + 1) (traverse bs zero) one

    traverse (b:bs) (Leaf (b':bs'))
      -- both into same branch
      | b && b'             = Node 2 Empty (traverse bs (singletonTrie bs'))
      | (not b) && (not b') = Node 2 (traverse bs (singletonTrie bs')) Empty

      -- both in opposite branches
      | b && (not b') = Node 2 (singletonTrie bs') (singletonTrie bs)
      | (not b) && b' = Node 2 (singletonTrie bs)  (singletonTrie bs')

      | otherwise = error $ "Conditions should be exhausted, original input: " ++ showBits bytes
    
    traverse (b:bs) (Leaf []) = 
      error $ "Encountered empty leaf with non empty bytes! " ++ showBits bytes ++ " : " ++ showBits (b:bs)

showTrie :: Trie -> String
showTrie = unlines . doShow 
  where 
    doShow :: Trie -> [String]
    doShow (Empty) = ["EMPTY"] 
    doShow (Leaf byte) = [showBits byte]
    doShow (Node size zero one) = 
      let lines = 
               ["0 [" ++ show (trieSize zero) ++ "]"] 
            ++ map ("| " ++) (doShow zero)
            ++ ["1 [" ++ show (trieSize one) ++ "]"] 
            ++ map ("| " ++) (doShow one)
            ++ [""]
       in lines
