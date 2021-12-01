module Day1.Soln where 

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR

inputFile :: FilePath
inputFile = "src/Day1/full-input.txt"

soln :: IO ()
soln = do
  content <- TIO.readFile inputFile
  let increases = countIncreases (parseInput content)
  print increases


countIncreases :: [Int] -> Int
countIncreases [] = 0
countIncreases [_] = 0
countIncreases (x:y:xs) = (if x < y then 1 else 0) + countIncreases (y:xs)


parseInput :: T.Text -> [Int]
parseInput = map parseLine . T.lines

parseLine :: T.Text -> Int
parseLine line = 
  case TR.decimal line of 
    Left e -> error e
    Right (val, _) -> val


readInput :: IO T.Text
readInput = TIO.readFile "src/Day1/short-input.txt"


