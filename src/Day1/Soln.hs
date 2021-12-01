module Day1.Soln where 

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR


inputFile :: FilePath
inputFile = "src/Day1/full-input.txt"


soln :: IO ()
soln = do
  content <- TIO.readFile inputFile
  let increases = countWindowIncreases (parseDepths content)
  print increases


countWindowIncreases :: [Int] -> Int
countWindowIncreases (x:rest@(_:_:y:_)) = (if x < y then 1 else 0) + countWindowIncreases rest
countWindowIncreases _ = 0


parseDepths :: T.Text -> [Int]
parseDepths = map parseLine . T.lines

parseLine :: T.Text -> Int
parseLine line = 
  case TR.decimal line of 
    Left e -> error e
    Right (val, _) -> val


