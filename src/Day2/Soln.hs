{-# LANGUAGE OverloadedStrings #-}


module Day2.Soln where 

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR


inputFile :: FilePath
inputFile = "src/Day2/full-input.txt"

-- displacement = (horizontal, depth)
type Disp = (Int, Int)

type Cmd = (Dir, Int)

data Dir = Fwd 
         | Down
         | Up
    

soln :: IO ()
soln = 
  do content <- TIO.readFile inputFile

     let displacements = map cmdToDisp (parseCmds content)
         (horiz, depth) = foldr (\(x,y) (xs,ys) -> (x + xs, y + ys)) (0,0) displacements

     putStrLn $ "Horizontal: " ++ show horiz
     putStrLn $ "Depth:      " ++ show depth
     putStrLn $ "Product:    " ++ show (horiz * depth)


cmdToDisp :: Cmd -> Disp
cmdToDisp (Fwd, dist)  = (dist, 0)
cmdToDisp (Down, dist) = (0, dist)
cmdToDisp (Up, dist)   = (0, -dist)


parseCmds :: T.Text -> [Cmd]
parseCmds = map parseCmd . T.lines
  where 
    parseCmd :: T.Text -> Cmd
    parseCmd input = 
      let [dir, dist] = T.words input
       in (parseDir dir, parseDecimal dist)

    parseDir :: T.Text -> Dir
    parseDir "forward" = Fwd
    parseDir "down" = Down
    parseDir "up" = Up
    parseDir input = error (T.unpack input)

parseDecimal :: T.Text -> Int
parseDecimal line = 
  case TR.decimal line of 
    Left e -> error e
    Right (val, _) -> val