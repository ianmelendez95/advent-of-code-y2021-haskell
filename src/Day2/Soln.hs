{-# LANGUAGE OverloadedStrings #-}


module Day2.Soln where 

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR


import Data.List (foldl')


inputFile :: FilePath
inputFile = "src/Day2/full-input.txt"

-- displacement = (horizontal, depth)
type Disp = (Int, Int)

data Cmd = Fwd Int
         | Aim Int

cmdAimVal :: Cmd -> Int
cmdAimVal (Fwd _) = 0
cmdAimVal (Aim v) = v

cmdFwdVal :: Cmd -> Int
cmdFwdVal (Fwd v) = v
cmdFwdVal (Aim _) = 0
    

soln :: IO ()
soln = 
  do content <- TIO.readFile inputFile

     let (horiz, depth) = cmdsToTotDisp (parseCmds content)

     putStrLn $ "Horizontal: " ++ show horiz
     putStrLn $ "Depth:      " ++ show depth
     putStrLn $ "Product:    " ++ show (horiz * depth)
     putStrLn ""
     putStrLn $ "Fold State: " ++ show (solnWithFold content)
     putStrLn $ "Arr Ops:    " ++ show (solnWithArrayOps content)


solnWithArrayOps :: T.Text -> Int
solnWithArrayOps input = 
  let input_lines = T.lines input
      
      fwds = map fwdVal input_lines

      cumulative_aims :: [Int]
      cumulative_aims = scanl1 (+) (map aimVal input_lines)

  in sum fwds * sum (zipWith (*) fwds cumulative_aims)
  where 
    fwdVal :: T.Text -> Int
    fwdVal line = 
      case T.words line of 
        ["forward", dist] -> read (T.unpack dist)
        _ -> 0
    
    aimVal :: T.Text -> Int
    aimVal line = 
      case T.words line of 
        ["down", dist] -> read (T.unpack dist)
        ["up", dist]   -> -(read (T.unpack dist))
        _ -> 0


solnWithFold :: T.Text -> Int
solnWithFold input = 
  let (horiz, depth, _) = foldl' fold (0,0,0) (T.lines input)
  in horiz * depth
  where 
    fold :: (Int, Int, Int) -> T.Text -> (Int, Int, Int)
    fold (horiz, depth, aim) line = 
      let [dir, dist_str] = T.words line
          dist = read (T.unpack dist_str) :: Int
      in case dir of 
            "forward" -> (horiz + dist, depth + (dist * aim), aim)
            "up"      -> (horiz, depth, aim - dist)
            "down"    -> (horiz, depth, aim + dist)
            _ -> error $ "Unrecognized dir: " ++ T.unpack dir


cmdsToTotDisp :: [Cmd] -> Disp
cmdsToTotDisp cmds = 
  let cumulative_aim = scanl1 (+) (map cmdAimVal cmds)
      fwds = map cmdFwdVal cmds

      disps :: (Int, Int)
      disps = 
        foldr (\(f, a) (h', d') -> (h' + f, d' + (f * a))) (0,0)
          . filter ((/= 0) . fst) 
          $ zip fwds cumulative_aim
   in disps


parseCmds :: T.Text -> [Cmd]
parseCmds = map parseCmd . T.lines
  where 
    parseCmd :: T.Text -> Cmd
    parseCmd input = 
      let [dir, dist] = T.words input
          dist_val = parseDecimal dist
       in case dir of 
            "forward" -> Fwd dist_val
            "down" -> Aim dist_val
            "up"   -> Aim (-dist_val)
            _ -> error (T.unpack dir)

parseDecimal :: T.Text -> Int
parseDecimal line = 
  case TR.decimal line of 
    Left e -> error e
    Right (val, _) -> val