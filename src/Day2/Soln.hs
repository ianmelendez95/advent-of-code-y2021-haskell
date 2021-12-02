{-# LANGUAGE OverloadedStrings #-}


module Day2.Soln where 

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR


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