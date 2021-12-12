{-# LANGUAGE OverloadedStrings #-}

module Day12.Soln where 

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR

import Data.Ord (comparing)
import Data.List
import Data.Maybe (maybe, fromMaybe, listToMaybe, catMaybes, mapMaybe)
import Data.Char

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Set (Set)
import qualified Data.Set as Set

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Control.Monad.State.Lazy

import Debug.Trace


type Graph = Map String [String]
type Path = [String]


type TravS = State TravCtx

data TravCtx = TravCtx {
  tCtxSkipped :: Bool,
  tCtxGraph   :: Graph,
  tCtxVisited :: Set String
}


inputFile :: FilePath
inputFile = "src/Day12/full-input.txt"

soln :: IO ()
soln = 
  do cave_graph <- parseInput <$> TIO.readFile inputFile
     let all_paths = paths cave_graph
     putStrLn "[Paths]"
     putStrLn $ "Count: " ++ show (length all_paths)
    --  mapM_ print all_paths

initTravCtx :: Graph -> TravCtx
initTravCtx cave_graph = 
  TravCtx {
    tCtxGraph = cave_graph,

    tCtxVisited = Set.empty,
    tCtxSkipped = False
  }

paths :: Graph -> [Path]
paths cave_graph = evalState (pathsM "start") (initTravCtx cave_graph) 

pathsM :: String -> TravS [Path]
pathsM "end" = pure [["end"]]
pathsM cur_cave = 
  do next_caves <- travGetNextCaves cur_cave
     travExitCave cur_cave
     rest_paths <- concat <$> traverse (travWithCurCtx . pathsM) next_caves 
     pure $ map ([cur_cave] ++) rest_paths

travWithCurCtx :: TravS [Path] -> TravS [Path]
travWithCurCtx trav = 
  do cur_state <- get
     let result = evalState trav cur_state
     pure result

travGetNextCaves :: String -> TravS [String]
travGetNextCaves cur_cave = 
  gets (fromMaybe [] . Map.lookup cur_cave . tCtxGraph)

-- | when exiting a cave, if not small, it stays intact.
-- | if small, 
-- |   then if 'start', it collapses, with no update to skipped
-- |        if have skipped, it collapses
-- |        if visited, it collapses, and we 'skip'
-- |        else it stays intact and update that we've visited
travExitCave :: String -> TravS ()
travExitCave cave 
  | not $ isSmallCave cave = pure ()
  | cave == "start" = travDeleteCave cave
  | otherwise = 
    do skipped <- gets tCtxSkipped
       if skipped 
         then travDeleteCave cave
         else do have_visited <- travVisitCave cave
                 if not have_visited 
                   then pure ()
                   else do travMarkSkipped
                           -- all visited small caves now 'collapse'
                           all_visited <- Set.toList <$> gets tCtxVisited
                           mapM_ travDeleteCave all_visited

travDeleteCave :: String -> TravS ()
travDeleteCave cave = 
  modify (\ctx@TravCtx{ tCtxGraph = graph } -> ctx{ tCtxGraph = deleteCave cave graph })

travVisitCave :: String -> TravS Bool
travVisitCave cave = 
  do have_visited <- gets ((cave `Set.member`) . tCtxVisited)
     if have_visited
       then pure True
       else modify doVisit >> pure False
  where 
    doVisit :: TravCtx -> TravCtx
    doVisit ctx@TravCtx{ tCtxVisited = vs } = 
      ctx{ tCtxVisited = Set.insert cave vs }

travSkipped :: TravS Bool
travSkipped = gets tCtxSkipped

travMarkSkipped :: TravS ()
travMarkSkipped = modify (\ctx -> ctx{ tCtxSkipped = True })

isSmallCave :: String -> Bool
isSmallCave [] = error "No cave name"
isSmallCave (c:_) = isLower c

deleteCave :: String -> Graph -> Graph
deleteCave cave graph = 
  Map.map (deleteAll cave) (Map.delete cave graph)
  where 
    deleteAll :: String -> [String] -> [String]
    deleteAll _ [] = []
    deleteAll x (y:ys)
      | x == y = deleteAll x ys
      | otherwise = y : deleteAll x ys


parseInput :: T.Text -> Graph
parseInput input = 
  let ls = T.lines input
   in Map.fromListWith (++) (concatMap parseEdge ls)
  where 
    parseEdge :: T.Text -> [(String, [String])]
    parseEdge line = 
      let [s,e] = map T.unpack $ T.splitOn "-" line
       in [(s, [e]), (e, [s])]


