module Main where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree  (Gr)
import System.Exit

import Flow

source = 1
sink = 2
node = 3

graph :: Gr String Int
graph = insEdge (node, sink, 1) . insEdge (source, node, 3) . insEdge (source, sink, 10) $
  insNodes [(source, "source"), (sink, "sink"), (node, "a")] empty

noSolution :: (Show a, Show b, DynGraph g) => g a b -> IO ()
noSolution graph = do
  putStrLn "There is no solution for:"
  prettyPrint graph
  exitFailure

printProblem :: DynGraph g => AFlow g -> IO ()
printProblem (Flow cap flow) = do
  putStrLn "-- Problem"
  prettyPrint cap
  putStrLn "\n"
  putStrLn "-- Solution"
  prettyPrint flow

main :: IO ()
main = do
  let prob = initialFlow graph source
  putStrLn "\n#\n# Initial Flow\n#\n"
  printProblem prob
  putStrLn "\n#\n# Solved Problem\n#\n"
  maybe (noSolution graph) printProblem $ maximalFlow graph source sink

