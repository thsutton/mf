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

main :: IO ()
main = do
  maybe (noSolution graph) prettyPrint $ maximalFlow graph source sink

