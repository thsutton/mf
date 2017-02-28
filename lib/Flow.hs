{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Flow where

import Data.Graph.Inductive.Graph

type NodeCap = Int

type FlowCap = Int

type Height = Int

newtype Flow g a b = Flow { fromFlow :: g a b }
  deriving (DynGraph, Graph)

-- | 'AFlow' is a 'Graph' which we'll use with height-labelled nodes
-- and flow-labelled edges.
type AFlow g = Flow g Height FlowCap

-- | Find the maximal flow from a source node to a sink node.
--
-- A graph of flows resulting in the maximum flow through the network
-- is returned.
maximalFlow
  :: DynGraph g
  => g a FlowCap -- ^ Graph.
  -> Node  -- ^ Source node.
  -> Node  -- ^ Sink node.
  -> Maybe (AFlow g)
maximalFlow graph source sink =
  let preflow = initialFlow graph source
  in Just preflow

-- | Calculate the initial flow of the graph.
initialFlow
  :: DynGraph g
  => g a FlowCap
  -> Node
  -> AFlow g
initialFlow graph source = Flow (gfiltermap ctx graph)
  where
    v = order graph
    ctx (ine, n, _, oute)
      | n == source = Just ([], n, v, oute)
      | otherwise  = Just ([], n, 0, [])

data PushError
  = NotActive Node
  | NotAdmissible Node Node
  | DistanceInvariant Node Height Node Height

-- | Push excess flow accumulated at u to v.
push
  :: DynGraph g
  => AFlow g
  -> Node
  -> Node
  -> Either PushError (AFlow g)
push flow u v =
  Left $ NotActive u

{-

- Network structure

- Distance of each node (s = |V|, t = 0, forall v. t <= v <= s)

- flow :: Flow -> Node -> Node -> Maybe Double
  flow g u v =

- excess :: Flow -> Node -> Double
  excess f u = sum . map (\v -> f v u) $ edges f

- residual :: Flow -> Node -> Node -> Maybe Double
  residual g u v = (-) <$> capacity u v <*> flow g u v

-

-}
