{-# LANGUAGE TupleSections #-}
module Flow where

import Data.Graph.Inductive.Graph

type NodeCap = Int

type FlowCap = Int

type Height = Int

data Flow g a b = Flow
  { capacity :: g a b
  , flow     :: g a b
  }

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
  in Just (saturate preflow source)

saturate
  :: DynGraph g
  => AFlow g
  -> Node
  -> AFlow g
saturate p@(Flow capacity flow) s =
  foldr step p $ suc capacity s
  where
    step t p = push p s t

-- | Construct a problem from a graph.
initialFlow
  :: DynGraph g
  => g a FlowCap
  -> Node
  -> AFlow g
initialFlow graph source = Flow capacity flow
  where
    capacity = gfiltermap (\(ins, n, _, outs) -> Just (ins, n, h n, outs)) graph
    h n = if n == source then v else 0
    flow = gfiltermap ctx graph
    v = order graph
    ctx (ine, n, _, oute) = Just (map ((0,) . snd) ine , n, 0, map ((0,) . snd) oute)

-- | Push excess flow accumulated at u to v.
push
  :: DynGraph g
  => AFlow g
  -> Node
  -> Node
  -> AFlow g
push (Flow capacity flow) u v =
  let u_ctx = context flow u
      v_ctx = context flow v
      u_excess = (sum . map edgeLabel $ inn' u_ctx) - (sum . map edgeLabel $ out' u_ctx)
      v_excess = (sum . map edgeLabel $ inn' u_ctx) - (sum . map edgeLabel $ out' u_ctx)
      uv_flow = edgeLabel (edgeTo' u_ctx v)
      vu_flow = edgeLabel (edgeTo' v_ctx u)
      uv_capacity = 0
      delta = min u_excess (uv_capacity - uv_flow)

      uv_flow' = uv_flow + delta
      vu_flow' = vu_flow - delta
      u_excess' = u_excess - delta
      v_excess' = v_excess + delta

  in Flow capacity
     . insEdge (u, v, uv_flow') . insEdge (v, u, vu_flow') $ flow

-- | Get the 'LEdge' from a node to another node (or fail).
edgeTo' :: Context a b -> Node -> LEdge b
edgeTo' c@(i, u, l, o) v =
    maybe (error $ "Could not find edge from " ++ show u ++ " to " ++ show v)
    (\l->(u,v,l)) (lookup v (map swap o))
  where
    swap (a,b) = (b,a)

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
