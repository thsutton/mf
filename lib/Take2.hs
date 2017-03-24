module Take2 where

import Data.Maybe
import Data.Word

type Nat = Word

-- | Graphs with edge and vertex labels.
type Graph el vl = ()

type Capacity = Double

type Height = Nat

type Vertex = ()
type Edge = (Vertex, Vertex)

data Problem = Problem
  { probGraph  :: Graph Capacity ()
  , probSource :: Vertex
  , probSink   :: Vertex
  , probFlow   :: Graph Capacity Height
  }

-- | Initialise a problem from a graph.
--
-- Set vertex labels; initialise preflow; perform initial pushes.
initialise
  :: Graph Capacity a
  -> Problem
initialise _ = error "Initialise is not implemented"

-- | Push flow along an edge.
--
-- If the source vertex is inactive or the edge is inadmissible and
-- 'error' will be raised.
push
  :: Problem
  -> (Vertex, Vertex)
  -> Problem
push p (u, v)
  | not (active p u) = error ("Attempted to push from an inactive node: " ++ show u)
  | not (admissible p u v) = error ("Attempted to push an inadmissible edge: " ++ show (u, v))
  | otherwise = error "Push is not implemented"

-- | Relabel an active vertex.
--
-- If the given vertex does not need relabelling (it is inactive or it
-- has admissible edges) an 'error' will be raised.
relabel
  :: Problem
  -> Vertex -- ^ Vetex (which had better need relabelling!)
  -> Problem
relabel p v
  | not (active p v) = error ("Attempted to relabel an inactive node: " ++ show v)
  | hasAdmissible p v = error ("Attempted to relabel a node with admissible edges: " ++ show v)

-- | Select an active vertex.
selectActive
  :: Problem
  -> Maybe Vertex
selectActive p = error "Select Active is not implemented"

-- | Select an admissible edge from an active vertex.
--
-- If the vertex is not active an 'error' will be raised.
selectAdmissible
  :: Problem
  -> Vertex -- ^ Source (which had better be active!)
  -> Maybe Edge
selectAdmissible p v
  | not (active p v) = error ("Can't find admissible node from inactive vertex: " ++ show v)
  | otherwise = error "Select Admissible is not implemented"

-- | Check that a vertex is active.
active :: Problem -> Vertex -> Bool
active p v = False

-- | Check that an edge is admissible.
admissible
  :: Problem
  -> Vertex
  -> Vertex
  -> Bool
admissible p u v = False

-- | Check that a vertex has an admissible edge.
hasAdmissible
  :: Problem
  -> Vertex
  -> Bool
hasAdmissible p v = isJust (selectAdmissible p v)
