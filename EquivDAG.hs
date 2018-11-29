{-# LANGUAGE NoMonomorphismRestriction #-}

import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import qualified Data.Map as M


type NodeID = Int
type NodeSet = IS.IntSet

data EquivLink = Leader NodeSet | InClass NodeID deriving (Eq, Ord, Show)

data EquivNode f = EquivNode {
    -- My ID
    enID :: NodeID,
    -- Expression associated with this node
    enExpr :: f NodeID,
    -- A set of direct equivalents
    enEquivs :: NodeSet,
    -- Equivalence class info
    enClass :: EquivLink
  }

data EquivDAG f = EquivDAG {
    -- Next available ID
    edNextID :: NodeID,
    -- A map from IDs to nodes
    edNodes :: IM.IntMap (EquivNode f),
    -- A map from (fmap leader expr) to node IDs
    edNodeIDs :: M.Map (EquivNode f) NodeID
    -- A list of users of particular node ID
    --edUsers :: IntMap NodeSet
  }


findLeader :: NodeID -> EquivDAG f -> (NodeID, EquivDAG f)
findLeader nid dag = go nid
  where
    go nid = let node = edNodes dag IM.! nid in
        case enClass node of
            Leader _ -> (nid, dag)
            InClass nid' -> go nid'

-- newNode dag rawExpr = let expr = fmap (findLeader dag) expr in
