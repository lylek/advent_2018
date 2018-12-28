#!/usr/bin/env stack
-- stack script --resolver lts-12.2
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

-- Day 7 Part 1

import           Data.Char (chr, ord)
import           Data.List (foldl', nub)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Text.Regex.PCRE.Heavy (re, scan)

-- Time:
--     Running as script, takes 0.827 seconds.
--     Compiled with full optimization, takes 0.026 seconds.

main :: IO ()
main = do
    contents <- TIO.readFile "day_7_input.txt"
    let dependencyList = map readDependency $ T.lines contents
        nodeMap = buildNodeMap dependencyList
        orderedList = orderedNodes nodeMap
    putStrLn orderedList

-- Idea: Keep a Map of the indegree of each node. Keep a reverse map nodes by
-- indegree. Keep modifying those as you iterate. The nodes with indegree zero
-- are the available ones.

data NodeMap = NodeMap
    { nmNodesByIndegree :: ! (IntMap IntSet)
    , nmIndegreeByNode  :: ! (IntMap Int)
    , nmOutgoingEdges   :: ! (IntMap IntSet)
    } deriving Show

buildNodeMap :: [(Char, Char)] -> NodeMap
buildNodeMap cdeps =
    let ideps = map (\(c1, c2) -> (ord c1, ord c2)) cdeps
        nodes = nub $ map fst ideps ++ map snd ideps
        insert v Nothing = Just $ IntSet.singleton v
        insert v (Just s) = Just $ IntSet.insert v s
        emptySetPerNode = IntMap.fromList $ map (\n -> (n, IntSet.empty)) nodes
        nmOutgoingEdges =
            foldl'
                (\m (f, t) -> IntMap.alter (insert t) f m)
                emptySetPerNode
                ideps
        inc Nothing = Just 1
        inc (Just n) = Just $ n + 1
        zeroPerNode = IntMap.fromList $ map (\n -> (n, 0)) nodes
        nmIndegreeByNode = foldl' (\m (_, t) -> IntMap.alter inc t m) zeroPerNode ideps
        nmNodesByIndegree =
            IntMap.foldlWithKey'
                (\m n i -> IntMap.alter (insert n) i m)
                IntMap.empty nmIndegreeByNode
    in NodeMap {..}

-- Finds the next step that should be completed. Returns it along with the NodeMap,
-- updated by removing that step.
extractMinNode :: NodeMap -> Maybe (Char, NodeMap)
extractMinNode nm =
    -- Is there a node left with indegree zero?
    case IntMap.lookupMin (nmNodesByIndegree nm) of
        Nothing -> Nothing
        Just (0, startingNodes) ->
            -- Yes, extract the node from nodesByIndegree
            let nodesByIndegree = nmNodesByIndegree nm
                (startingNode, startingNodes') = IntSet.deleteFindMin startingNodes
                nodesByIndegree' =
                    if IntSet.null startingNodes'
                    then IntMap.delete 0 nodesByIndegree
                    else IntMap.insert 0 startingNodes' nodesByIndegree
                -- Also remove it from indegreeByNode
                indegreeByNode' = IntMap.delete startingNode $ nmIndegreeByNode nm
                -- Remove it from outgoingEdges, but get the successor nodes
                outgoingEdges = nmOutgoingEdges nm
                Just nodesFollowingStartingNode = IntMap.lookup startingNode outgoingEdges
                outgoingEdges' = IntMap.delete startingNode outgoingEdges
                -- now decrement the indegree by node for each of the nodes following the starting node
                decrementIndegree (nodesByIndegree, indegreeByNode) node =
                    let (Just degree, indegreeByNode') =
                            IntMap.updateLookupWithKey (\_ n -> Just $ n - 1) node indegreeByNode
                        insert Nothing = Just $ IntSet.singleton node
                        insert (Just nodes) = Just $ IntSet.insert node nodes
                        remove Nothing = Nothing
                        remove (Just nodes) =
                            let nodes' = IntSet.delete node nodes
                            in if IntSet.null nodes' then Nothing else Just nodes'
                        nodesByIndegree' =
                            IntMap.alter insert (degree - 1)
                            $ IntMap.alter remove degree nodesByIndegree
                    in (nodesByIndegree', indegreeByNode')
                (nodesByIndegree'', indegreeByNode'') =
                    IntSet.foldl'
                        decrementIndegree
                        (nodesByIndegree', indegreeByNode')
                        nodesFollowingStartingNode
            in Just
                (chr startingNode
                , NodeMap
                    { nmNodesByIndegree = nodesByIndegree''
                    , nmIndegreeByNode = indegreeByNode''
                    , nmOutgoingEdges = outgoingEdges'
                    }
                )


orderedNodes :: NodeMap -> [Char]
orderedNodes nm = case extractMinNode nm of
    Nothing -> []
    Just (node, nm') -> node : orderedNodes nm'

readDependency :: Text -> (Char, Char)
readDependency line =
    let [(_, [earlierStepT, laterStepT])] =
            scan [re|Step (.) must be finished before step (.) can begin\.|] line
    in (T.head earlierStepT, T.head laterStepT)