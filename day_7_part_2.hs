#!/usr/bin/env stack
-- stack script --resolver lts-12.2
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}

-- Day 7 Part 2

-- Time:
--     Running as script, takes 1.85 seconds.
--     Compiled with full optimization, takes 0.025 seconds.

import           Data.Char (chr, ord)
import           Data.List (foldl', nub)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Data.Sequence (Seq, (|>), Seq((:<|)))
import qualified Data.Sequence as Sequence
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Text.Regex.PCRE.Heavy (re, scan)

main :: IO ()
main = do
    contents <- TIO.readFile "day_7_input.txt"
    let dependencyList = map readDependency $ T.lines contents
        nodeMap = buildNodeMap dependencyList
    print $ totalCompletionTime nodeMap

-- Idea: Keep a Map of the indegree of each node. Keep a reverse map nodes by
-- indegree. Keep modifying those as you iterate. The nodes with indegree zero
-- are the available ones.

-- For part 2, when we extract a free node, we cannot make its successor nodes available
-- yet. We have to wait until the work is completed for the node.
-- We need to keep track of tasks being worked, and their completion time.
-- We need a count of free workers.

data NodeMap = NodeMap
    { nmNodesByIndegree :: ! (IntMap IntSet)
    , nmIndegreeByNode  :: ! (IntMap Int)
    , nmOutgoingEdges   :: ! (IntMap IntSet)
    , nmCurTime         :: ! Int
    -- Represents all steps in progress. Key is time of completion, value is step id.
    , nmStepCompletions :: ! (IntMap (Seq Int))
    , nmFreeWorkers     :: ! Int
    } deriving Show

numWorkers :: Int
numWorkers = 5

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
        nmCurTime = 0
        nmStepCompletions = IntMap.empty
        nmFreeWorkers = numWorkers
    in NodeMap {..}

getMinFreeNode :: NodeMap -> Maybe Int
getMinFreeNode nm =
    case IntMap.lookup 0 (nmNodesByIndegree nm) of
        Nothing -> Nothing
        Just startingNodes -> Just $ IntSet.findMin startingNodes

areBusyWorkers :: NodeMap -> Bool
areBusyWorkers nm = nmFreeWorkers nm < numWorkers

areFreeWorkers :: NodeMap -> Bool
areFreeWorkers nm = nmFreeWorkers nm > 0

advanceToNextCompletion :: NodeMap -> NodeMap
advanceToNextCompletion nm =
    let ((completionTime, completingNodes), stepCompletions') = IntMap.deleteFindMin $ nmStepCompletions nm
        (completingNode :<| completingNodes') = completingNodes
        stepCompletions'' =
            case completingNodes' of
                Sequence.Empty -> stepCompletions'
                _ -> IntMap.insert completionTime completingNodes' stepCompletions'
        freeWorkers' = nmFreeWorkers nm + 1
        curTime' = completionTime
        -- Remove the completing node from outgoingEdges, but get its successor nodes
        outgoingEdges = nmOutgoingEdges nm
        Just successorNodes = IntMap.lookup completingNode outgoingEdges
        outgoingEdges' = IntMap.delete completingNode outgoingEdges
        -- Now decrement the indegree by node for each of the nodes following the completing node
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
        (nodesByIndegree', indegreeByNode') =
            IntSet.foldl'
                decrementIndegree
                (nmNodesByIndegree nm, nmIndegreeByNode nm)
                successorNodes
    in
        NodeMap
            { nmNodesByIndegree = nodesByIndegree'
            , nmIndegreeByNode = indegreeByNode'
            , nmOutgoingEdges = outgoingEdges'
            , nmCurTime = curTime'
            , nmStepCompletions = stepCompletions''
            , nmFreeWorkers = freeWorkers'
            }

stepDuration :: Int -> Int
stepDuration step = step - ord 'A' + 61

assignNodeToWorker :: Int -> NodeMap -> NodeMap
assignNodeToWorker freeNode nm =
    let nodesByIndegree = nmNodesByIndegree nm
        Just startingNodes = IntMap.lookup 0 nodesByIndegree
        (startingNode, startingNodes') = IntSet.deleteFindMin startingNodes
        nodesByIndegree' =
            if IntSet.null startingNodes'
            then IntMap.delete 0 nodesByIndegree
            else IntMap.insert 0 startingNodes' nodesByIndegree
        indegreeByNode' = IntMap.delete startingNode $ nmIndegreeByNode nm
        freeWorkers' = nmFreeWorkers nm - 1
        completionTime = nmCurTime nm + stepDuration freeNode
        insert v Nothing = Just $ Sequence.singleton v
        insert v (Just s) = Just $ s |> v
        stepCompletions' = IntMap.alter (insert freeNode) completionTime $ nmStepCompletions nm
    in
        nm
            { nmNodesByIndegree = nodesByIndegree'
            , nmIndegreeByNode = indegreeByNode'
            , nmFreeWorkers = freeWorkers'
            , nmStepCompletions = stepCompletions'
            }

totalCompletionTime :: NodeMap -> Int
totalCompletionTime nm =
    case getMinFreeNode nm of
        Nothing ->
            if areBusyWorkers nm
                then totalCompletionTime $ advanceToNextCompletion nm
                else nmCurTime nm
        Just freeNode ->
            if areFreeWorkers nm
                then totalCompletionTime $ assignNodeToWorker freeNode nm
                else totalCompletionTime $ advanceToNextCompletion nm

readDependency :: Text -> (Char, Char)
readDependency line =
    let [(_, [earlierStepT, laterStepT])] =
            scan [re|Step (.) must be finished before step (.) can begin\.|] line
    in (T.head earlierStepT, T.head laterStepT)
