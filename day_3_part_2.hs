#!/usr/bin/env stack
-- stack script --resolver lts-12.2
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- Day 3 Part 2

import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Function (on)
import Data.List (foldl', groupBy, sortBy)
import Data.Maybe (fromJust)
import Safe (headMay)
import System.IO (readFile)
import Text.Printf (printf)
import Text.Regex.PCRE.Heavy (re, scan)

data Claim = Claim
    { cId :: !Int
    , cLeft :: !Int
    , cTop :: !Int
    , cWidth :: !Int
    , cHeight :: !Int
    }

cRight :: Claim -> Int
cRight claim = cLeft claim + cWidth claim

cBottom :: Claim -> Int
cBottom claim = cTop claim + cHeight claim

instance Show Claim where
    show Claim{..} =
        printf "#%d @ %d,%d: %dx%d" cId cLeft cTop cWidth cHeight

data Edge = Edge
    { ePos :: !Int
    , eDir :: !Dir
    , eClaim :: !Claim
    } deriving (Show)

data EdgeGroup = EdgeGroup
    { egFromPos :: !Int
    , egToPos :: !Int
    , egEdges :: [Edge]
    } deriving (Show)

data Dir = Start | End deriving (Show)

main :: IO ()
main = do
    contents <- readFile "day_3_input.txt"
    let claims = map (fromJust . parseClaim) $ lines contents
        claimMap = IntMap.fromList [(cId claim, claim) | claim <- claims]
        horizBounds = groupBy ((==) `on` ePos) $ sortBy (compare `on` ePos)
            $ concatMap horizEdgesForClaim claims
        horizEdgeGroups = map
            (\(es1, es2) -> EdgeGroup
                { egFromPos = ePos (head es1)
                , egToPos = ePos (head es2)
                , egEdges = es1
                })
            $ zip horizBounds (tail horizBounds)
        horizAccum :: (IntSet, IntSet) -> EdgeGroup -> (IntSet, IntSet)
        horizAccum (prevClaimIds, prevOverlappers) group =
            let claimIds = applyEdges prevClaimIds (egEdges group)
                claims = map (\id -> fromJust $ IntMap.lookup id claimMap)
                    $ IntSet.toList claimIds
                vertBounds = groupBy ((==) `on` ePos)
                    $ sortBy (compare `on` ePos)
                    $ concatMap vertEdgesForClaim claims
                vertEdgeGroups = map
                    (\(es1, es2) -> EdgeGroup
                        { egFromPos = ePos (head es1)
                        , egToPos = ePos (head es2)
                        , egEdges = es1
                        })
                    $ zip vertBounds (tail vertBounds)
                vertAccum :: (IntSet, IntSet) -> EdgeGroup -> (IntSet, IntSet)
                vertAccum (prevClaimIds, prevOverlappers) group =
                    let groupHeight = egToPos group - egFromPos group
                        claimIds = applyEdges prevClaimIds (egEdges group)
                        numClaims = IntSet.size claimIds
                        overlappers =
                            if numClaims < 2
                            then prevOverlappers
                            else IntSet.union prevOverlappers claimIds
                    in (claimIds, overlappers)
                (_, overlappers) = foldl' vertAccum (IntSet.empty, IntSet.empty) vertEdgeGroups
            in (claimIds, IntSet.union prevOverlappers overlappers)
        (_, overlappers) = foldl' horizAccum (IntSet.empty, IntSet.empty) horizEdgeGroups
        nonOverlappers = IntSet.difference (IntMap.keysSet claimMap) overlappers
    mapM_ print $ IntSet.toList nonOverlappers

horizEdgesForClaim :: Claim -> [Edge]
horizEdgesForClaim claim@Claim{..} =
    [ Edge { ePos = cLeft, eDir = Start, eClaim = claim }
    , Edge { ePos = cRight claim, eDir = End, eClaim = claim }
    ]

vertEdgesForClaim :: Claim -> [Edge]
vertEdgesForClaim claim@Claim{..} =
    [ Edge { ePos = cTop, eDir = Start, eClaim = claim }
    , Edge { ePos = cBottom claim, eDir = End, eClaim = claim }
    ]

applyEdges :: IntSet -> [Edge] -> IntSet
applyEdges ids edges =
    let applyEdge ids edge =
            let id = cId $ eClaim edge
            in case eDir edge of
                Start -> IntSet.insert id ids
                End -> IntSet.delete id ids
    in foldl' applyEdge ids edges

parseClaim :: String -> Maybe Claim
parseClaim line =
    let parses = scan [re|#(\d+) @ (\d+),(\d+): (\d+)x(\d+)|] line
        extractClaim (_, groups) =
            let [cId, cLeft, cTop, cWidth, cHeight] = map read groups
            in Claim{..}
    in fmap extractClaim $ headMay parses
