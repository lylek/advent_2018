#!/usr/bin/env stack
-- stack script --resolver lts-12.2
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- Day 3 Part 1

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

-- I figured the simplest solution was to create a two-dimensional array of counts,
-- iterate over each rectangle, and iterate over the square inches in the rectangle,
-- adding to the count for each square inch. Then go over the whole grid finding
-- how many square inches have a count of at least 2.
--
-- But that approach wouldn't scale well if all the rectangles were scaled up. E.g.,
-- if each rectangle were made ten times as large, then it would take 100 times as
-- long to find the solution. I didn't like that.
--
-- So I created this solution, that only depends on the number of rectangles. But it
-- is a bit hairier. It operates by looking at edge transitions where any rectangle
-- starts or ends, in the horizontal direction. Then, for each range where there are
-- no transitions in the horizontal direction, it performs a similar operation in the
-- vertical direction, looking for edge transitions. It then multiplies the width by
-- the height of each block it finds.
--
-- I could be even more efficient when iterating in the vertical direction, if I
-- held onto the edge list from the prior sweep, and just tweaked it according to
-- which new claims were introduced or removed, rather than recalculating it each
-- time. This would require maintaining it in a map structure instead of a list.
--
-- There's also some refactoring that could be done here. But I have more puzzles to
-- solve!

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
        horizAccum :: (IntSet, Int) -> EdgeGroup -> (IntSet, Int)
        horizAccum (prevClaimIds, prevSqIn) group =
            let groupWidth = egToPos group - egFromPos group
                claimIds = applyEdges prevClaimIds (egEdges group)
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
                vertAccum :: (IntSet, Int) -> EdgeGroup -> (IntSet, Int)
                vertAccum (prevClaimIds, prevSqIn) group =
                    let groupHeight = egToPos group - egFromPos group
                        claimIds = applyEdges prevClaimIds (egEdges group)
                        area = groupWidth * groupHeight
                        numClaims = IntSet.size claimIds
                        sqIn = prevSqIn + if numClaims > 1 then area else 0
                    in (claimIds, sqIn)
                (_, area) = foldl' vertAccum (IntSet.empty, 0) vertEdgeGroups
            in (claimIds, prevSqIn + area)
        (_, sqIn) = foldl' horizAccum (IntSet.empty, 0) horizEdgeGroups
    print sqIn

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
