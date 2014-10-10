-- Copyright (c) 2014 Eric McCorkle.  All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- 3. Neither the name of the author nor the names of any contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHORS AND CONTRIBUTORS ``AS IS''
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
-- TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
-- PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS
-- OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
-- USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
-- ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
-- OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.

module Tests.Data.Enumeration.Traversal(tests) where

import Data.Enumeration
import Data.Enumeration.Traversal
import Test.HUnitPlus.Base

import qualified Data.ArithEncode as ArithEncode

firstEncoding :: ArithEncode.Encoding [Int]
firstEncoding = ArithEncode.fromHashableList [[0], [1], [2], [3]]

stepFunc :: Path -> [Int] -> Enumeration [Int]
stepFunc path val @ (0 : _) =
  fromEncodingWithPrefix path (ArithEncode.singleton (reverse val))
stepFunc path val @ (n : _) =
  let
    nextEncoding :: ArithEncode.Encoding [Int]
    nextEncoding = ArithEncode.wrap (Just . head) (Just . (: val))
                                    (ArithEncode.interval 0 (n - 1))
  in
    stepWithPrefix path nextEncoding stepFunc tail

enum :: Enumeration [Int]
enum = step firstEncoding stepFunc tail

depthFirstVals = [
    ([0], [0,0]),
    ([1,0], [1,0,0]),
    ([2,0], [2,0,0]),
    ([2,1,0], [2,1,0,0]),
    ([3,0], [3,0,0]),
    ([3,1,0], [3,1,0,0]),
    ([3,2,0], [3,2,0,0]),
    ([3,2,1,0], [3,2,1,0,0])
  ]

breadthFirstVals = [
    ([0], [0,0]),
    ([1,0], [1,0,0]),
    ([2,0], [2,0,0]),
    ([3,0], [3,0,0]),
    ([2,1,0], [2,1,0,0]),
    ([3,1,0], [3,1,0,0]),
    ([3,2,0], [3,2,0,0]),
    ([3,2,1,0], [3,2,1,0,0])
  ]

prioritizedVals = [
    ([3,2,1,0], [3,2,1,0,0]),
    ([3,2,0], [3,2,0,0]),
    ([3,1,0], [3,1,0,0]),
    ([3,0], [3,0,0]),
    ([2,1,0], [2,1,0,0]),
    ([2,0], [2,0,0]),
    ([1,0], [1,0,0]),
    ([0], [0,0])
  ]

infOne :: ArithEncode.Encoding Integer
infOne = ArithEncode.integral

finTwo :: Path -> Integer -> Enumeration (Integer, Integer)
finTwo path n =
  fromEncodingWithPrefix path (ArithEncode.wrap (Just . snd) (\m -> Just (n, m))
                                                (ArithEncode.interval 0 3))
finOne :: ArithEncode.Encoding Integer
finOne = ArithEncode.interval 0 3

infTwo :: Path -> Integer -> Enumeration (Integer, Integer)
infTwo path n =
  fromEncodingWithPrefix path (ArithEncode.wrap (Just . snd) (\m -> Just (n, m))
                                                ArithEncode.integral)

infiniteFinite :: Enumeration (Integer, Integer)
infiniteFinite = step infOne finTwo fst

finiteInfinite :: Enumeration (Integer, Integer)
finiteInfinite = step finOne infTwo fst

infiniteInfinite :: Enumeration (Integer, Integer)
infiniteInfinite = step infOne infTwo fst

breadthFirstInfFinVals = [
    ((0,0), [0,0]),
    ((0,1), [0,1]),
    ((0,2), [0,2]),
    ((0,3), [0,3]),
    ((-1,0), [1,0]),
    ((-1,1), [1,1]),
    ((-1,2), [1,2]),
    ((-1,3), [1,3])
  ]

breadthFirstFinInfVals = [
    ((0,0), [0,0]),
    ((1,0), [1,0]),
    ((2,0), [2,0]),
    ((3,0), [3,0]),
    ((0,-1), [0,1]),
    ((1,-1), [1,1]),
    ((2,-1), [2,1]),
    ((3,-1), [3,1])
  ]

breadthFirstInfInfVals = [
    ((0,0), [0,0]),
    ((0,-1), [0,1]),
    ((-1,0), [1,0]),
    ((0,1), [0,2]),
    ((-1,-1), [1,1]),
    ((1,0), [2,0]),
    ((0,-2), [0,3]),
    ((-1,1), [1,2])
  ]

depthFirstFinInfVals = [
    ((0,0), [0,0]),
    ((0,-1), [0,1]),
    ((0,1), [0,2]),
    ((0,-2), [0,3]),
    ((0,2), [0,4]),
    ((0,-3), [0,5]),
    ((0,3), [0,6]),
    ((0,-4), [0,7])
  ]

depthFirstFinite :: DepthFirst [Int]
depthFirstFinite = mkTraversal enum

breadthFirstFinite :: BreadthFirst [Int]
breadthFirstFinite = mkTraversal enum

breadthFirstInfiniteFinite :: BreadthFirst (Integer, Integer)
breadthFirstInfiniteFinite = mkTraversal infiniteFinite

breadthFirstFiniteInfinite :: BreadthFirst (Integer, Integer)
breadthFirstFiniteInfinite = mkTraversal finiteInfinite

breadthFirstInfiniteInfinite :: BreadthFirst (Integer, Integer)
breadthFirstInfiniteInfinite = mkTraversal infiniteInfinite

depthFirstFiniteInfinite :: DepthFirst (Integer, Integer)
depthFirstFiniteInfinite = mkTraversal finiteInfinite

scorefunc (enum, curr) = fromIntegral (curr + sum (prefix enum))

prioritizedFinite :: Prioritized [Int]
prioritizedFinite = mkPrioritizedTraversal scorefunc enum

prioritizedDefaultVals = [
    ([1,0], [1,0,0]),
    ([0], [0,0]),
    ([3,0], [3,0,0]),
    ([2,0], [2,0,0]),
    ([2,1,0], [2,1,0,0]),
    ([3,1,0], [3,1,0,0]),
    ([3,2,0], [3,2,0,0]),
    ([3,2,1,0], [3,2,1,0,0])
  ]

prioritizedFiniteDefault :: Prioritized [Int]
prioritizedFiniteDefault = mkTraversal enum

depthFirstTests = [
    "finite" ~: getAll depthFirstFinite @?= depthFirstVals,
    "finiteInfinite" ~: take 8 (getAll depthFirstFiniteInfinite) @?=
                        depthFirstFinInfVals
  ]

breadthFirstTests = [
    "finite" ~: getAll breadthFirstFinite @?= breadthFirstVals,
    "infiniteFinite" ~: take 8 (getAll breadthFirstInfiniteFinite) @?=
                        breadthFirstInfFinVals,
    "finiteInfinite" ~: take 8 (getAll breadthFirstFiniteInfinite) @?=
                        breadthFirstFinInfVals,
    "infiniteInfinite" ~: take 8 (getAll breadthFirstInfiniteInfinite) @?=
                          breadthFirstInfInfVals
  ]

prioritizedTests = [
    "finite" ~: getAll prioritizedFinite @?= prioritizedVals,
    "default_finite" ~: getAll prioritizedFiniteDefault @?=
                        prioritizedDefaultVals
  ]

testlist :: [Test]
testlist = [
    "depthFirst" ~: depthFirstTests,
    "breadthFirst" ~: breadthFirstTests,
    "prioritized" ~: prioritizedTests
  ]

tests :: Test
tests = "Traversal" ~: testlist
