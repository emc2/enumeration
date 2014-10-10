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
{-# OPTIONS_GHC -Wall -Werror -funbox-strict-fields #-}

module Data.Enumeration.Traversal(
       Traversal(..),
       DepthFirst,
       BreadthFirst,
       Prioritized,
       scoring,
       mkPrioritizedTraversal
       ) where

import Data.Enumeration
import Data.Enumeration.Traversal.Class
import Data.Heap(MaxPrioHeap)
import Data.Sequence(Seq, (<|), (|>), ViewL(..), viewl)

import qualified Data.Heap as Heap
import qualified Data.Sequence as Sequence

-- | Depth-first traversal.  Note that this style of traversal is not
-- guaranteed to be complete (it may deep-dive and never visit some
-- possibilities).  However, this implementation should continue
-- producing results even with infinite-sized branches, so long as the
-- depth of any one path isn't too great.
newtype DepthFirst ty = DepthFirst { dfStack :: [(Enumeration ty, Integer)] }

instance Traversal DepthFirst where
  mkTraversal enum = DepthFirst { dfStack = [(enum, 0)] }

  getNext DepthFirst { dfStack = [] } = Nothing
  getNext DepthFirst { dfStack = (enum, curr) : rest } =
    case numBranches enum of
      -- For a leaf, produce a result
      Just 0 -> Just (fromPath enum [], prefix enum,
                      DepthFirst { dfStack = rest })
      Just high
        -- If we are exhausting the current step, proceed directly to
        -- the next level
        | curr + 1 >= high ->
          getNext DepthFirst { dfStack = (withPrefix enum [curr], 0) : rest }
        -- Otherwise, keep it on the stack.
        | otherwise ->
          getNext DepthFirst { dfStack = (withPrefix enum [curr], 0) :
                                         (enum, curr + 1) : rest }
      -- When there's a step with infinite branches, go ahead and
      -- jettison the rest of the stack; we'll never get to it
      -- anyway.
      Nothing ->
        getNext DepthFirst { dfStack = [(withPrefix enum [curr], 0),
                                        (enum, curr + 1)] }


-- | Breadth-first traversal.  This style of traversal is guaranteed
-- to be complete- that is, it will visit every possibility
-- eventually.  However, it may take a very long time to reach any
-- given possibility.
newtype BreadthFirst ty =
  BreadthFirst { bfQueue :: Seq (Enumeration ty, Integer) }

instance Traversal BreadthFirst where
  mkTraversal enum = BreadthFirst { bfQueue = Sequence.singleton (enum, 0) }

  getNext BreadthFirst { bfQueue = queue } =
    case viewl queue of
      (enum, curr) :< rest ->
        case numBranches enum of
          -- For a leaf, produce a result
          Just 0 -> Just (fromPath enum [], prefix enum,
                          BreadthFirst { bfQueue = rest })
          Just high
            -- If we are exhausting the current head of the queue, remove it
            | curr + 1 >= high ->
              getNext BreadthFirst { bfQueue = rest |>
                                               (withPrefix enum [curr], 0) }
            -- Otherwise, keep it on the queue
            | otherwise ->
              getNext BreadthFirst { bfQueue = ((enum, curr + 1) <| rest) |>
                                               (withPrefix enum [curr], 0) }
          -- If there's a step with infinite branches, cycle it to the
          -- back of the queue, so we don't deep-dive into it.
          Nothing ->
            getNext BreadthFirst { bfQueue = rest |>
                                             (withPrefix enum [curr], 0) |>
                                             (enum, curr + 1) }
      EmptyL -> Nothing

-- | Prioritized traversal.  Will always pick the highest-scored
-- option.  Completeness depends entirely on the scoring function.
data Prioritized ty =
  Prioritized {
    scoring :: !((Enumeration ty, Integer) -> Float),
    priHeap :: !(MaxPrioHeap Float (Enumeration ty, Integer))
  }

-- | Create a prioritized traversal with a given scoring function.
mkPrioritizedTraversal :: ((Enumeration ty, Integer) -> Float)
                       -- ^ The scoring function to use.
                       -> Enumeration ty
                       -- ^ The enumeration to use.
                       -> Prioritized ty
mkPrioritizedTraversal scorefunc enum =
  let
    initial = (enum, 0)
    scored = (scorefunc initial, initial)
  in
    Prioritized { scoring = scorefunc, priHeap = Heap.singleton scored }

inverseDepth :: (Enumeration ty, Integer) -> Float
inverseDepth (enum, curr) =
  case numBranches enum of
    Just finitemax -> -(fromIntegral (length (prefix enum)) -
                        (fromIntegral curr / fromIntegral finitemax))
    Nothing -> -(fromIntegral (length (prefix enum)))

instance Traversal Prioritized where
  mkTraversal = mkPrioritizedTraversal inverseDepth

  getNext pri @ Prioritized { scoring = scorefunc, priHeap = heap } =
    case Heap.view heap of
      Just ((_, (enum, curr)), rest) ->
        case numBranches enum of
          -- For a leaf, produce a result
          Just 0 -> Just (fromPath enum [], prefix enum,
                          pri { priHeap = rest })
          -- If we're exhausting the current step, don't keep it in the heap
          Just high | curr + 1 >= high ->
            let
              newelem = (withPrefix enum [curr], 0)
              scored = (scorefunc newelem, newelem)
              withNew = Heap.insert scored rest
            in
              getNext pri { priHeap = withNew }
          -- Otherwise, insert the incremented current and the new
          -- branch into the heap.
          _ ->
            let
              newelem = (withPrefix enum [curr], 0)
              scored = (scorefunc newelem, newelem)
              increment = (enum, curr + 1)
              incscored = (scorefunc increment, increment)
              withNew = Heap.insert scored rest
              withInc = Heap.insert incscored withNew
            in
              getNext pri { priHeap = withInc }
      Nothing -> Nothing
