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

-- Note: It takes an enormously long time for these tests to complete.
module Tests.Data.Enumeration.Binary(tests) where

import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.List
import Data.Enumeration
import Data.Enumeration.Binary
import Test.HUnitPlus.Base

import qualified Data.ArithEncode as ArithEncode

integralInteger :: ArithEncode.Encoding Integer
integralInteger = ArithEncode.integral

intervalInteger :: Integer -> Integer -> ArithEncode.Encoding Integer
intervalInteger = ArithEncode.interval

testPutGet :: (Eq ty, Show ty) => Enumeration ty -> ty -> Assertion
testPutGet enum val =
  let
    bs = runPut (putWithEnumeration enum val)
    val' = runGet (getWithEnumeration enum) bs
  in do
    val @=? val'

testEnumeration :: (Eq ty, Show ty) => String -> Enumeration ty -> [ty] -> Test
testEnumeration name enc vals =
  name ~: mapM_ (testPutGet enc) vals

infiniteVals = [((1 `shiftL` 64) - 0x100)..((1 `shiftL` 64) + 0x100)] ++
  (map (\n -> product (map (\m -> m * 8 + 1) [1..n])) [1..1500])

components :: [(String, ArithEncode.Encoding Integer, [Integer])]
components = [
    ("singleton", (ArithEncode.singleton 1), [1]),
    ("finite_2", (intervalInteger 1 2), [1, 2]),
    ("finite_10", (intervalInteger 1 10), [1..10]),
    ("finite_100", (intervalInteger 1 100), [1..100]),
    ("finite_10000", (intervalInteger 1 10000), [1..1000]),
    ("finite_64bit", (intervalInteger 0 0x100000000000000000000), [1..1000]),
    ("infinite", integralInteger, infiniteVals)
  ]

crossprod as bs =
  foldr (\a accum -> foldr (\b accum -> (a, b) : accum) accum bs) [] as

pairs = crossprod components components

makeTestCase ((name1, enc1, vals1), (name2, enc2, vals2)) =
  let
    vals = crossprod vals1 vals2
    innerEncoding n = ArithEncode.wrap (Just . snd) (\n' -> Just (n, n')) enc2
    innerEnum prefixPath = fromEncodingWithPrefix prefixPath . innerEncoding
    enum = step enc1 innerEnum fst
  in
    (name1 ++ "_" ++ name2) ~: mapM_ (testPutGet enum) vals

testlist :: [Test]
testlist = map makeTestCase pairs

tests :: Test
tests = "Binary" ~: testlist
