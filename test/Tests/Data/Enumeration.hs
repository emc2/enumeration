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

module Tests.Data.Enumeration(tests) where

import Data.Enumeration
import Data.Word
import Test.HUnitPlus.Base hiding (withPrefix)

import qualified Data.ArithEncode as ArithEncode
import qualified Tests.Data.Enumeration.Binary as Binary
import qualified Tests.Data.Enumeration.Traversal as Traversal

intervalWord8 :: Word8 -> Word8 -> ArithEncode.Encoding Word8
intervalWord8 = ArithEncode.interval

throwsIllegalArgument (IllegalArgument _) = assertSuccess
throwsBadPath (BadPath _) = assertSuccess

singletonTests enum val nonval prefixPath =
  let
    noRecurseTests enum = [
        "toPath_succeed" ~: toPath enum val @?= [],
        "toPath_fail" ~: assertThrows throwsIllegalArgument
                                      (return $! toPath enum nonval),
        "toSizedPath_succeed" ~: toSizedPath enum val @?= [],
        "toSizedPath_fail" ~: assertThrows throwsIllegalArgument
                                           (return $! toSizedPath enum nonval),
        "fromPath_succeed" ~: fromPath enum [] @?= val,
        "fromPath_fail" ~: assertThrows throwsBadPath (return $! fromPath enum [0]),
        "withPrefix_fail" ~: assertThrows throwsBadPath
                                          (return $! withPrefix enum [0]),
        "numBranches" ~: numBranches enum @?= Just 0,
        "prefix" ~: prefix enum @?= prefixPath
      ]
  in
    ("withPrefix_empty" ~: noRecurseTests (withPrefix enum [])) :
    noRecurseTests enum

fromEncodingTests enum vals prefixPath =
  let
    len = toInteger (length vals)

    makePrefixTest val =
      ("withPrefix_" ++ show val) ~:
        singletonTests (withPrefix enum [toInteger val]) val (val + 1)
                       (prefixPath ++ [toInteger val])

    noRecurseTests enum = [
        "toPath" ~: mapM_ (\val -> toPath enum val @?= [toInteger val]) vals,
        "toSizedPath" ~:
          mapM_ (\val -> toSizedPath enum val @?= [(toInteger val, Just len)]) vals,
        "fromPath" ~: mapM_ (\val -> fromPath enum [toInteger val] @?= val) vals,
        "fromPath_short" ~: assertThrows throwsBadPath (return $! fromPath enum []),
        "fromPath_long" ~: assertThrows throwsBadPath
                                        (return $! fromPath enum [0, 0]),
        "withPrefix_long" ~: assertThrows throwsBadPath
                                          (return $! withPrefix enum [0, 0]),
        "numBranches" ~: numBranches enum @?= Just len,
        "prefix" ~: prefix enum @?= prefixPath
      ] ++ map makePrefixTest vals
  in
    ("withPrefix_empty" ~: noRecurseTests (withPrefix enum [])) :
    noRecurseTests enum

stepTests enum nums vals prefixPath =
  let
    innerEncodingTests enum num prefixPath =
      let
        nums = [0..num]
        len = toInteger (num + 1)

        makePrefixTest num' =
          ("withPrefix_" ++ show num') ~:
          singletonTests (withPrefix enum [toInteger num']) (num, num')
                         (num, num' + 1) (prefixPath ++ [toInteger num'])

        noRecurseTests enum = [
            "toPath" ~: mapM_ (\num' -> toPath enum (num, num') @?=
                                        [toInteger num']) nums,
            "toSizedPath" ~:
              mapM_ (\num' -> toSizedPath enum (num, num') @?=
                             [(toInteger num', Just len)]) nums,
            "fromPath" ~: mapM_ (\num' -> fromPath enum [toInteger num'] @?=
                                          (num, num')) nums,
            "fromPath_short" ~:
              assertThrows throwsBadPath (return $! fromPath enum []),
            "fromPath_long" ~:
              assertThrows throwsBadPath (return $! fromPath enum [0, 0]),
            "withPrefix_long" ~: assertThrows throwsBadPath
                                              (return $! withPrefix enum [0, 0]),
            "numBranches" ~: numBranches enum @?= Just len,
            "prefix" ~: prefix enum @?= prefixPath
          ] ++ map makePrefixTest nums
      in
        ("withPrefix_empty" ~: noRecurseTests (withPrefix enum [])) :
        noRecurseTests enum

    len = toInteger (length nums)

    makePrefixTest num =
      ("withPrefix_" ++ show num) ~:
        innerEncodingTests (withPrefix enum [toInteger num]) num
                           (prefixPath ++ [toInteger num])

    noRecurseTests enum = [
        "toPath" ~: mapM_ (\val @ (v1, v2) -> toPath enum val @?=
                                              [toInteger v1, toInteger v2]) vals,
        "toSizedPath" ~:
          mapM_ (\val @ (v1, v2) -> toSizedPath enum val @?=
                                    [(toInteger v1, Just len),
                                     (toInteger v2, Just (toInteger v1 + 1))]) vals,
        "fromPath" ~:
          mapM_ (\val @ (v1, v2) -> fromPath enum [toInteger v1, toInteger v2] @?=
                                    val) vals,
        "fromPath_short" ~: assertThrows throwsBadPath (return $! fromPath enum []),
        "fromPath_long" ~: assertThrows throwsBadPath
                                        (return $! fromPath enum [0, 0, 0]),
        "withPrefix_long" ~: assertThrows throwsBadPath
                                          (return $! withPrefix enum [0, 0, 0]),
        "numBranches" ~: numBranches enum @?= Just len,
        "prefix" ~: prefix enum @?= prefixPath
      ] ++ map makePrefixTest nums
  in
    ("withPrefix_empty" ~: noRecurseTests (withPrefix enum [])) :
    noRecurseTests enum

testlist :: [Test]
testlist =
  let
    intervalEncoding = intervalWord8 0 100

    innerEncoding n = ArithEncode.wrap (Just . snd) (\n' -> Just (n, n'))
                                       (intervalWord8 0 n)

    innerEnum prefixPath = fromEncodingWithPrefix prefixPath . innerEncoding

    makeVals = concat . map (\n -> map (\n' -> (n, n')) [0..n])
  in [
    "singleton" ~: singletonTests (singleton 'A') 'A' 'B' [],
    "singletonWithPrefix" ~:
      singletonTests (singletonWithPrefix [0] 'A') 'A' 'B' [0],
    "fromEncoding" ~: fromEncodingTests (fromEncoding intervalEncoding) [0..100] [],
    "fromEncodingWithPrefix" ~:
      fromEncodingTests (fromEncodingWithPrefix [1] intervalEncoding) [0..100] [1],
    "step" ~: stepTests (step intervalEncoding innerEnum fst)
                        [0..100] (makeVals [0..100]) []
  ]

tests :: Test
tests = "Enumeration" ~: Binary.tests : Traversal.tests : testlist
