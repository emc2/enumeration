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

module Data.Enumeration.Binary(
       putWithEnumeration,
       getWithEnumeration
       ) where

import Data.Binary.Get hiding (remaining)
import Data.Binary.Put
import Data.Bits
import Data.Enumeration
import Math.NumberTheory.Logarithms

-- Emit a natural number as a sequence of some number of bytes
putNatural :: Int -> Integer -> Put
putNatural 0 0 = return ()
putNatural 0 _ = error "Data remaining at end of encoding"
putNatural remaining natural
  | remaining > 8 =
    let
      output = fromInteger (natural .&. 0xffffffffffffffff)
      rest = natural `shiftR` 64
    in do
      putWord64le output
      putNatural (remaining - 8) rest
  | remaining > 4 =
    let
      output = fromInteger (natural .&. 0xffffffff)
      rest = natural `shiftR` 32
    in do
      putWord32le output
      putNatural (remaining - 4) rest
  | remaining > 2 =
    let
      output = fromInteger (natural .&. 0xffff)
      rest = natural `shiftR` 16
    in do
      putWord16le output
      putNatural (remaining - 2) rest
  | otherwise =
    let
      output = fromInteger (natural .&. 0xff)
      rest = natural `shiftR` 8
    in do
      putWord8 output
      putNatural (remaining - 1) rest

putWithEnumeration :: Enumeration ty -> ty -> Put
putWithEnumeration enum =
  let
    putWithEnumeration' [] = return ()
    putWithEnumeration' ((encoded, branches) : rest) =
      case branches of
        Just 0 -> return ()
        Just 1 -> putWithEnumeration' rest
        Just finitesize ->
          let
            bytes = (integerLog2 (finitesize - 1) `quot` 3) + 1
          in do
            putNatural bytes encoded
            putWithEnumeration' rest
        Nothing ->
          do
            if encoded < 64
              then
                putWord8 (fromInteger encoded `shiftL` 2)
              else
                let
                  bytes = (integerLog2 (encoded - 1) `quot` 3) + 1
                in do
                  if bytes <= 64
                    then putWord8 (fromIntegral (((bytes - 1) `shiftL` 2) .|.
                                                 0x1))
                    else if bytes <= 16384
                      then
                        putWord16le (fromIntegral (((bytes - 1) `shiftL` 2) .|.
                                                     0x2))
                      else
                        putWord64le (fromIntegral (((bytes - 1) `shiftL` 2) .|.
                                                   0x3))
                  putNatural bytes encoded
            putWithEnumeration' rest
  in
    putWithEnumeration' . toSizedPath enum

-- Read in a natural number as a sequence of some number of bytes
getNatural :: Int -> Get Integer
getNatural bytes =
  let
    getNatural' :: Integer -> Int -> Get Integer
    getNatural' accum count
      | count + 8 < bytes =
        do
          input <- getWord64le
          getNatural' ((toInteger input `shiftL` (count * 8)) .|. accum)
                      (count + 8)
      | count + 4 < bytes =
        do
          input <- getWord32le
          getNatural' ((toInteger input `shiftL` (count * 8)) .|. accum)
                      (count + 4)
      | count + 2 < bytes =
        do
          input <- getWord16le
          getNatural' ((toInteger input `shiftL` (count * 8)) .|. accum)
                      (count + 2)
      | count < bytes =
        do
          input <- getWord8
          getNatural' ((toInteger input `shiftL` (count * 8)) .|. accum)
                      (count + 1)
      | otherwise = return accum
  in
    getNatural' 0 0

-- | Use an @Enumeration@ to extract a @ty@ from binary data.
getWithEnumeration :: Enumeration ty -> Get ty
getWithEnumeration enum =
  case numBranches enum of
    Just 0 -> return (fromPath enum [])
    Just 1 -> getWithEnumeration (withPrefix enum [0])
    Just finitesize ->
      let
        bytes = (integerLog2 (finitesize - 1) `quot` 3) + 1
      in do
        encoded <- getNatural bytes
        getWithEnumeration (withPrefix enum [encoded])
    -- Arbitrary-length naturals are encoded with a more complex
    -- scheme.  The first two bits are a tag, which tells how to
    -- interpret the rest.
    Nothing ->
      do
        firstbyte <- lookAhead getWord8
        encoded <-
          case firstbyte .&. 0x03 of
            -- Naturals less than 64 get packed into the same byte as
            -- the tag
            0x0 ->
              do
                datafield <- getWord8
                return (toInteger (datafield `shiftR` 2))
            -- One-byte length field, and then up to 64 bytes of data
            0x1 ->
              do
                lenfield <- getWord8
                getNatural (fromIntegral (lenfield `shiftR` 2) + 1)
            -- Two-byte length field, and then up to 16384 bytes of data
            0x2 ->
              do
                lenfield <- getWord16le
                getNatural (fromIntegral (lenfield `shiftR` 2) + 1)
            -- Eight-byte length field, and then data
            0x3 ->
              do
                lenfield <- getWord64le
                getNatural (fromIntegral (lenfield `shiftR` 2) + 1)
            _ -> error "Impossible case"
        getWithEnumeration (withPrefix enum [encoded])
