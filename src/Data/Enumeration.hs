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
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Enumeration(
       -- * Definitions
       Enumeration,
       Path,
       BadPath(..),
       IllegalArgument(..),

       -- ** Using Enumerations
       fromPath,
       toPath,
       toSizedPath,
       withPrefix,
       numBranches,
       prefix,

       -- * Constructions
       singleton,
       singletonWithPrefix,
       fromEncoding,
       fromEncodingWithPrefix,
       step,
       stepWithPrefix
       ) where

import Control.Exception
import Data.List
import Data.ArithEncode hiding (singleton)
import Data.Typeable

-- | A path that uniquely identifies a value in an @Enumeration@.
type Path = [Integer]

-- | A datatype that represents a mapping between @Path@s and @ty@s.
-- Note that unlike @Encoding@s, not all @Path@s are necessarily
-- valid.
data Enumeration ty =
  Enumeration {
    -- | Convert a @ty@ to a @Path@
    toPath :: !(ty -> Path),
    -- | Convert to a list of pairs, where the @fst@ holds
    -- the path entry, and @snd@ holds @numBranches@.  This is used
    -- primarily for encoding values as binary.
    toSizedPath :: !(ty -> [(Integer, Maybe Integer)]),
    -- | Generate an @ty@ from a @Path@
    fromPath :: !(Path -> ty),
    -- | Given a prefix path, get an enumeration that generates @ty@s
    -- from the rest of the path.
    withPrefix :: !(Path -> Enumeration ty),
    -- | Get the upper bound on values for the first path component,
    -- or @Nothing@ if there is no bound.
    numBranches :: !(Maybe Integer),
    -- | The reversed prefix path.
    prefix :: !Path
  }

data BadPath = BadPath String
  deriving Typeable

instance Show BadPath where
  show (BadPath "") = "Bad Path"
  show (BadPath msg) = "Bad Path: " ++ msg

instance Exception BadPath

showPath :: Path -> String
showPath = intercalate "." . map show

-- | Create an @Enumeration@ with an empty prefix that maps a single
-- value to and from the empty path.  Equivalent to
-- @singletonWithPrefix []@
singleton :: Eq ty =>
             ty
          -- ^ The value to map to and from the empty path.
          -> Enumeration ty
singleton = singletonWithPrefix []

-- | Create an @Enumeration@ with a given prefix path that maps a
-- single value to and from the empty path.
singletonWithPrefix :: Eq ty => Path -> ty -> Enumeration ty
singletonWithPrefix prefixPath val =
  let
    showCompletePath path = showPath (prefixPath ++ path)

    fromPathFunc [] = val
    fromPathFunc path =
      throw $! BadPath $! "Extra path elements " ++ showCompletePath path

    toPathFunc val'
      | val' == val = []
      | otherwise = throw $! IllegalArgument "Bad argument to singleton"

    toSizedPathFunc val'
      | val' == val = []
      | otherwise = throw $! IllegalArgument "Bad argument to singleton"

    withPrefixFunc [] = out
    withPrefixFunc path =
      throw $! BadPath $! "Extra path elements " ++ showCompletePath path

    out = Enumeration { fromPath = fromPathFunc, toPath = toPathFunc,
                        withPrefix = withPrefixFunc, numBranches = Just 0,
                        prefix = prefixPath, toSizedPath = toSizedPathFunc }
  in
    out

-- | Create an @Enumeration@ with an empty prefix from a single
-- @Encoding@.  The @Path@ will always be of length 1, and contains
-- the encoded value.
fromEncoding :: Eq ty =>
                Encoding ty
             -- ^ The @Encoding@ to use
             -> Enumeration ty
fromEncoding = fromEncodingWithPrefix []

-- | Create an @Enumeration@ with a given prefix from a single
-- @Encoding@.  The @Path@ will always be of length 1, and contains
-- the encoded value.
fromEncodingWithPrefix :: Eq ty => Path -> Encoding ty -> Enumeration ty
fromEncodingWithPrefix prefixPath enc =
  let
    fromPathFunc [encoded] = decode enc encoded
    fromPathFunc [] = throw $! BadPath "Path too short"
    fromPathFunc (_ : path) =
      throw $! BadPath $! "Extra path elements " ++ showPath path

    toPathFunc val = [encode enc val]
    toSizedPathFunc val = [(encode enc val, size enc)]

    withPrefixFunc newPrefix @ [encoded] =
      singletonWithPrefix (prefixPath ++ newPrefix) (decode enc encoded)
    withPrefixFunc [] = out
    withPrefixFunc (_ : path) =
      throw $! BadPath $! "Extra path elements " ++ showPath path

    out = Enumeration { fromPath = fromPathFunc, toPath = toPathFunc,
                        withPrefix = withPrefixFunc, numBranches = size enc,
                        prefix = prefixPath, toSizedPath = toSizedPathFunc }
  in
    out

-- | Create an @Enumeration@ with an empty prefix that uses an
-- @Encoding@ to convert the first element of the path to an interim
-- value, then uses that value to construct an @Enumeration@ to decode
-- the rest of the path.
step :: Encoding ty1
     -- ^ The encoding for the first type.
     -> (Path -> ty1 -> Enumeration ty2)
     -- ^ A function that produces an enumeration from the first type.
     -> (ty2 -> ty1)
     -- ^ A function that extracts the first type from the second.
     -> Enumeration ty2
step = stepWithPrefix []

-- | Create an @Enumeration@ with a prefix that uses an @Encoding@ to
-- convert the first element of the path to an interim value, then
-- uses that value to construct an @Enumeration@ to decode the rest of
-- the path.
stepWithPrefix :: Path
               -- ^ The prefix path.
               -> Encoding ty1
               -- ^ The encoding for the first type.
               -> (Path -> ty1 -> Enumeration ty2)
               -- ^ A function that produces an enumeration from the first type.
               -> (ty2 -> ty1)
               -- ^ A function that extracts the first type from the second.
               -> Enumeration ty2
stepWithPrefix prefixPath enc build extract =
  let
    fromPathFunc (first : rest) = fromPath (build prefixPath (decode enc first)) rest
    fromPathFunc [] = throw $! BadPath "Path too short"

    toPathFunc val =
      let
        extracted = extract val
        inner = build prefixPath extracted
      in
        encode enc extracted : toPath inner val

    toSizedPathFunc val =
      let
        extracted = extract val
        inner = build prefixPath extracted
      in
        (encode enc extracted, size enc) : toSizedPath inner val

    withPrefixFunc (first : rest) =
      let
        extracted = decode enc first
        newPrefix = prefixPath ++ [first]
        inner = build newPrefix extracted
      in
       withPrefix inner rest
    withPrefixFunc [] = out

    out = Enumeration { fromPath = fromPathFunc, toPath = toPathFunc,
                        withPrefix = withPrefixFunc, numBranches = size enc,
                        prefix = prefixPath, toSizedPath = toSizedPathFunc }
  in
    out
