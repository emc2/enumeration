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

-- | Utilities for constructing enumerations over datatypes.
--
-- An 'Enumeration' is a mapping between a particular datatype and a
-- 'Path', which consists of a list of natural numbers.  Conceptually,
-- we can think of all the values of a given datatype as being
-- organized into a tree, with nodes representing decisions that
-- narrow down the choices.  In such a scheme, a 'Path' represents a
-- path through the tree to a single value, and an 'Enumeration' is a
-- procedure for converting a 'Path' to and from a value.
--
--
-- An 'Enumeration' has two key functions: 'fromPath' and 'toPath',
-- which translate between paths and instances of a datatype.  These
-- functions are expected to be inverses; or:
--
--   * @fromPath (toPath v) == v@ for all values in the domain
--
-- Beyond this, there are no additional restrictions.  Specifically,
-- two paths /may/ map to the same value.
--
--
-- The 'numBranches' function indicates the maximum value of the first
-- path element for an 'Enumeration'.  The minimum value is always 0,
-- and all values between 0 and 'numBranches' must be valid.  If there
-- is no upper bound on the value of the first path element,
-- 'numBranches' returns 'Nothing'.  The 'toSizedPath' maps a value to
-- a path, which also contains the result of 'numBranches' at each
-- step in the path.
--
--
-- The 'withPrefix' function supplies a partial path to an
-- 'Enumeration', yielding a new 'Enumeration' that maps each value to
-- the same path(s) as the original 'Enumeration', with the prefix
-- added or removed.  More formally, if
--
--   * @subenc = withPrefix enc prepath@
--
-- Then
--
--   * @toPath enc val == prepath ++ toPath subenc val@
--
--   * @fromPath enc (prepath ++ subpath) == fromPath subenc subpath@
--
-- With multiple uses of 'withPrefix', the following must be true:
--
--   * @withPrefix (withPrefix enc path1) path2 == withPrefix enc (path1 ++ path2)@
--
-- Finally, if a complete path is given to 'withPrefix', then the
-- result is a singleton encoding that gives the value associated with
-- that path.  That is:
--
--   * @fromPath enc fullpath == fromPath (withPrefix enc fullpath) []@
--
-- This provides \"warm-start\" functionality for 'Enumeration's.
-- When translating a large number of 'Path's with the same prefix, it
-- will generally be much more efficient to use 'withPrefix' and use
-- the resulting 'Enumeration' than to translate the 'Path's directly.
--
--
-- The 'prefix' function gives the current prefix for an
-- 'Enumeration'.  The following rule describes the relationship
-- between 'prefix' and 'withPrefix':
--
--   * @prefix (withPrefix enc prepath) == prepath@
--
--
-- 'Enumeration's are similar to 'Encoding's from the arith-encode
-- library, except 'Enumeration's are generally more flexible, and can
-- more easily accomodate complex datatypes with invariants.  However,
-- as 'Path's are constructed from natural numbers, we can create an
-- 'Enumeration' using a series of 'Encoding's for intermediate data.
-- The functions in this module provide the ability to construct
-- 'Enumeration's using 'Encoding's
--
-- A singleton 'Enumeration' can be constructed using the 'singleton'
-- and 'singletonWithPrefix' functions.
--
-- An 'Encoding' for a datatype can be converted into an 'Enumeration'
-- (where all paths have a single element) using the 'fromEncoding'
-- and 'fromEncodingWithPrefix' functions.
--
-- The 'step' and 'stepWithPrefix' functions construct an
-- 'Enumeration' from an 'Encoding' for an intermediate value, and a
-- generator function that produces an 'Enumeration' from the
-- intermediate value.
--
-- The @withPrefix@ variants for each of these take a prefix path,
-- where the non-@withPrefix@ variants set the prefix to @[]@.
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
    -- | Generate a @ty@ from a @Path@
    fromPath :: !(Path -> ty),
    -- | Given a prefix path, get an enumeration that generates @ty@s
    -- from the rest of the path.
    withPrefix :: !(Path -> Enumeration ty),
    -- | Get the upper bound on values for the first path component,
    -- or @Nothing@ if there is no bound.
    numBranches :: !(Maybe Integer),
    -- | The prefix path.
    prefix :: !Path
  }

-- | An exception thrown when a 'Path' is invalid.
data BadPath = BadPath String
  deriving Typeable

instance Show BadPath where
  show (BadPath "") = "Bad Path"
  show (BadPath msg) = "Bad Path: " ++ msg

instance Exception BadPath

showPath :: Path -> String
showPath = intercalate "." . map show

-- | Create an 'Enumeration' with an empty prefix that maps a single
-- value to and from the empty path.  Equivalent to
-- @singletonWithPrefix []@
singleton :: Eq ty =>
             ty
          -- ^ The value to map to and from the empty path.
          -> Enumeration ty
singleton = singletonWithPrefix []

-- | Create an 'Enumeration' with a given prefix path that maps a
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

-- | Create an 'Enumeration' with an empty prefix from a single
-- 'Encoding'.  The 'Path' will always be of length 1, and contains
-- the encoded value.
fromEncoding :: Eq ty =>
                Encoding ty
             -- ^ The 'Encoding' to use
             -> Enumeration ty
fromEncoding = fromEncodingWithPrefix []

-- | Create an 'Enumeration' with a given prefix from a single
-- 'Encoding'.  The 'Path' will always be of length 1, and contains
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

-- | Create an 'Enumeration' with an empty prefix that uses an
-- 'Encoding' to convert the first element of the path to an interim
-- value, then uses that value to construct an 'Enumeration' to decode
-- the rest of the path.
step :: Encoding ty1
     -- ^ The encoding for the first type.
     -> (Path -> ty1 -> Enumeration ty2)
     -- ^ A function that produces an enumeration from the first type.
     -> (ty2 -> ty1)
     -- ^ A function that extracts the first type from the second.
     -> Enumeration ty2
step = stepWithPrefix []

-- | Create an 'Enumeration' with a prefix that uses an 'Encoding' to
-- convert the first element of the path to an interim value, then
-- uses that value to construct an 'Enumeration' to decode the rest of
-- the path.
stepWithPrefix :: Path
               -- ^ The prefix path.
               -> Encoding ty1
               -- ^ The 'Encoding' for the first type.
               -> (Path -> ty1 -> Enumeration ty2)
               -- ^ A function that produces an enumeration from the first type.
               -> (ty2 -> ty1)
               -- ^ A function that extracts the first type from the second.
               -> Enumeration ty2
stepWithPrefix prefixPath enc build extract =
  let
    fromPathFunc (first : rest) =
      fromPath (build prefixPath (decode enc first)) rest
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
