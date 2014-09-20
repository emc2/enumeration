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
{-# OPTIONS_GHC -Wall -Werror #-}

module Data.Enumeration(
       -- * Definitions
       Enumeration,
       Path
       {-
       BadPath,

       -- ** Using Enumerations
       fromPath,
       toPath

       -- * Constructions
       fromEncoding,
       step
-}
       ) where

import Data.ArithEncode

type Path = [Integer]

data Enumeration ty1 ty2 =
    End {
      endEnc :: Encoding ty2
    }
  | Step {
      stepEnc :: Encoding ty1,
      stepNext :: ty1 -> Enumeration ty ty2
  }
{-
-- | Generate an @Enumeration@ from a single @Encoding@.  The @Path@
-- will always be of length 1, and contains the encoded value.
fromEncoding :: Encoding ty -> Enumeration ty
fromEncoding enc =
  let
    frompathfunc [ encoded ] = decode enc encoded
    topathfunc val = [ encode enc val ]
  in
    Enumeration { fromPath = frompathfunc, toPath = topathfunc }

step :: Encoding ty1
     -- ^ The encoding for the first type.
     -> (ty1 -> Enumeration ty2)
     -- ^ A function that produces an enumeration from the first type.
     -> Enumeration ty2
-}
