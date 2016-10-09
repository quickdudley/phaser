{-|
Module:     Codec.Parser.ByteString
Copyright:  Jeremy List
License:    BSD-3
Maintainer: quick.dudley@gmail.com

'Phase's for processing bytestrings.
-}
module Codec.Phaser.ByteString (
  unpackBS,
  unpackLBS
 ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Word

import Control.Monad
import Control.Applicative

import Codec.Phaser.Core

-- | A 'Phase' which takes 'BS.ByteString's as input and yields their individual
-- bytes.
unpackBS :: Phase p BS.ByteString Word8 ()
unpackBS = (go >> unpackBS) <|> return () where
  go = get >>= BS.foldr (\w r -> yield w >> r) (return ())

-- | A 'Phase' which takes lazy 'BL.ByteString's as input and yields their
-- individual bytes.
unpackLBS :: Phase p BL.ByteString Word8 ()
unpackLBS = (go >> unpackLBS) <|> return () where
  go = get >>= BL.foldr (\w r -> yield w >> r) (return ())
