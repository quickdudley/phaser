module Phaser.ByteString (
  unpackBS,
  unpackLBS
 ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Word

import Control.Monad

import Phaser.Core

unpackBS :: Phase p BS.ByteString Word8 ()
unpackBS = (go >> unpackBS) <|> return () where
  go = get >>= BS.foldr (\w r -> yield w >> r) (return ())

unpackLBS :: Phase p BL.ByteString Word8 ()
unpackLBS = (go >> unpackLBS) <|> return () where
  go = get >>= BL.foldr (\w r -> yield w >> r) (return ())

