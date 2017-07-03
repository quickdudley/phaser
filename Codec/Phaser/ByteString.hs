{-|
Module:     Codec.Parser.ByteString
Copyright:  Jeremy List
License:    BSD-3
Maintainer: quick.dudley@gmail.com

'Phase's for processing bytestrings.
-}
module Codec.Phaser.ByteString (
  unpackBS,
  unpackLBS,
  parseFile_,
  parseHandle_
 ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Word

import Control.Monad
import Control.Applicative
import System.IO (openFile,IOMode(ReadMode),Handle)

import Codec.Phaser.Core

-- | A 'Phase' which takes 'BS.ByteString's as input and yields their individual
-- bytes.
unpackBS :: (Monoid p) => Phase p BS.ByteString Word8 ()
unpackBS = (go >> unpackBS) <|> return () where
  go = get >>= BS.foldr (\w r -> yield w >> r) (return ())

-- | A 'Phase' which takes lazy 'BL.ByteString's as input and yields their
-- individual bytes.
unpackLBS :: (Monoid p) => Phase p BL.ByteString Word8 ()
unpackLBS = (go >> unpackLBS) <|> return () where
  go = get >>= BL.foldr (\w r -> yield w >> r) (return ())


-- | Run a parser on input from a file. Input is provided as bytes, if
-- characters are needed: a decoding phase such as
-- 'Codec.Phaser.UTF8.utf8_stream' or 'latin1' may be used. Counter type
-- agnostic version.
parseFile_ :: (Monoid p,PhaserType s) => p -> s p Word8 o a -> FilePath ->
  IO (Either [(p,[String])] [a])
parseFile_ p c n = openFile n ReadMode >>= parseHandle_ p c

-- | Run a parser from the contents of a 'Handle'. Input is provided as bytes.
parseHandle_ :: (Monoid p,PhaserType s) => p -> s p Word8 o a -> Handle ->
  IO (Either [(p,[String])] [a])
parseHandle_ p c h = do
  d <- BL.hGetContents h
  return $ parse_ p (unpackBS >># c) (BL.toChunks d)
