module Codec.Phaser.UTF16 (
  utf16_char,
  utf16_word16_stream,
  utf16_stream_useBOM,
  utf16_stream_le,
  utf16_stream_be,
  utf16_stream_unknown
 ) where

import Data.Bits
import Data.Word

import Control.Applicative
import Control.Monad

import Codec.Phaser.Core
import Codec.Phaser.Common

unit_be :: Phase p Word8 o Word16
unit_be = (\a b -> shiftL a 8 .|. b) <$> byte <*> byte where
  byte = fromIntegral <$> get

unit_le :: Phase p Word8 o Word16
unit_le = (\a b -> a .|. shiftL b 8) <$> byte <*> byte where
  byte = fromIntegral <$> get

useBOM_unit :: Monoid p => Phase p Word8 o1 (Phase p Word8 o2 Word16)
useBOM_unit = "UTF-16: No byte order mark" <?> (go unit_be <|> go unit_le) where
  go u = do
    bom <- fitYield u
    if bom == 0xFEFF
      then return $ fitYield u
      else empty

utf16_char :: Monoid p => Phase p Word16 o Char
utf16_char = do
  hs <- fromIntegral <$> get :: Phase p Word16 o Int
  case () of
   _ | hs <= 0xD7FF || hs >= 0xE000 -> return $ toEnum hs
     | hs >= 0xD800 && hs <= 0xDBFF -> do
       ls <- fromIntegral <$> get :: Phase p Word16 o Int
       "UTF-16: Invalid low surrogate" <?> guard (ls >= 0xDC00 && ls <= 0xDFFF)
       return $ toEnum $ 0x010000 + 0x0400 * (hs - 0xD800) + (ls - 0xDC00)
     | otherwise -> fail "UTF-16: Invalid high surrogate"

mkStream :: Monoid p => Phase p i a a -> Phase p i a ()
mkStream u = go where
  go = ((u >>= yield) >> go) <|> return ()

utf16_word16_stream :: Monoid p => Phase p Word16 Char ()
utf16_word16_stream = mkStream utf16_char

utf16_stream_useBOM :: Monoid p => Phase p Word8 Char ()
utf16_stream_useBOM = do
  unit <- useBOM_unit :: Monoid p =>
    Phase p Word8 Char (Phase p Word8 Word16 Word16)
  mkStream unit >># utf16_word16_stream

utf16_stream_le :: Monoid p => Phase p Word8 Char ()
utf16_stream_le = mkStream unit_le >># utf16_word16_stream

utf16_stream_be :: Monoid p => Phase p Word8 Char ()
utf16_stream_be = mkStream unit_be >># utf16_word16_stream

utf16_stream_unknown :: Monoid p => Phase p Word8 Char ()
utf16_stream_unknown = flip (<|>) (return ()) $ do
  unit <- return unit_le <|> return unit_be
  h <- fitYield unit
  case h of
    0xFEFF -> mkStream (fitYield unit) >># utf16_word16_stream
    0xFFFE -> fail "Reversed byte order mark"
    _ -> mkStream (fitYield unit) >># (put1 h >> utf16_word16_stream)

