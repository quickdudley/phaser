module Codec.Phaser.UTF16 (
  utf16_char,
  utf16_word16_stream,
  utf16_stream_useBOM,
  utf16_stream_le,
  utf16_stream_be,
  utf16_stream_unknown,
  utf16_encode_stream_be_nobom,
  utf16_encode_stream_le_nobom,
  utf16_encode_stream_be,
  utf16_encode_stream_le,
  utf16_encode_char
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

encode_unit_be :: Word16 -> Phase p i Word8 ()
encode_unit_be c = do
  yield $ fromIntegral $ shiftR c 8
  yield $ fromIntegral $ c .&. 0xFF

encode_unit_le :: Word16 -> Phase p i Word8 ()
encode_unit_le c = do
  yield $ fromIntegral $ c .&. 0xFF
  yield $ fromIntegral $ shiftR c 8

useBOM_unit :: Monoid p => Phase p Word8 o1 (Phase p Word8 o2 Word16)
useBOM_unit = "UTF-16: No byte order mark" <?> (go unit_be <|> go unit_le) where
  go u = do
    bom <- fitYield u
    if bom == 0xFEFF
      then return $ fitYield u
      else empty

-- | Consume one or two 16 bit words and decode to a 'Char' using UTF-16
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

utf16_encode_char :: Char -> Phase p i Word16 ()
utf16_encode_char c = let
  cc = fromEnum c
  in case () of
   _ | cc <= 0xD7FF || (cc >= 0xE000 && cc <= 0xFFFF) ->
         yield $ fromIntegral cc
     | cc >= 0x10000 && cc <= 0x10FFFF -> do
         let s = cc .&. complement 0x010000
         yield $ fromIntegral $ shiftR s 10 .|. 0xD800
         yield $ fromIntegral $ (s .&. 0x03FF) .|. 0xDC00
     | otherwise -> fail "Character not representable in UTF-16"

mkStream :: Monoid p => Phase p i a a -> Phase p i a ()
mkStream u = go where
  go = ((u >>= yield) >> go) <|> return ()

mkEncodeStream :: Monoid p => (c -> Phase p c o a) -> Phase p c o ()
mkEncodeStream f = go where
  go = (get >>= f >> go) <|> return ()

utf16_word16_stream :: Monoid p => Phase p Word16 Char ()
utf16_word16_stream = mkStream utf16_char

-- | Decode bytes to characters using UTF-16, using the byte order mark to
-- detect endianness.
utf16_stream_useBOM :: Monoid p => Phase p Word8 Char ()
utf16_stream_useBOM = do
  unit <- useBOM_unit :: Monoid p =>
    Phase p Word8 Char (Phase p Word8 Word16 Word16)
  toPhase $ mkStream unit >># utf16_word16_stream

-- | Decode bytes to characters using UTF-16, little endian mode
utf16_stream_le :: Monoid p => Automaton p Word8 Char ()
utf16_stream_le = mkStream unit_le >># utf16_word16_stream

-- | Decode bytes to characters using UTF-16, big endian mode
utf16_stream_be :: Monoid p => Automaton p Word8 Char ()
utf16_stream_be = mkStream unit_be >># utf16_word16_stream

-- | Decode bytes to characters using UTF-16, using byte order mark if
-- available, otherwise trying both byte orders. Downstream parser may be used
-- to disambiguate.
utf16_stream_unknown :: Monoid p => Phase p Word8 Char ()
utf16_stream_unknown = flip (<|>) (return ()) $ do
  unit <- return unit_le <|> return unit_be
  h <- fitYield unit
  case h of
    0xFEFF -> toPhase $ mkStream (fitYield unit) >># utf16_word16_stream
    0xFFFE -> fail "Reversed byte order mark"
    _ -> toPhase $ mkStream (fitYield unit) >># (put1 h >> utf16_word16_stream)

-- | Encode a stream of characters to 16 bit units
utf16_encode_stream_word16 :: Monoid p => Phase p Char Word16 ()
utf16_encode_stream_word16 = mkEncodeStream utf16_encode_char

-- | Encode a stream of characters to bytes using UTF-16, big endian without
-- the byte order mark.
utf16_encode_stream_be_nobom :: Monoid p => Automaton p Char Word8 ()
utf16_encode_stream_be_nobom =
  utf16_encode_stream_word16 >># mkEncodeStream encode_unit_be

-- | Encode a stream of characters to bytes using UTF-16, little endian without
-- the byte order mark.
utf16_encode_stream_le_nobom :: Monoid p => Automaton p Char Word8 ()
utf16_encode_stream_le_nobom =
  utf16_encode_stream_word16 >># mkEncodeStream encode_unit_le

-- | Encode a stream of characters to bytes using UTF-16, little endian
-- including the byte order mark.
utf16_encode_stream_be :: Monoid p => Automaton p Char Word8 ()
utf16_encode_stream_be =
  (yield 0xFEFF >> utf16_encode_stream_word16) >># mkEncodeStream encode_unit_be

-- | Encode a stream of characters to bytes using UTF-16, big endian
-- including the byte order mark.
utf16_encode_stream_le :: Monoid p => Automaton p Char Word8 ()
utf16_encode_stream_le =
  (yield 0xFEFF >> utf16_encode_stream_word16) >># mkEncodeStream encode_unit_le
