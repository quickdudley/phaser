module Codec.Phaser.Text (

 ) where

import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL

import Control.Applicative

import Phaser.Core

-- | A 'Phase' which takes 'TS.Text's and yields their individual characters
unpackText :: Phase p TS.Text Char ()
unpackText = (go >> unpackText) <|> return () where
  go = get >>= TS.foldr (\w r -> yield w >> r) (return ())

-- | A 'Phase' which takes 'TL.Text's and yields their individual characters
unpackLazyText :: Phase p TL.Text Char ()
unpackLazyText = (go >> unpackLazyText) <|> return () where
  go = get >>= TL.foldr (\w r -> yield w >> r) (return ())

