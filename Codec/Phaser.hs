{-|
Module:     Codec.Parser
Copyright:  Jeremy List
License:    BSD-3
Maintainer: quick.dudley@gmail.com
-}
module Codec.Phaser (
  Phase,
  Automaton,
  Position(..),
  Standardized(..),
  (>>#),
  (>#>),
  (<?>),
  ($#$),
  parse,
  parse_,
  parseFile,
  parseFile_,
  parseHandle,
  parseHandle_,
  get,
  count,
  yield,
  put1,
  put,
  run,
  step,
  extract,
  satisfy,
  match,
  char,
  iChar,
  string,
  iString,
  integer,
  decimal,
  scientificNotation,
  sepBy,
  munch,
  munch1,
  trackPosition
 ) where

import Codec.Phaser.Core
import Codec.Phaser.Common
import Codec.Phaser.ByteString

