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
  (>>#),
  (>#>),
  (<?>),
  (>><),
  parse,
  parse_,
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
  sepBy,
  munch,
  munch1
 ) where

import Codec.Phaser.Core
import Codec.Phaser.Common

