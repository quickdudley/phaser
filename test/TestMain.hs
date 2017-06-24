import Data.List
import Test.QuickCheck

import Codec.Phaser.Core
import Codec.Phaser.Common

main = quickCheck testScientific

testScientific :: Double -> Bool
testScientific v = case parse_ () scientificNotation (show v) of
  Right l@(_:_) -> all (== v) l
  _ -> False
