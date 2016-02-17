module Test.Main where

import Prelude (Unit(), bind)

import Test.ESTree.Position (checkPosition)

import Test.QuickCheck (QC())


main :: QC () Unit
main = do
  checkPosition
