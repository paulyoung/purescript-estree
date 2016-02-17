module Test.Main where

import Prelude (Unit(), bind)

import Test.ESTree.Expression (checkExpression)
import Test.ESTree.Node (checkNode)
import Test.ESTree.Position (checkPosition)
import Test.ESTree.SourceLocation (checkSourceLocation)

import Test.QuickCheck (QC())


main :: QC () Unit
main = do
  checkPosition
  checkSourceLocation
  checkExpression
  checkNode
