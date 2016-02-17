module Test.ESTree.Node where

import Control.Monad.Eff.Console (log)

import Data.Either (Either(Right))
import Data.Foreign.Class (readJSON)
import Data.Generic (Generic)

import ESTree.Node (Node(..))

import Prelude (Show, Unit(), (==), (<$>), bind)

import Test.ESTree.Expression (FakeExpression(..), showExpressionJSON)
import Test.Helpers (showTestFailure)
import Test.QuickCheck (QC(), Result(), (<?>), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Gen (elements)


checkNode :: QC () Unit
checkNode = do

  log "Check JSON decoding for Node"
  quickCheck decoding

  where

  decoding :: FakeNode -> Result
  decoding (FakeNode node) = do
    let json = showNodeJSON node

    let expected = Right node
    let actual = readJSON json

    actual == expected <?> showTestFailure actual expected json


showNodeJSON :: Node -> String
showNodeJSON (Expression expression) = showExpressionJSON expression


newtype FakeNode = FakeNode Node

instance arbNode :: Arbitrary FakeNode where
  arbitrary = FakeNode <$> do
    (FakeExpression expression) <- arbitrary

    elements (Expression expression)
      [
      ]
