module Test.ESTree.Node where

import Control.Monad.Eff.Console (log)

import Data.Either (Either(Left, Right))
import Data.Foreign (ForeignError(JSONError))
import Data.Foreign.Class (readJSON)

import ESTree.Node (Node(..))

import Prelude (Unit(), (==), (<$>), (++), bind)

import Test.ESTree.Expression (FakeExpression(..), showExpressionJSON)
import Test.Helpers (showTestFailure)
import Test.QuickCheck (QC(), Result(), (<?>), quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (elements)


checkNode :: QC () Unit
checkNode = do

  log "Check JSON decoding for Node"
  quickCheck' 100 decoding

  log "Check error handling for unrecognized Node types"
  quickCheck' 1 unrecognizedType

  where

  decoding :: FakeNode -> Result
  decoding (FakeNode node) = do
    let json = showNodeJSON node

    let expected = Right node
    let actual = readJSON json

    actual == expected <?> showTestFailure actual expected json

  unrecognizedType :: Result
  unrecognizedType = do
    let json =
      "{ \"type\": \"Test\"" ++
      "}"

    let expected = Left (JSONError "Unrecognized Node type: \"Test\"") :: Either ForeignError Node
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
