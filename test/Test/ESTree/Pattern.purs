module Test.ESTree.Pattern where

import Control.Monad.Eff.Console (log)

import Data.Either (Either(Left, Right))
import Data.Foreign (ForeignError(JSONError))
import Data.Foreign.Class (readJSON)

import ESTree.Pattern (Pattern(..))

import Prelude (Unit(), (==), (<$>), (++), bind)

import Test.ESTree.Identifier (FakeIdentifier(..), showIdentifierJSON)

import Test.Helpers (showTestFailure)
import Test.QuickCheck (QC(), Result(), (<?>), quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (elements)


checkPattern :: QC () Unit
checkPattern = do

  log "Check JSON decoding for Pattern"
  quickCheck' 100 decoding

  log "Check error handling for unrecognized Pattern types"
  quickCheck' 1 unrecognizedType

  where

  decoding :: FakePattern -> Result
  decoding (FakePattern node) = do
    let json = showPatternJSON node

    let expected = Right node
    let actual = readJSON json

    actual == expected <?> showTestFailure actual expected json

  unrecognizedType :: Result
  unrecognizedType = do
    let json =
      "{ \"type\": \"Test\"" ++
      "}"

    let expected = Left (JSONError "Unrecognized Pattern type: \"Test\"") :: Either ForeignError Pattern
    let actual = readJSON json

    actual == expected <?> showTestFailure actual expected json


showPatternJSON :: Pattern -> String
showPatternJSON (IdentifierPattern identifier) = showIdentifierJSON identifier


newtype FakePattern
  = FakePattern Pattern


instance arbPattern :: Arbitrary FakePattern where
  arbitrary = FakePattern <$> do
    (FakeIdentifier identifier) <- arbitrary

    elements (IdentifierPattern identifier)
      [
      ]
