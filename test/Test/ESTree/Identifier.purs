module Test.ESTree.Identifier where

import Control.Monad.Eff.Console (log)

import Data.Either (Either(Right))
import Data.Foreign.Class (readJSON)
import Data.Maybe (Maybe(Just), maybe)

import ESTree.Identifier (Identifier(..))

import Prelude (Unit(), ($), (++), (==), (<$>), bind, return)

import Test.ESTree.SourceLocation (FakeSourceLocation(..), showSourceLocationJSON)
import Test.Helpers (showTestFailure)
import Test.QuickCheck (QC(), Result(), (<?>), quickCheck)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)


checkIdentifier :: QC () Unit
checkIdentifier = do

  log "Check JSON decoding for Identifier"
  quickCheck decoding

  where

  decoding :: FakeIdentifier -> Result
  decoding (FakeIdentifier identifier) = do
    let json = showIdentifierJSON identifier

    let expected = Right identifier
    let actual = readJSON json

    actual == expected <?> showTestFailure actual expected json


showIdentifierJSON :: Identifier -> String
showIdentifierJSON (Identifier identifier) =
  "{ \"type\": \"" ++ identifier.type ++ "\"" ++
  (maybe "" (\x -> ", \"loc\": " ++ showSourceLocationJSON x) identifier.loc) ++
  ", \"name\": \"" ++ identifier.name ++ "\"" ++
  "}"


newtype FakeIdentifier
  = FakeIdentifier Identifier


instance arbIdentifier :: Arbitrary FakeIdentifier where
  arbitrary = FakeIdentifier <$> do
    (FakeSourceLocation loc) <- arbitrary
    name <- arbitrary

    return $ Identifier
      { type: "Identifier"
      , loc: Just loc
      , name: name
      }
