module Test.ESTree.SourceLocation where

import Control.Monad.Eff.Console (log)

import Data.Either (Either(Right))
import Data.Foreign.Class (readJSON)
import Data.Maybe (maybe)

import ESTree.SourceLocation (SourceLocation(..))

import Prelude (Unit(), ($), (++), (==), (<$>), bind, return, show)

import Test.ESTree.Position (FakePosition(..), showPositionJSON)
import Test.Helpers (showTestFailure)
import Test.QuickCheck (QC(), Result(), (<?>), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)


checkSourceLocation :: QC () Unit
checkSourceLocation = do

  log "Check JSON decoding for SourceLocation"
  quickCheck decoding

  where

  decoding :: FakeSourceLocation -> Result
  decoding (FakeSourceLocation sourceLocation) = do
    let json = showSourceLocationJSON sourceLocation

    let expected = Right sourceLocation
    let actual = readJSON json

    actual == expected <?> showTestFailure actual expected json


showSourceLocationJSON :: SourceLocation -> String
showSourceLocationJSON (SourceLocation sourceLocation) =
  "{ \"start\": " ++ showPositionJSON sourceLocation.start ++
  ", \"end\": " ++ showPositionJSON sourceLocation.end ++
  (maybe "" (\x -> ", \"source\": " ++ show x) sourceLocation.source) ++
  "}"


newtype FakeSourceLocation = FakeSourceLocation SourceLocation

instance arbSourceLocation :: Arbitrary FakeSourceLocation where
  arbitrary = FakeSourceLocation <$> do
    source <- arbitrary
    (FakePosition start) <- arbitrary
    (FakePosition end) <- arbitrary

    return $ SourceLocation
      { source: source
      , start: start
      , end: end
      }
