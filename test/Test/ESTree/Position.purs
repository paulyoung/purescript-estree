module Test.ESTree.Position where

import Control.Monad.Eff.Console (log)

import Data.Either (Either(Right))
import Data.Foreign.Class (readJSON)

import ESTree.Position (Position(..))

import Prelude (Show, Unit(), ($), (++), (==), (<$>), bind, return, show)

import Test.Helpers (showTestFailure)
import Test.QuickCheck (QC(), Result(), (<?>), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)


checkPosition :: QC () Unit
checkPosition = do

  log "Check JSON decoding for Position"
  quickCheck decoding

  where

  decoding :: FakePosition -> Result
  decoding (FakePosition position) = do
    let json = showPositionJSON position

    let expected = Right position
    let actual = readJSON json

    actual == expected <?> showTestFailure actual expected json


showPositionJSON :: Position -> String
showPositionJSON (Position position) =
  "{ \"line\": " ++ show position.line ++
  ", \"column\": " ++ show position.column ++
  "}"


newtype FakePosition = FakePosition Position

instance arbPosition :: Arbitrary FakePosition where
  arbitrary = FakePosition <$> do
    line <- arbitrary
    column <- arbitrary

    return $ Position
      { line: line
      , column: column
      }
