module Test.ESTree.Expression where

import Control.Monad.Eff.Console (log)

import Data.Either (Either(Left, Right))
import Data.Foldable (intercalate)
import Data.Foreign (ForeignError(JSONError))
import Data.Foreign.Class (readJSON)
import Data.Maybe (Maybe(Just), maybe)

import ESTree.Expression (Expression(..))

import Prelude (Unit(), (++), (==), (<$>), bind)

import Test.ESTree.SourceLocation (FakeSourceLocation(..), showSourceLocationJSON)
import Test.Helpers (showTestFailure)
import Test.QuickCheck (QC(), Result(), (<?>), quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (elements)


checkExpression :: QC () Unit
checkExpression = do

  log "Check JSON decoding for Expression"
  quickCheck' 100 decoding

  log "Check error handling for unrecognized Expression types"
  quickCheck' 1 unrecognizedType

  where

  decoding :: FakeExpression -> Result
  decoding (FakeExpression expression) = do
    let json = showExpressionJSON expression

    let expected = Right expression
    let actual = readJSON json

    actual == expected <?> showTestFailure actual expected json

  unrecognizedType :: Result
  unrecognizedType = do
    let json =
      "{ \"type\": \"Test\"" ++
      "}"

    let expected = Left (JSONError "Unrecognized Expression type: \"Test\"") :: Either ForeignError Expression
    let actual = readJSON json

    actual == expected <?> showTestFailure actual expected json


showExpressionJSON :: Expression -> String

showExpressionJSON (ThisExpression expression) =
  "{ \"type\": \"" ++ expression.type ++ "\"" ++
  (maybe "" (\x -> ", \"loc\": " ++ showSourceLocationJSON x) expression.loc) ++
  "}"

showExpressionJSON (ArrayExpression expression) =
  "{ \"type\": \"" ++ expression.type ++ "\"" ++
  (maybe "" (\x -> ", \"loc\": " ++ showSourceLocationJSON x) expression.loc) ++
  ", \"elements\": " ++
  "  [" ++ elements ++
  "  ]" ++
  "}"

  where

  elements = intercalate ", " (showExpressionJSON <$> expression.elements)


newtype FakeExpression = FakeExpression Expression

instance arbExpression :: Arbitrary FakeExpression where
  arbitrary = FakeExpression <$> do
    (FakeSourceLocation thisLoc) <- arbitrary
    (FakeSourceLocation arrayLoc) <- arbitrary

    elements (thisExpression thisLoc)
      [ (arrayExpression thisLoc arrayLoc)
      ]

    where

    thisExpression loc = ThisExpression
      { type: "ThisExpression"
      , loc: Just loc
      }

    arrayExpression arrayLoc thisLoc = ArrayExpression
      { type: "ArrayExpression"
      , loc: Just arrayLoc
      , elements:
        [ (thisExpression thisLoc)
        ]
      }
