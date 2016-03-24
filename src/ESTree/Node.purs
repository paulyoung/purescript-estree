module ESTree.Node where

import Data.Either (Either(Left))
import Data.Foreign (ForeignError(JSONError))
import Data.Foreign.Class (class IsForeign, readProp, read)
import Data.Generic (class Generic, gCompare, gEq, gShow)

import ESTree.Expression (Expression())
import ESTree.Pattern (Pattern())

import Prelude (class Eq, class Ord, class Show, ($), (++), bind, return)


data Node
  = Expression Expression
  | Pattern Pattern


derive instance genericNode :: Generic Node


instance isForeignNode :: IsForeign Node where
  read f = do
    _type <- readProp "type" f

    case _type of
      "Identifier" -> readExpression f -- readPattern f
      "ThisExpression" -> readExpression f
      "ArrayExpression" -> readExpression f
      unrecognized -> do
        Left $ JSONError $ "Unrecognized Node type: \"" ++ unrecognized ++ "\""

    where

    readExpression f = do
      expression <- read f
      return $ Expression expression

    readPattern f = do
      pattern <- read f
      return $ Pattern pattern


instance showNode :: Show Node where
  show = gShow


instance eqNode :: Eq Node where
  eq = gEq


instance ordNode :: Ord Node where
  compare = gCompare
