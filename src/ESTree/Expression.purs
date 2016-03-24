module ESTree.Expression where

import Data.Either (Either(Left))
import Data.Foreign (ForeignError(JSONError))
import Data.Foreign.Class (class IsForeign, readProp)
import Data.Foreign.NullOrUndefined (runNullOrUndefined)
import Data.Generic (class Generic, gCompare, gEq, gShow)
import Data.Maybe (Maybe())

import ESTree.Identifier (Identifier(Identifier))
import ESTree.SourceLocation (SourceLocation())

import Prelude (class Eq, class Ord, class Show, ($), (++), (<$>), bind, return)


data Expression
  = IdentifierExpression Identifier
  | ThisExpression
    { type :: String
    , loc :: Maybe SourceLocation
    }
  | ArrayExpression
    { type :: String
    , loc :: Maybe SourceLocation
    , elements :: Array Expression
    }


derive instance genericExpression :: Generic Expression


instance isForeignExpression :: IsForeign Expression where
  read f = do
    _type <- readProp "type" f
    loc <- runNullOrUndefined <$> readProp "loc" f

    case _type of
      "Identifier" -> do
        name <- readProp "name" f

        return $ IdentifierExpression $ Identifier
          { type: _type
          , loc: loc
          , name: name
          }

      "ThisExpression" -> do
        return $ ThisExpression
          { type: _type
          , loc: loc
          }

      "ArrayExpression" -> do
        elements <- readProp "elements" f

        return $ ArrayExpression
          { type: _type
          , loc: loc
          , elements: elements
          }

      unrecognized -> do
        Left $ JSONError $ "Unrecognized Expression type: \"" ++ unrecognized ++ "\""


instance showSourceLocation :: Show Expression where
  show = gShow


instance eqExpression :: Eq Expression where
  eq = gEq


instance ordExpression :: Ord Expression where
  compare = gCompare
