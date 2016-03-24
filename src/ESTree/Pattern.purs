module ESTree.Pattern where

import Data.Either (Either(Left))
import Data.Foreign (ForeignError(JSONError))
import Data.Foreign.Class (class IsForeign, readProp)
import Data.Foreign.NullOrUndefined (runNullOrUndefined)
import Data.Generic (class Generic, gCompare, gEq, gShow)

import ESTree.Identifier (Identifier(Identifier))

import Prelude (class Eq, class Ord, class Show, ($), (++), (<$>), bind, return)


data Pattern
  = IdentifierPattern Identifier


derive instance genericPattern :: Generic Pattern


instance isForeignPattern :: IsForeign Pattern where
  read f = do
    _type <- readProp "type" f
    loc <- runNullOrUndefined <$> readProp "loc" f

    case _type of
      "Identifier" -> do
        name <- readProp "name" f

        return $ IdentifierPattern $ Identifier
          { type: _type
          , loc: loc
          , name: name
          }

      unrecognized -> do
        Left $ JSONError $ "Unrecognized Pattern type: \"" ++ unrecognized ++ "\""


instance showPattern :: Show Pattern where
  show = gShow


instance eqPattern :: Eq Pattern where
  eq = gEq


instance ordPattern :: Ord Pattern where
  compare = gCompare
