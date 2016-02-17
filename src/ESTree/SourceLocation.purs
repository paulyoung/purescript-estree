module ESTree.SourceLocation where

import Data.Foreign.Class (class IsForeign)
import Data.Foreign.Generic (defaultOptions, readGeneric)
import Data.Generic (class Generic, gCompare, gEq, gShow)
import Data.Maybe (Maybe())

import ESTree.Position (Position())

import Prelude (class Eq, class Ord, class Show, ($))


data SourceLocation = SourceLocation
  { source :: Maybe String
  , start :: Position
  , end :: Position
  }

derive instance genericSourceLocation :: Generic SourceLocation

instance isForeignSourceLocation :: IsForeign SourceLocation where
  read = readGeneric $ defaultOptions { unwrapNewtypes = true }

instance showSourceLocation :: Show SourceLocation where
  show = gShow

instance eqSourceLocation :: Eq SourceLocation where
  eq = gEq

instance ordSourceLocation :: Ord SourceLocation where
  compare = gCompare
