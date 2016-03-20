module ESTree.Identifier where

import Data.Foreign.Class (class IsForeign)
import Data.Foreign.Generic (defaultOptions, readGeneric)
import Data.Generic (class Generic, gCompare, gEq, gShow)
import Data.Maybe (Maybe())

import ESTree.SourceLocation (SourceLocation())

import Prelude (class Eq, class Ord, class Show, ($))


data Identifier = Identifier
  { type :: String
  , loc :: Maybe SourceLocation
  , name :: String
  }


derive instance genericIdentifier :: Generic Identifier


instance isForeignIdentifier :: IsForeign Identifier where
  read = readGeneric $ defaultOptions { unwrapNewtypes = true }


instance showIdentifier :: Show Identifier where
  show = gShow


instance eqIdentifier :: Eq Identifier where
  eq = gEq


instance ordIdentifier :: Ord Identifier where
  compare = gCompare
