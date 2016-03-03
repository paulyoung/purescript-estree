module ESTree.Position where

import Data.Foreign.Class (class IsForeign)
import Data.Foreign.Generic (defaultOptions, readGeneric)
import Data.Generic (class Generic, gCompare, gEq, gShow)

import Prelude (class Eq, class Ord, class Show, ($))


data Position = Position
  { line :: Int
  , column :: Int
  }


derive instance genericPosition :: Generic Position


instance isForeignPosition :: IsForeign Position where
  read = readGeneric $ defaultOptions { unwrapNewtypes = true }


instance showPosition :: Show Position where
  show = gShow


instance eqPosition :: Eq Position where
  eq = gEq


instance ordPosition :: Ord Position where
  compare = gCompare
