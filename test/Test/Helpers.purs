module Test.Helpers where

import Prelude (class Show, (++), show)


showTestFailure :: forall a b c. (Show a) => (Show b) => (Show c) => a -> b -> c -> String
showTestFailure actual expected input = ""
  ++ "\n"
  ++ "Actual:" ++ "\n"
  ++ "  " ++ show actual ++ "\n"
  ++ "\n"
  ++ "Expected:" ++ "\n"
  ++ "  " ++ show expected ++ "\n"
  ++ "\n"
  ++ "Input:" ++ "\n"
  ++ "  " ++ show input ++ "\n"
