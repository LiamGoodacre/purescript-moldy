module Test.Main where

import Prelude

import Data.Foldable (foldMap)
import Data.Moldy (moldMap, moldy)
import Data.String (singleton, toUpper)
import Effect (Effect)
import Effect.Console (log)

foo :: String
foo = "Foo"

foo' :: String
foo' = moldMap (toUpper <<< singleton) foo

foo'' :: String
foo'' = foldMap (toUpper <<< singleton) (moldy foo)

main :: Effect Unit
main = when (foo' == foo'') $ log "Done!"
