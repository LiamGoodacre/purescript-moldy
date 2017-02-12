module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foldable (foldMap)
import Data.Moldy (moldMap, moldy)
import Data.Char (toUpper)
import Data.String (singleton)

foo :: String
foo = "Foo"

foo' :: String
foo' = moldMap (singleton <<< toUpper) foo

foo'' :: String
foo'' = foldMap (singleton <<< toUpper) (moldy foo)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = when (foo' == foo'') $ log "Done!"
