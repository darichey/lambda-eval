-- module Main where

-- import Prelude

-- import Data.Either (Either(..))
-- import Effect (Effect)
-- import Effect.Class.Console (log, logShow)
-- import Lambda.Parse (term)
-- import Lambda.Pretty (prettyPrint)
-- import Parsing (runParser)

-- main :: Effect Unit
-- main = do
--   case runParser "(\\f. \\x. f x) (\\y. y) (\\z. z)" term of
--     Left e -> logShow e
--     Right t -> log $ prettyPrint t


module Main where

import Prelude
import App (appComponent)
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI appComponent unit body
