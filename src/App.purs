module App (appComponent) where

import Prelude

import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Halogen (PropName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Lambda.Parse (term)
import Lambda.Pretty (prettyPrint)
import Lambda.Reduce (reduce)
import Parsing (parseErrorMessage, runParser)

appComponent :: forall query input output m. MonadAff m => H.Component query input output m
appComponent = Hooks.component \_ _ -> Hooks.do
  expression /\ expressionId <- Hooks.useState ""

  Hooks.pure do
    HH.div
      [ twclass "flex flex-col" ]
      [ HH.input
          [ HP.value expression
          , HE.onValueInput \input -> Hooks.put expressionId input
          , twclass "border-red-500 border-2"
          ]
      , HH.text $ case runParser expression term of
          Left error -> parseErrorMessage error
          Right t -> prettyPrint $ reduce t
      ]

twclass :: forall r i. String -> HP.IProp (class :: String | r) i
twclass = HP.prop (PropName "className")
