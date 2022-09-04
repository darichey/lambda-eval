module App (appComponent) where

import Prelude

import Data.Tuple.Nested ((/\))
import Halogen.Hooks as Hooks
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

appComponent = Hooks.component \_ input -> Hooks.do
  count /\ countId <- Hooks.useState 0

  Hooks.pure do
    HH.button
      [ HE.onClick \_ -> Hooks.modify_ countId (_ + 1) ]
      [ HH.text $ show count ]
