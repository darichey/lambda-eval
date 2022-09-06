module App (appComponent) where

import Prelude

import Data.Array (catMaybes, length, mapWithIndex, snoc, toUnfoldable, (..))
import Data.Either (Either(..), hush)
import Data.List (List(..), (:))
import Data.Traversable (for)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Halogen (PropName(..))
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Lambda.Model (Term(..))
import Lambda.Parse (term)
import Lambda.Pretty (prettyPrint)
import Lambda.Reduce (reduce)
import Parsing (ParseError, parseErrorMessage, runParser)
import Type.Proxy (Proxy(..))

type CellState = { name :: String, value :: String }

type Binding = { name :: String, value :: Term }

type Environment = List Binding

withEnv :: Environment -> Term -> Term
withEnv Nil t = t
withEnv (b : bs) t = withEnv bs (App (Abs b.name t) b.value)

appComponent :: forall query input output m. MonadAff m => H.Component query input output m
appComponent = Hooks.component \{ slotToken } _ -> Hooks.do
  expression /\ expressionId <- Hooks.useState ""
  cells /\ cellsId <- Hooks.useState []
  environment /\ environmentId <- Hooks.useState Nil

  Hooks.pure do
    HH.div
      [ twclass "grid grid-cols-12 grid-rows-3" ]
      [ HH.div [ twclass "col-span-2" ] [ HH.text "let" ]
      , HH.div
          [ twclass "col-span-10" ]
          [ HH.div_ $ mapWithIndex (\i cell -> HH.slot _cell i cellComponent cell (const $ pure unit)) cells
          , HH.button
              [ HE.onClick \_ -> Hooks.modify_ cellsId (flip snoc { name: "", value: "" }) ]
              [ HH.text "Add cell" ]
          ]
      , HH.div [ twclass "col-span-2" ] [ HH.text "in" ]
      , HH.div
          [ twclass "col-span-10" ]
          [ HH.input
              [ twclass "border-2 border-black"
              , HP.value expression
              , HE.onValueInput \s -> do
                  env <- (for (0 .. (length cells - 1)) \i -> Hooks.request slotToken _cell i GetTerm)
                  Hooks.put environmentId (toUnfoldable $ catMaybes env)
                  Hooks.put expressionId s
              ]
          ]
      , HH.div
          [ twclass "col-span-full" ]
          [ parseResultDiv (runParser expression term) \t ->
              let
                reduced = reduce $ withEnv environment t
              in
                HH.div_ [ HH.text $ prettyPrint reduced ]
          ]
      ]

data Query a = GetTerm (Binding -> a)

_cell = Proxy :: Proxy "cell"

cellComponent :: forall output m. MonadAff m => H.Component Query CellState output m
cellComponent = Hooks.component \{ queryToken } { name: iName, value: iValue } -> Hooks.do
  name /\ nameId <- Hooks.useState iName
  value /\ valueId <- Hooks.useState iValue

  let parsed = runParser value term

  Hooks.useQuery queryToken case _ of
    GetTerm reply -> pure $ reply <$> { name, value: _ } <$> hush parsed

  Hooks.pure do
    HH.div
      [ twclass "border-2 border-black p-2 mb-2" ]
      [ HH.div_
          [ HH.input
              [ twclass "border-2 border-black"
              , HP.value name
              , HE.onValueInput $ Hooks.put nameId
              ]
          , HH.text " = "
          , HH.input
              [ twclass "border-2 border-black"
              , HP.value value
              , HE.onValueInput $ Hooks.put valueId
              ]
          ]
      , parseResultDiv parsed (const (HH.div_ []))
      ]

parseResultDiv :: forall w i. Either ParseError Term -> (Term -> HTML w i) -> HTML w i
parseResultDiv parsed f = case parsed of
  Left error -> HH.div
    [ twclass "text-red-400" ]
    [ HH.text $ parseErrorMessage error ]
  Right t -> f t

twclass :: forall r i. String -> HP.IProp (class :: String | r) i
twclass = HP.prop (PropName "className")
