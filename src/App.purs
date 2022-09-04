module App (appComponent) where

import Prelude

import Data.Either (Either(..))
import Data.List (List(..), filter, reverse, toUnfoldable, (:))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (debug)
import Effect.Unsafe (unsafePerformEffect)
import Halogen (PropName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Lambda.Model (Term(..))
import Lambda.Parse (term)
import Lambda.Pretty (prettyPrint)
import Lambda.Reduce (reduce)
import Parsing (parseErrorMessage, runParser)

type Binding = { name :: String, value :: Term }

type Environment = List Binding

deleteBinding :: String -> Environment -> Environment
deleteBinding name = filter (\b -> b.name /= name)

putBinding :: Binding -> Environment -> Environment
putBinding binding environment = binding : (deleteBinding binding.name environment)

withEnv :: Environment -> Term -> Term
withEnv Nil t = t
withEnv (b : bs) t = withEnv bs (App (Abs b.name t) b.value)

appComponent :: forall query input output m. MonadAff m => H.Component query input output m
appComponent = Hooks.component \_ _ -> Hooks.do
  environment /\ environmentId <- Hooks.useState Nil

  name /\ nameId <- Hooks.useState ""
  expression /\ expressionId <- Hooks.useState ""

  Hooks.pure do
    HH.div
      [ twclass "inline-flex flex-col" ]
      [ HH.text "Environment"
      , HH.div_ $ toUnfoldable $ reverse $ environment <#> (\{ name, value } -> HH.div_ [ HH.text $ name <> " = " <> prettyPrint value ])
      , HH.text "Name"
      , HH.input
          [ HP.value name
          , HE.onValueInput \input -> Hooks.put nameId input
          , twclass "border-black border-2"
          ]
      , HH.text "Expression"
      , HH.input
          [ HP.value expression
          , HE.onValueInput \input -> Hooks.put expressionId input
          , twclass "border-black border-2"
          ]
      , case runParser expression term of
          Left error -> HH.div
            [ twclass "text-red-400" ]
            [ HH.text $ parseErrorMessage error ]
          Right t -> HH.div
            [ twclass "inline-flex flex-col" ]
            [ HH.div_ [ HH.text $ "Input: " <> prettyPrint t ]
            , HH.div_ [ HH.text $ "w/ env: " <> (prettyPrint $ withEnv environment t) ]
            , HH.div_ [ HH.text $ "Reduced: " <> (prettyPrint $ reduce $ withEnv environment t) ]
            , HH.button
                [ HE.onClick \_ -> Hooks.modify_ environmentId (putBinding { name, value: t }) ]
                [ HH.text "Add to env" ]
            ]
      ]

twclass :: forall r i. String -> HP.IProp (class :: String | r) i
twclass = HP.prop (PropName "className")
