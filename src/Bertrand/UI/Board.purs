module Bertrand.UI.Board where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Bertrand.Game.Card (Card)
import Bertrand.Game.Deck (Deck, visibleCards)
import Bertrand.UI.Card (renderCard)

type State =
  { selection :: Array Card
  , deck :: Deck
  }

data Query a
  = Toggle a

data Message =
    Select Card
  | Deselected Card

component :: forall m. Deck -> H.Component HH.HTML Query Unit Message m
component deck =
  H.component
    { initialState: const $ initialState deck
    , render
    , eval
    , receiver: const Nothing
    }

initialState :: Deck -> State
initialState deck =
  { deck: deck
  , selection: []
  }

render :: State -> H.ComponentHTML Query
render state =
    HH.div_ $ renderCard <$> (visibleCards state.deck)

eval :: forall m. Query ~> H.ComponentDSL State Query Message m
eval = case _ of
  Toggle next -> do
    state <- H.get
    H.put state
--    H.raise $ Toggled nextState
    pure next

