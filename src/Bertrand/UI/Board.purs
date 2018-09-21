module Bertrand.UI.Board where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Bertrand.Game.Card (Card)
import Bertrand.Game.Deck (Deck, SelectionResult(..), visibleCards, selectVisibleCard, deselectVisibleCard, isCardSelected)
import Bertrand.UI.Card (renderCard)

type State = Deck

data Query a =
    Select Card a
  | Deselect Card a

data Message = Message

component :: forall m. Deck -> H.Component HH.HTML Query Unit Message m
component deck =
  H.component
    { initialState: const $ initialState deck
    , render
    , eval
    , receiver: const Nothing
    }

initialState :: Deck -> State
initialState deck = deck

render :: State -> H.ComponentHTML Query
render state =
    HH.div [ HP.class_ (HH.ClassName "grid-x grid-margin-x medium-up-3") ] $
      (renderSelectableCard state) <$> (visibleCards state)

renderSelectableCard :: forall p. Deck -> Card -> HH.HTML p (Query Unit)

renderSelectableCard deck card =
  let cssClass = case isCardSelected deck card of
                   true -> "cell selected"
                   false -> "cell"
      event = case isCardSelected deck card of
                   true -> Deselect
                   false -> Select
   in HH.div [ HP.class_ (HH.ClassName cssClass)
             , HE.onClick (HE.input_ (event card))
             ]
             [ renderCard card
             ]

eval :: forall m. Query ~> H.ComponentDSL State Query Message m
eval = case _ of
  Select card next -> do
    state <- H.get
    case selectVisibleCard state card of
         NoSet d -> H.put d
         FoundSet d _ -> H.put d
--    H.raise $ Toggled nextState
    pure next

  Deselect card next -> do
    state <- H.get
    H.put $ deselectVisibleCard state card
--    H.raise $ Toggled nextState
    pure next

