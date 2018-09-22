module Bertrand.UI.Board where

import Prelude
import Data.Array ((:), length)
import Data.Maybe (Maybe(..), maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Bertrand.Game.Card (Card)
import Bertrand.Game.Deck as Deck
import Bertrand.UI.Card (renderCard)

type State = { deck :: Deck.Deck
             , sets :: Array Deck.CardSet
             , warning :: Maybe String
             }

data Query a =
    Select Card a
  | Deselect Card a
  | AskForMoreCards a
  | DismissWarning a

data Message = Message

component :: forall m. Deck.Deck -> H.Component HH.HTML Query Unit Message m
component deck =
  H.component
    { initialState: const $ initialState deck
    , render
    , eval
    , receiver: const Nothing
    }

initialState :: Deck.Deck -> State
initialState deck =
  { deck: deck
  , sets: []
  , warning: Nothing
  }

render :: State -> H.ComponentHTML Query
render state =
    HH.div_
        [ HH.div [ HP.class_ (HH.ClassName "cell small-8") ]
            [ renderGameStatus state
            , HH.div [ HP.class_ (HH.ClassName "grid-x grid-margin-x medium-up-3") ] $
                (renderSelectableCard state.deck) <$> (Deck.visibleCards state.deck)
            ]
        , maybe (HH.text "") renderWarning state.warning
        , HH.div [ HP.class_ (HH.ClassName "cell small-2") ]
          [ HH.button [ HP.class_ (HH.ClassName "primary button")
                      , HP.type_ HP.ButtonButton
                      , HE.onClick (HE.input_ AskForMoreCards)
                      ]
                [ HH.text "No Set!"
                ]
          ]
        ]

renderWarning :: forall p. String -> HH.HTML p (Query Unit)
renderWarning warning =
  HH.div [ HP.class_ (HH.ClassName "grid-x") ]
      [ HH.div [ HP.class_ (HH.ClassName "cell small-12") ]
            [ HH.div [ HP.class_ (HH.ClassName "callout warning")
                     , HE.onClick (HE.input_ DismissWarning)
                     ]
                  [ HH.text warning
                  ]
            ]
      ]

renderGameStatus :: forall p. State -> HH.HTML p (Query Unit)
renderGameStatus state =
  HH.div [ HP.class_ (HH.ClassName "grid-x") ]
      [ HH.div [ HP.class_ (HH.ClassName "cell small-6") ]
          [ HH.text "Found sets: "
          , HH.text $ show $ length state.sets
          ]
      , HH.div [ HP.class_ (HH.ClassName "cell small-6") ]
          [ HH.text "Remaining cards: "
          , HH.text $ show $ length $ Deck.remainingCards state.deck
          ]
      ]

renderSelectableCard :: forall p. Deck.Deck -> Card -> HH.HTML p (Query Unit)
renderSelectableCard deck card =
  let cssClass = case Deck.isCardSelected deck card of
                   true -> "cell selected"
                   false -> "cell"
      event = case Deck.isCardSelected deck card of
                   true -> Deselect
                   false -> Select
   in HH.div [ HP.class_ (HH.ClassName cssClass)
             , HE.onClick (HE.input_ (event card))
             ]
             [ renderCard card
             ]

eval :: forall m. Query ~> H.ComponentDSL State Query Message m
eval (Select card next) = do
  state <- H.get
  case Deck.selectVisibleCard state.deck card of
       Deck.NoSet d -> H.put $ state { deck = d }
       Deck.FoundSet d s -> H.put $ state { deck = d, sets = s : state.sets }
  pure next

eval (Deselect card next) = do
  state <- H.get
  H.put $ state { deck = Deck.deselectVisibleCard state.deck card }
  pure next

eval (AskForMoreCards next) = do
  state <- H.get
  case Deck.askForMoreCards state.deck of
       Deck.Fine deck -> H.put $ state { deck = deck }
       Deck.SetsVisible -> H.modify_ $ showWarning "There are still sets visible!"

  pure next

eval (DismissWarning next) = do
  H.modify_ hideWarning
  pure next

showWarning :: String -> State -> State
showWarning warning state = state { warning = Just warning }

hideWarning :: State -> State
hideWarning state = state { warning = Nothing }
