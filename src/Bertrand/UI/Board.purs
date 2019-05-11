module Bertrand.UI.Board where

import Prelude
import Data.Array ((:), length, slice)
import Data.Maybe (Maybe(..), maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Bertrand.Game.Card (Card, cardImageUrl)
import Bertrand.Game.Deck as Deck

type State = { deck :: Deck.Deck
             , sets :: Array Deck.CardSet
             , warning :: Maybe String
             , imageRootUrl :: String
             }

data Query a =
    Select Card a
  | Deselect Card a
  | AskForMoreCards a
  | DismissWarning a

data Message = Message

component :: forall m.
             String
          -> Deck.Deck
          -> H.Component HH.HTML Query Unit Message m
component imageRootUrl deck =
  H.component
    { initialState: const $ initialState imageRootUrl deck
    , render
    , eval
    , receiver: const Nothing
    }

initialState :: String -> Deck.Deck -> State
initialState imageRootUrl deck =
  { deck: deck
  , sets: []
  , warning: Nothing
  , imageRootUrl: imageRootUrl
  }

render :: State -> H.ComponentHTML Query
render state =
    HH.div [ HP.class_ (HH.ClassName "section") ]
        [ HH.div [ HP.class_ (HH.ClassName "container") ]
            [ renderGameStatus state
            , HH.div [ HP.class_ (HH.ClassName "is-ancestor") ]
                [ HH.div [ HP.class_ (HH.ClassName "tile is-parent") ] $
                    (renderSelectableCard state) <$> (slice 0 3 $ Deck.visibleCards state.deck)
                , HH.div [ HP.class_ (HH.ClassName "tile is-parent") ] $
                    (renderSelectableCard state) <$> (slice 3 6 $ Deck.visibleCards state.deck)
                , HH.div [ HP.class_ (HH.ClassName "tile is-parent") ] $
                    (renderSelectableCard state) <$> (slice 6 9 $ Deck.visibleCards state.deck)
                , HH.div [ HP.class_ (HH.ClassName "tile is-parent") ] $
                    (renderSelectableCard state) <$> (slice 9 12 $ Deck.visibleCards state.deck)
                ]
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
  HH.div [ HP.class_ (HH.ClassName "columns") ]
      [ HH.div [ HP.class_ (HH.ClassName "column") ]
          [ HH.text "Found sets: "
          , HH.text $ show $ length state.sets
          ]
      , HH.div [ HP.class_ (HH.ClassName "column") ]
          [ HH.text "Remaining cards: "
          , HH.text $ show $ length $ Deck.remainingCards state.deck
          ]
      ]

renderSelectableCard :: forall p. State -> Card -> HH.HTML p (Query Unit)
renderSelectableCard state card =
  let selected = Deck.isCardSelected state.deck card
      cssClass = case selected of
                   true -> "tile is-child is-4 selected"
                   false -> "tile is-child is-4"
      event = case selected of
                   true -> Deselect
                   false -> Select
   in HH.div [ HP.class_ (HH.ClassName cssClass)
             , HE.onClick (HE.input_ (event card))
             ]
             [ HH.img [ HP.src $ cardImageUrl state.imageRootUrl card ]
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
