module Bertrand.UI.Card
  ( renderCard
  )
  where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Bertrand.Game.Card (Card, cardImageUrl)

renderCard :: forall p i. Card -> HH.HTML p i
renderCard card =
  HH.img
    [ HP.src $ cardImageUrl card
    ]
