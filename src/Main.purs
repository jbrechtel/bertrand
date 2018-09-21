module Main where

import Prelude
import Effect (Effect)
import Effect.Class (liftEffect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Bertrand.UI.Board as Board
import Bertrand.Game.Deck (shuffledDeck)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  deck <- liftEffect shuffledDeck
  runUI (Board.component deck) unit body
