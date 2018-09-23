module Main where

import Prelude
import Effect (Effect)
import Effect.Class (liftEffect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Bertrand.UI.Board as Board
import Bertrand.Game.Deck (shuffledDeck)
import Web.HTML (window)
import Web.HTML.Window (location)
import Web.HTML.Location (href)
import Data.String (Pattern(..), split)
import Data.Maybe (fromMaybe)
import Data.Array (last)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  deck <- liftEffect shuffledDeck
  imgRoot <- liftEffect imageRoot
  runUI (Board.component imgRoot deck) unit body

imageRoot :: Effect String
imageRoot = do
  w <- window
  l <- location w
  h <- href l
  pure $ fromMaybe "" $ last $ split (Pattern "?") h
