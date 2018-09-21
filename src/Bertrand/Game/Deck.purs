module Bertrand.Game.Deck
  ( Deck
  , CardSet
  , SelectableCard
  , trySet
  , claimSet
  , visibleCards
  , shuffledDeck
  , selectVisibleCard
  )
  where

import Prelude
import Data.Array ((!!), drop, take, sortWith, delete, union)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect.Random (randomInt)

import Effect (Effect)
import Bertrand.Game.Card (Card, allCards)

data Deck = Deck (Array SelectableCard) (Array Card)
data CardSet = CardSet Card Card Card

data SelectableCard =
    Selected Card
  | NotSelected Card

selectVisibleCard :: Deck -> Card -> Deck
selectVisibleCard (Deck vis rem) card =
  let updater sCard = case selectedEqualsCard sCard card of
                        true -> selectCard sCard
                        false -> sCard
      newVis = updater <$> vis
   in Deck newVis rem

selectedEqualsCard :: SelectableCard -> Card -> Boolean
selectedEqualsCard (Selected card) card' = card == card'
selectedEqualsCard (NotSelected card) card' = card == card'

selectCard :: SelectableCard -> SelectableCard
selectCard (Selected card) = Selected card
selectCard (NotSelected card) = Selected card

trySet :: Card -> Card -> Card -> Maybe CardSet
trySet c c' c'' = Just $ CardSet c c' c''

claimSet :: CardSet -> Deck -> Deck
claimSet (CardSet one two three) (Deck visible remaining) =
  let newDraw = NotSelected <$> take 3 remaining
      newRemaining = drop 3 remaining
      newVis = union newDraw $ delete one $ delete two $ delete three visible
   in Deck newVis newRemaining

   -- THIS MAY BE BAD BECAUSE CardSet doesn't have SelectableCards. UGH
   -- probably fine, actually

shuffledDeck :: Effect Deck
shuffledDeck = do
  sortable <- traverse mkSortableCard allCards
  let shuffled = toCard <$> sortWith toIndex sortable
      visible = take 12 shuffled
      remaining = drop 12 shuffled
  pure $ Deck (NotSelected <$> visible) remaining

mkSortableCard :: Card -> Effect SortableCard
mkSortableCard card = do
  i <- randomInt 0 200
  pure $ SortableCard card i

toCard :: SortableCard -> Card
toCard (SortableCard card _) = card

toIndex :: SortableCard -> Int
toIndex (SortableCard _ i) = i

data SortableCard = SortableCard Card Int

visibleCards :: Deck -> Array SelectableCard
visibleCards (Deck visible _) = visible
