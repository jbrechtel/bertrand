module Bertrand.Game.Deck
  ( Deck
  , CardSet
  , SelectionResult(..)
  , visibleCards
  , shuffledDeck
  , selectVisibleCard
  , deselectVisibleCard
  , isCardSelected
  )
  where

import Prelude
import Data.Array (catMaybes, drop, take, sortWith, nub, length)
import Data.Traversable (traverse)
import Effect.Random (randomInt)

import Effect (Effect)
import Bertrand.Game.Card (Card, allCards, cardProperties, blankCard)

data Deck = Deck (Array Card) (Array Card) Selection
data Selection =
    NoCards
  | OneCard Card
  | TwoCards Card Card

data CardSet = CardSet Card Card Card
data Draw = Draw Card Card Card

data SelectionResult =
    NoSet Deck
  | FoundSet Deck CardSet

selectVisibleCard :: Deck -> Card -> SelectionResult
selectVisibleCard (Deck vis rem NoCards) card =
  NoSet $ Deck vis rem (OneCard card)

selectVisibleCard (Deck vis rem (OneCard sel)) card =
  NoSet $ Deck vis rem (TwoCards sel card)

selectVisibleCard deck@(Deck vis rem (TwoCards one two)) three =
  case isSet one two three of
       false -> NoSet deck
       true ->
         let cardSet = CardSet one two three
             (Draw newOne newTwo newThree) = take3OrBlank rem
             newRemaining = drop 3 rem
             newVis = replace one newOne
                        $ replace two newTwo
                        $ replace three newThree vis
          in FoundSet (Deck newVis newRemaining NoCards) cardSet


take3OrBlank :: Array Card -> Draw
take3OrBlank cards =
  let attempt = take 3 cards
   in case attempt of
        [one, two, three] -> Draw one two three
        _                 -> Draw blankCard blankCard blankCard

replace :: forall a. Eq a => a -> a -> Array a -> Array a
replace old new items =
  let updater c = if c == old
                    then new
                    else c
   in updater <$> items

isSet :: Card -> Card -> Card -> Boolean
isSet c c' c'' =
  case catMaybes (cardProperties <$> [c, c', c'']) of
       [] -> false
       props ->
         let colors = nub (_.color <$> props)
             shapeCounts = nub (_.shapeCount <$> props)
             shapes = nub (_.shape <$> props)
             shadings = nub (_.shading <$> props)
          in    length colors /= 2
             && length shapeCounts /= 2
             && length shapes /= 2
             && length shadings /= 2

deselectVisibleCard :: Deck -> Card -> Deck
deselectVisibleCard deck@(Deck vis rem NoCards) _ = deck

deselectVisibleCard deck@(Deck vis rem (OneCard c)) card =
  if c /= card
    then deck
    else Deck vis rem NoCards

deselectVisibleCard deck@(Deck vis rem (TwoCards c c')) card =
  if c == card
    then Deck vis rem (OneCard c')
    else if c' == card
           then Deck vis rem (OneCard c)
           else deck

isCardSelected :: Deck -> Card -> Boolean
isCardSelected (Deck _ _ sel) card =
  case sel of
       OneCard c -> card == c
       TwoCards c c' -> card == c || card == c'
       _ -> false

shuffledDeck :: Effect Deck
shuffledDeck = do
  sortable <- traverse mkSortableCard allCards
  let shuffled = toCard <$> sortWith toIndex sortable
      visible = take 12 shuffled
      remaining = drop 12 shuffled
  pure $ Deck visible remaining NoCards

mkSortableCard :: Card -> Effect SortableCard
mkSortableCard card = do
  i <- randomInt 0 200
  pure $ SortableCard card i

toCard :: SortableCard -> Card
toCard (SortableCard card _) = card

toIndex :: SortableCard -> Int
toIndex (SortableCard _ i) = i

data SortableCard = SortableCard Card Int

visibleCards :: Deck -> Array Card
visibleCards (Deck visible _ _) = visible
