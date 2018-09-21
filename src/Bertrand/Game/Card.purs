module Bertrand.Game.Card
  ( Card
  , CardProperties
  , Color(..)
  , Shape(..)
  , Shading(..)
  , ShapeCount(..)
  , isValid
  , allCards
  , cardImageUrl
  ) where

import Prelude
import Data.Array ((:))
import Data.Enum (class Enum, succ)
import Data.Maybe (Maybe(..))
import Data.Ordering (Ordering(..))
import Data.Ord (class Ord)

data Color =
    Red
  | Green
  | Purple

instance colorEnum :: Enum Color where
  succ Red = Just Green
  succ Green = Just Purple
  succ Purple = Nothing

  pred Red = Just Green
  pred Green = Just Purple
  pred Purple = Nothing

instance colorBounded :: Bounded Color where
  top = Red
  bottom = Purple

instance colorOrd :: Ord Color where
  compare Red Red = EQ
  compare Red _ = LT

  compare Green Green = EQ
  compare Green Purple = LT
  compare Green Red = GT

  compare Purple Purple = EQ
  compare Purple _ = GT

derive instance eqColor :: Eq Color

data Shape =
    Diamond
  | Oval
  | Squiggly

instance shapeEnum :: Enum Shape where
  succ Diamond = Just Oval
  succ Oval = Just Squiggly
  succ Squiggly = Nothing

  pred Diamond = Nothing
  pred Oval = Just Diamond
  pred Squiggly = Just Oval

instance shapeBounded :: Bounded Shape where
  top = Diamond
  bottom = Squiggly

instance shapeOrd :: Ord Shape where
  compare Diamond Diamond = EQ
  compare Diamond _ = LT

  compare Oval Oval = EQ
  compare Oval Squiggly = LT
  compare Oval Diamond = GT

  compare Squiggly Squiggly = EQ
  compare Squiggly _ = GT

derive instance eqShape :: Eq Shape

data Shading =
    Empty
  | Solid
  | Partial

instance shadingEnum :: Enum Shading where
  succ Empty = Just Solid
  succ Solid = Just Partial
  succ Partial = Nothing

  pred Empty = Nothing
  pred Solid = Just Empty
  pred Partial = Just Solid

instance shadingBounded :: Bounded Shading where
  top = Empty
  bottom = Partial

derive instance eqShading :: Eq Shading

instance shadingOrd :: Ord Shading where
  compare Empty Empty = EQ
  compare Empty _ = LT

  compare Solid Solid = EQ
  compare Solid Partial = LT
  compare Solid Empty = GT

  compare Partial Partial = EQ
  compare Partial _ = GT

data ShapeCount =
    One
  | Two
  | Three

instance shapeCountEnum :: Enum ShapeCount where
  succ One = Just Two
  succ Two = Just Three
  succ Three = Nothing

  pred One = Nothing
  pred Two = Just One
  pred Three = Just Two

instance shapeCountBounded :: Bounded ShapeCount where
  top = One
  bottom = Three

instance shapeCountOrd :: Ord ShapeCount where
  compare One One = EQ
  compare One _ = LT

  compare Two Two = EQ
  compare Two Three = LT
  compare Two One = GT

  compare Three Three = EQ
  compare Three _ = GT

derive instance eqShapeCount :: Eq ShapeCount

newtype Card =
  Card CardProperties

derive instance eqCard :: Eq Card

type CardProperties =
  { shapeCount :: ShapeCount
  , shading :: Shading
  , color :: Color
  , shape :: Shape
  }

allCards :: Array Card
allCards = do
  shape <- enumerate
  shading <- enumerate
  color <- enumerate
  shapeCount <- enumerate
  pure $ Card { shapeCount: shapeCount, shading: shading, color: color, shape: shape }

enumerate :: forall e. Enum e => Bounded e => Array e
enumerate = enumerateFrom (Just top) []

enumerateFrom :: forall e. Enum e => Bounded e => Maybe e -> Array e -> Array e
enumerateFrom (Just e) items = enumerateFrom (succ e) (e:items)
enumerateFrom Nothing items = items

isValid :: Card -> Card -> Card -> Boolean
isValid _ _ _ = false

colorToString :: Color -> String
colorToString Red = "red"
colorToString Purple = "purple"
colorToString Green = "green"

shapeToString :: Shape -> String
shapeToString Diamond = "diamond"
shapeToString Oval = "oval"
shapeToString Squiggly = "squiggly"

shadingToString :: Shading -> String
shadingToString Empty = "empty"
shadingToString Solid = "solid"
shadingToString Partial = "partial"

shapeCountToString :: ShapeCount -> String
shapeCountToString One = "one"
shapeCountToString Two = "two"
shapeCountToString Three = "three"

cardImageUrl :: Card -> String
cardImageUrl (Card c) =
     "images/"
  <> (shapeCountToString c.shapeCount)
  <> "-"
  <> (shadingToString c.shading)
  <> "-"
  <> (colorToString c.color)
  <> "-"
  <> (shapeToString c.shape)
  <> ".png"
