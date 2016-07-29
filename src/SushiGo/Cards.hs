module SushiGo.Cards (
  Card(..)
, Group(..)
, GroupResult(..)
, HandResult(..)
, score
, group
) where

import           Data.List (groupBy, sort)

data Card = Sashimi
          deriving (Eq, Ord, Show)

type Hand = [Card]
type CardsToPlay = [Card]

-- Groups are ALWAYS full
data Group c = SashimiGroup
             | TempuraGroup
             | WasabiGroup { nagiri :: c }
             deriving (Eq, Show)

data HandResult = HandResult {
                  toPlay :: CardsToPlay
                , toPass :: Hand
                } deriving (Eq, Show)

data GroupResult c = GroupResult {
                     groups    :: [Group c]
                   , remainder :: CardsToPlay
                   } deriving (Eq, Show)

score :: [Card] -> Int
score [Sashimi, Sashimi, Sashimi] = 10
score _ = 0

group :: CardsToPlay -> GroupResult Card
group cs = GroupResult { groups = [], remainder = [] }
  where cardSets = groupBy cardTypeEq $ sort cs

cardTypeEq :: Card -> Card -> Bool
cardTypeEq Sashimi Sashimi = True
cardTypeEq _ _ = False

