module SushiGo.Cards (
  Card(..)
, Group(..)
, GroupResult(..)
, HandResult(..)
, score
, group
, sortByCard
) where

import           Data.List       (groupBy, head, sort)
import qualified Data.Map.Strict as M

data Card = Sashimi
          | Wasabi
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

group :: CardsToPlay -> M.Map Card [Card]--GroupResult Card
group cs = groupByCard cs

groupByCard :: [Card] -> M.Map Card [Card]
groupByCard cs = M.fromList zipped
  where grouped = groupBy (==) $ sort cs
        cards = map head grouped
        zipped = zip cards grouped

