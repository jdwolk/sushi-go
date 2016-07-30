module SushiGo.Cards (
  Card(..)
, Group(..)
, GroupResult(..)
, HandResult(..)
, score
, group
, groupSashimi
, makeGroupResult
, separateCardTypes
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

-- TODO: make this a monoid so I can use <>
-- to combine multiple GroupResults
data GroupResult c = GroupResult {
                     groups    :: [Group c]
                   , remainder :: CardsToPlay
                   } deriving (Eq, Show)

makeGroupResult :: [Group Card] -> CardsToPlay -> GroupResult Card
makeGroupResult gs rdr = GroupResult { groups = gs, remainder = rdr }

score :: [Card] -> Int
score [Sashimi, Sashimi, Sashimi] = 10
score _ = 0

group :: CardsToPlay -> M.Map Card [Card]--GroupResult Card
group cs = separateCardTypes cs

separateCardTypes :: [Card] -> M.Map Card [Card]
separateCardTypes cs = M.fromList zipped
  where grouped = groupBy (==) $ sort cs
        cards = map head grouped
        zipped = zip cards grouped

groupSashimi :: CardsToPlay -> GroupResult Card
groupSashimi = undefined

