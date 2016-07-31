module SushiGo.Cards (
  Card(..)
, Group(..)
, GroupResult(..)
, HandResult(..)
, WithRemainder(..)
, score
, group
, groupSashimi
, separateCardTypes
) where

import           Data.List       (groupBy, head, sort)
import qualified Data.Map.Strict as M
import           Data.Monoid     ((<>))

data Card = Sashimi
          | Wasabi
            deriving (Eq, Ord, Show)

type Hand = [Card]
type CardsToPlay = [Card]

-- Groups are ALWAYS full
data Group c = SashimiGroup
             | TempuraGroup
             | WasabiGroup { nagiri :: c }
             | NoGroup
             deriving (Eq, Show)

data HandResult = HandResult {
                  toPlay :: CardsToPlay
                , toPass :: Hand
                } deriving (Eq, Show)

class WithRemainder gr where
  remainder :: gr -> CardsToPlay

data GroupResult c = GroupResult {
                     groups      :: [Group c]
                   , grRemainder :: CardsToPlay
                   }
                   | NoResult
                   deriving (Eq, Show)

instance WithRemainder (GroupResult c) where
  remainder (GroupResult { grRemainder = r }) = r
  remainder NoResult = []

instance Monoid (GroupResult c) where
  mempty = GroupResult [] []
  mappend (GroupResult gsA rA)
          (GroupResult gsB rB) = GroupResult (gsA <> gsB) (rA <> rB)
  mappend NoResult other = other
  mappend other NoResult = other

score :: [Card] -> Int
score [Sashimi, Sashimi, Sashimi] = 10
score _ = 0

group :: CardsToPlay -> M.Map Card [Card]--GroupResult Card
group = undefined
{-group cs = groupSashimi cs-}
  {-where separated = separateCardTypes cs-}

separateCardTypes :: [Card] -> M.Map Card [Card]
separateCardTypes cs = M.fromList zipped
  where grouped = groupBy (==) $ sort cs
        cards = map head grouped
        zipped = zip cards grouped

groupSashimi :: CardsToPlay -> GroupResult Card
groupSashimi cs = gsHelp cs mempty
  where gsHelp (Sashimi:Sashimi:Sashimi:cs) gr = gsHelp cs (gr <> GroupResult [SashimiGroup] [])
        gsHelp cs gr = gr <> GroupResult [] cs

