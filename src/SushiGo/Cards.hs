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

-- TODO: make this a monoid so I can use <>
-- to combine multiple GroupResults
data GroupResult c = HasGroups {
                     groups      :: [Group c]
                   , grRemainder :: CardsToPlay
                   }
                   | OnlyRemainder { onlyRemainder :: CardsToPlay }
                   | NoResult
                   deriving (Eq, Show)

instance WithRemainder (GroupResult c) where
  remainder (HasGroups { grRemainder = r }) = r
  remainder (OnlyRemainder { onlyRemainder = r }) = r

instance Monoid (GroupResult c) where
  mempty = OnlyRemainder []
  mappend (HasGroups gsA rA)
          (HasGroups gsB rB) = HasGroups (gsA <> gsB) (rA <> rB)
  mappend (HasGroups gs rA) (OnlyRemainder rB) = HasGroups gs (rA <> rB)
  mappend (OnlyRemainder rA) (HasGroups gs rB) = HasGroups gs (rA <> rB)
  mappend (OnlyRemainder rA) (OnlyRemainder rB) = OnlyRemainder (rA <> rB)
  mappend NoResult other = other
  mappend other NoResult = other
  mappend _ _ = NoResult

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
  where gsHelp (Sashimi:Sashimi:Sashimi:cs) gr = gsHelp cs (gr <> HasGroups [SashimiGroup] [])
        gsHelp cs gr = gr <> OnlyRemainder cs

