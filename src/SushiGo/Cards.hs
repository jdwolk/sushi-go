module SushiGo.Cards (
  Card(..)
, Group(..)
, GroupResult(..)
, HandResult(..)
, WithRemainder(..)
, score
, group
, groupSashimi
, getCards
, separateCardTypes
) where

import           Data.List       (concatMap, dropWhile, groupBy, head, sort,
                                  takeWhile)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)
import           Data.Monoid     ((<>))

data Card = Sashimi
          | Tempura
          | Wasabi
          | Dumpling
          | Maki { numMaki :: Int }
            deriving (Ord, Show)

instance Eq Card where
  (Maki _) == (Maki _) = True
  Tempura == Tempura = True
  Sashimi == Sashimi = True
  Wasabi == Wasabi = True
  Dumpling == Dumpling = True
  _ == _ = False

type Hand = [Card]
type CardsToPlay = [Card]

-- Groups are ALWAYS full
data Group c = SashimiGroup
             | TempuraGroup
             | WasabiGroup { nagiri :: c }
             | DumplingGroup { totalDumplings :: Int }
             | MakiGroup { totalMaki :: Int }
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

group :: CardsToPlay -> GroupResult Card
group cs = sashimiGroups
        <> tempuraGroups
        <> dumplingGroups
        <> makiGroups
  where separated = separateCardTypes cs
        sashimiGroups = groupSashimi $ getCards Sashimi separated
        tempuraGroups = groupTempura $ getCards Tempura separated
        dumplingGroups = groupDumplings $ getCards Dumpling separated
        makiGroups = groupMaki $ getCards (Maki 1) separated

getCards :: Card -> M.Map Card [Card] -> [Card]
getCards c sortedCs = fromMaybe [] $ M.lookup c sortedCs

separateCardTypes :: [Card] -> M.Map Card [Card]
separateCardTypes cs = M.fromList zipped
  where grouped = groupBy (==) $ sort cs
        cards = map head grouped
        zipped = zip cards grouped

-- Sashimi groups have 3 sashimi each; rest are remainder
groupSashimi :: CardsToPlay -> GroupResult Card
groupSashimi cs = gsHelp cs mempty
  where gsHelp (Sashimi:Sashimi:Sashimi:cs) gr = gsHelp cs (gr <> GroupResult [SashimiGroup] [])
        gsHelp cs gr = gr <> GroupResult [] cs

-- Tempura groups have 2 tempura each; rest are remainder
groupTempura :: CardsToPlay -> GroupResult Card
groupTempura cs = tHelp cs mempty
  where tHelp (Tempura:Tempura:cs) gr = tHelp cs (gr <> GroupResult [TempuraGroup] [])
        tHelp cs gr = gr <> GroupResult [] cs

-- Dumpling groups have >= 1 dumplings
groupDumplings :: CardsToPlay -> GroupResult Card
groupDumplings cs = GroupResult dumplingGroups notDumplings
  where dumplingGroups = if numDumplings > 0 then [DumplingGroup numDumplings] else []
        numDumplings = length dumplings
        dumplings = (takeWhile (== Dumpling) cs)
        notDumplings = (dropWhile (== Dumpling) cs)

groupMaki :: CardsToPlay -> GroupResult Card
groupMaki cs = GroupResult makiGroups notMaki
  where makiGroups = if makiCount > 0 then [MakiGroup $ sum (map numMaki maki)] else []
        makiCount = length maki
        maki = (takeWhile (== (Maki 1)) cs)
        notMaki = (dropWhile (== (Maki 1)) cs)

