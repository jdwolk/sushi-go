module SushiGo.Cards (
  Card(..)
, score
) where

data Card = Sashimi

score :: [Card] -> Int
score [Sashimi, Sashimi, Sashimi] = 10
score _ = 0
