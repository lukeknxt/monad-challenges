module Set1 where

import           MCPrelude

type Gen a = Seed -> (a, Seed)

fiveRands :: [Integer]
fiveRands = [one, two, three, four, five]
 where
  (one  , s1) = rand $ mkSeed 1
  (two  , s2) = rand s1
  (three, s3) = rand s2
  (four , s4) = rand s3
  (five , _ ) = rand s4