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

randLetter :: Gen Char
randLetter s = (toLetter i, s') where (i, s') = rand s

randString3 :: String
randString3 = [one, two, three]
 where
  (one  , s1) = randLetter $ mkSeed 1
  (two  , s2) = randLetter s1
  (three, _ ) = randLetter s2

randEven :: Gen Integer
randEven = generalA (* 2)

randOdd :: Gen Integer
randOdd = generalA ((+ 1) . (* 2))

randTen :: Gen Integer
randTen = generalA (* 10)

generalA :: (Integer -> Integer) -> Gen Integer
generalA l s = (l i, s') where (i, s') = rand s

randEven2 :: Gen Integer
randEven2 = generalA2 (* 2) rand

randOdd2 :: Gen Integer
randOdd2 = generalA2 ((+ 1) . (* 2)) rand

randTen2 :: Gen Integer
randTen2 = generalA2 (* 10) rand

generalA2 :: (a -> b) -> Gen a -> Gen b
generalA2 l r s = (l i, s') where (i, s') = r s

randPair :: Gen (Char, Integer)
randPair s = ((c, i), s3)
 where
  (c, s2) = randLetter s
  (i, s3) = rand s2

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair ga gb s = ((a, b), s3)
 where
  (a, s2) = ga s
  (b, s3) = gb s2

generalB :: Gen a -> Gen b -> (a -> b -> c) -> Gen c
generalB ga gb f s = (f a b, s3)
 where
  (a, s2) = ga s
  (b, s3) = gb s2

generalPair2 :: Gen a -> Gen b -> Gen (a, b)
generalPair2 ga gb = generalB ga gb (,)