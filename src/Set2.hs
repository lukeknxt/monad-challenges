{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}

module Set2 where

import MCPrelude

data Maybe a = Nothing | Just a deriving (Eq)

instance Show a => Show (Maybe a) where
  show m = case m of
    Just a -> "Just " ++ show a
    Nothing -> "Nothing"

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x : xs) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay [x] = Just []
tailMay (x : xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay a [] = Nothing
lookupMay a [x] = if fst x == a then Just (snd x) else Nothing
lookupMay a l = lookupMay a (filter (\(one, two) -> one == a) l)

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay x y
  | y == 0 = Nothing
           | otherwise = Just $ x / y

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay [x] = Just x
maximumMay (x : y : xs) = maximumMay ((if x >= y then x else y) : xs)

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay [x] = Just x
minimumMay (x : y : xs) = minimumMay ((if x <= y then x else y) : xs)

queryGreek :: GreekData -> String -> Maybe Double
queryGreek gd s = case lookupMay s gd of
  (Just xs) -> case tailMay xs of
    (Just tail) -> case maximumMay tail of
      (Just max) -> case headMay xs of
        (Just head) -> divMay (fromIntegral max) (fromIntegral head)
        Nothing -> Nothing
      Nothing -> Nothing
    Nothing -> Nothing
  Nothing -> Nothing

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain f ma = case ma of
  (Just x) -> f x
  Nothing -> Nothing

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link = flip chain

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 gd s =
  link
  (lookupMay s gd)
    ( \xs ->
        link
    (tailMay xs)
          ( \tail ->
              link
      (maximumMay tail)
                ( \max ->
                    link
                      (headMay xs)
                      (divMay (fromIntegral max) . fromIntegral)
      )
    )
  )

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries salaries person1 person2 = case lookupMay person1 salaries of
  (Just p1Salary) -> case lookupMay person2 salaries of
    (Just p2Salary) -> Just $ p1Salary + p2Salary
    Nothing -> Nothing
  Nothing -> Nothing

mkMaybe :: a -> Maybe a
mkMaybe = Just 

ylink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
ylink f ma mb = link ma (\a -> link mb (mkMaybe . f a))

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 salaries person1 person2 = 
  ylink (+) (lookupMay person1 salaries) (lookupMay person2 salaries)

