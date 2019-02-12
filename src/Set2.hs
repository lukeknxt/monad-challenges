{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import           MCPrelude

data Maybe a = Nothing | Just a deriving Eq

instance Show a => Show (Maybe a) where
  show m = case m of
    Just a  -> "Just " ++ show a
    Nothing -> "Nothing"

headMay :: [a] -> Maybe a
headMay []       = Nothing
headMay (x : xs) = Just x

tailMay :: [a] -> Maybe [a]
tailMay []       = Nothing
tailMay (x : []) = Just []
tailMay (x : xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay a []  = Nothing
lookupMay a [x] = if (fst x) == a then Just (snd x) else Nothing
lookupMay a l   = lookupMay a (filter (\(one, two) -> one == a) l)

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay x y | y == 0    = Nothing
           | otherwise = Just $ x / y

maximumMay :: Ord a => [a] -> Maybe a
maximumMay []           = Nothing
maximumMay [x         ] = Just x
maximumMay (x : y : xs) = maximumMay ((if x >= y then x else y) : xs)

minimumMay :: Ord a => [a] -> Maybe a
minimumMay []           = Nothing
minimumMay [x         ] = Just x
minimumMay (x : y : xs) = minimumMay ((if x <= y then x else y) : xs)
