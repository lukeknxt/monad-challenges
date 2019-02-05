{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import           MCPrelude

data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
  show m = case m of
    Just a -> "Just " ++ show a 
    Nothing -> "Nothing"
