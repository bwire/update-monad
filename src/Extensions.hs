{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Extensions where

import Types
import Update
import Data.Tuple

--A simple implementation of Writer!
--The state doesn't matter; only the monoidal actions are tracked!
instance Monoid m => ApplyAction m () where
  applyAction = flip const

type UpdateWriter w a = Update w () a

updateWriter :: (a, w) -> UpdateWriter w a
updateWriter (a, w) = Update $ \_ -> (w, a)

updateTell :: w -> UpdateWriter w ()
updateTell w = Update $ \_ -> (w, ())

updateListen :: UpdateWriter w a -> (w, a)
updateListen = flip runUpdate ()

