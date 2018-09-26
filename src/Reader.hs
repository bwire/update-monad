{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Reader where

import Types
import Update
import UpdateT

-- A simple implementation of Reader!
-- There're no sensible updates to do; so your state always stays the same.

type Reader r a = Update () r a
type ReaderT r m a = UpdateT () r m a

instance ApplyAction () r where
  applyAction = flip const 

reader :: (r -> a) -> Reader r a
reader f = Update $ \r -> (mempty, f r) 

readerT :: Monad m => (r -> a) -> ReaderT r m a
readerT f = UpdateT $ \r -> return (mempty, f r)

ask :: Reader r r
ask = reader id

askT :: Monad m => ReaderT r m r
askT = readerT id

local :: (r -> r) -> Reader r a -> Reader r a
local f (Update u) = Update $ u . f

localT :: (r -> r) -> ReaderT r m a -> ReaderT r m a
localT f (UpdateT u) = UpdateT $ u . f