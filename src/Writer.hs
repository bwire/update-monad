{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Writer where

import Types
import Update
import UpdateT
import Data.Tuple

--A simple implementation of Writer!
--The state doesn't matter; only the monoidal actions are tracked!
instance Monoid m => ApplyAction m () where
  applyAction = flip const

type Writer w a = Update w () a
type WriterT w m a = UpdateT w () m a

writer :: (a, w) -> Writer w a
writer = Update . const . swap

writerT :: Monad m => (a, w) -> WriterT w m a
writerT = UpdateT . const . return . swap

tell :: w -> Writer w ()
tell = writer . (,) () 

tellT :: Monad m => w -> WriterT w m ()
tellT = writerT . (,) ()

listen :: Writer w a -> Writer w (a, w)
listen (Update f) = Update $ \s -> 
  let ~(w, a) = f s
  in (w, (a, w))

listenT :: Monad m => WriterT w m a -> WriterT w m (a, w)
listenT (UpdateT f) = UpdateT $ \s -> do 
  ~(w, a) <- f s
  return (w, (a, w))

pass :: Writer w (a, w -> w) -> Writer w a
pass (Update f) = Update $ \s ->
  let ~(w, (a, g)) = f s
  in (g w, a)

passT :: Monad m => WriterT w m (a, w -> w) -> WriterT w m a
passT (UpdateT f) = UpdateT $ \s -> do
  ~(w, (a, g)) <- f s
  return (g w, a) 