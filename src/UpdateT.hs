{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module UpdateT where

import Data.Functor.Identity
import Data.Monoid
import Types

data UpdateT p s m a = UpdateT {
  runUpdateT :: s -> m (p, a)
} deriving (Functor)

instance (ApplyAction p s, Monad m) => Applicative (UpdateT p s m) where
  pure a = UpdateT . const . return $ (mempty, a)
  UpdateT u <*> UpdateT t = UpdateT $ \s -> do
    (p, f) <- u s 
    (p', a) <- t (applyAction p s) 
    return (p' <> p, f a)

instance (ApplyAction p s, Monad m) => Monad (UpdateT p s m) where
  return = pure
  UpdateT u >>= k = UpdateT $ \s -> do
    (p, a) <- u s
    (p', a') <- runUpdateT (k a) (applyAction p s)
    return (p <> p', a')

instance (ApplyAction p s, Monad m) => MonadUpdate (UpdateT p s m) p s where
  putAction p = UpdateT $ \_ -> return (p, ())
  getState = UpdateT $ \s -> return (mempty, s)

evalUpdateT :: (ApplyAction p s, Functor m) => UpdateT p s m a -> s -> m a
evalUpdateT (UpdateT f) s = snd <$> f s

execUpdateT :: (ApplyAction p s, Monad m) => UpdateT p s m a -> s -> m s
execUpdateT u s = snd <$> runUpdateT (u *> getState) s

collectUpdateT :: (ApplyAction p s, Functor m) => UpdateT p s m a -> s -> m p
collectUpdateT u s = fst <$> runUpdateT u s

auditUpdateT :: (ApplyAction p s, Monad m) => UpdateT p s m a -> s -> m (s, p, a)
auditUpdateT u s = do
  (p, (a, s')) <- runUpdateT ((,) <$> u <*> getState) s
  return (s', p, a)

resultWithLogUpdateT :: (ApplyAction p s, Monad m) => UpdateT p s m a -> s -> m (p, s)
resultWithLogUpdateT u s = do
  (p, (_, s')) <- runUpdateT ((,) <$> u <*> getState) s
  return (p, s') 