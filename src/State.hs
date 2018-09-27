{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module State where

import Types
import Update
import UpdateT
import Data.Monoid

type State s a = Update [s] s a
type StateT s m a = UpdateT [s] s m a

instance ApplyAction [a] a where
  applyAction = flip const

state :: (s -> (s, a)) -> State s a
state f = Update $ \s -> 
  let (s', a) = f s
  in ([s'], a)

stateT :: Monad m => (s -> (s, a)) -> StateT s m a
stateT f = UpdateT $ \s -> do
 let (s', a) = f s
 return ([s'], a)

get :: State s s
get = state $ (,) <$> id <*> id

getT :: Monad m => StateT s m s
getT = stateT $ (,) <$> id <*> id

put :: s -> State s ()
put s = state $ const (s, ())

putT :: Monad m => s -> StateT s m ()
putT s = stateT $ const (s, ())

modify :: (s -> s) -> State s ()
modify f = state $ \s -> (f s, ())

modifyT :: Monad m => (s -> s) -> StateT s m ()
modifyT f = stateT $ \s -> (f s, ())