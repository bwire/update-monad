{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module Update where

import Data.Monoid

class Monoid p => ApplyAction p s where
  applyAction :: p -> s -> s

class (ApplyAction p s, Monad m) => MonadUpdate m p s | m -> s, m -> p
  where 
    putAction :: p -> m ()
    getState :: m s

data Update p s a = Update {
  runUpdate :: s -> (p, a)
} deriving Functor 

instance (ApplyAction p s) => Applicative (Update p s) where
  pure a = Update $ \_ -> (mempty, a)
  Update u <*> Update t = Update $ \s -> 
    let (p, f) = u s -- first update with the iitial state
        (p', a) = t (applyAction p s) -- second updtae with the new state altered by the applyAction
    in (p' <> p, f a)

instance (ApplyAction p s) => Monad (Update p s) where
  return = pure 
  u >>= k = Update $ \s ->
    let (p, v) = runUpdate u s 
        (p', v') = runUpdate (k v) (applyAction p s)
    in (p <> p', v')

instance (ApplyAction p s) => MonadUpdate (Update p s) p s where
  putAction p = Update $ \_ -> (p, ())
  getState = Update $ \s -> (mempty, s)

evalUpdate :: (ApplyAction p s) => Update p s a -> s -> a
evalUpdate (Update f) s = snd $ f s

execUpdate :: (ApplyAction p s) => Update p s a -> s -> s
execUpdate u s = snd (runUpdate (u *> getState) s)

collectUpdate :: (ApplyAction p s) => Update p s a -> s -> p
collectUpdate u s = fst $ runUpdate u s

-- u <*> getState --> the value in u is a function which takes a and returns result of getState (which is a pair - (mempty, s))
-- we apply (,) to the second elemet of the result a nd get (mempty, (, s)). According to the applicative instance we get
-- (mempty <> p, (a, s)) as a result. Not that clear at the first glance :))
auditUpdate :: (ApplyAction p s) => Update p s a -> s -> (s, p, a)
auditUpdate u s = 
  let (p, (a, s')) = runUpdate ((,) <$> u <*> getState) s
  in (s, p, a)

-- actually to get the result like this:
-- runUpdate useATM (BankBalance 0)
-- ([Deposit 20,Deposit 30,ApplyInterest,Withdraw 10],BankBalance 45)
-- we need a function like this:
resultWithLog :: (ApplyAction p s ) => Update p s a -> s -> (p, s)
resultWithLog u s =
  let (p, (_, s')) = runUpdate ((,) <$> u <*> getState) s
  in (p, s') 