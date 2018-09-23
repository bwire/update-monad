{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Types where

newtype BankBalance = BankBalance Int 
  deriving (Eq, Ord, Show)

data AccountAction = 
    Deposit Int
  | Withdraw Int
  | ApplyInterest deriving (Eq, Ord, Show)
  
class Monoid p => ApplyAction p s where
  applyAction :: p -> s -> s

class (ApplyAction p s, Monad m) => MonadUpdate m p s | m -> s, m -> p
  where 
    putAction :: p -> m ()
    getState :: m s
