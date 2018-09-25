{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Update
import Data.Monoid
import Types
import Writer
import Data.Tuple (swap)

processTransaction :: AccountAction -> BankBalance -> BankBalance
processTransaction (Deposit n) (BankBalance b) = BankBalance $ n + b
processTransaction (Withdraw n) (BankBalance b) 
  | (n > b) = BankBalance 0
  | otherwise = BankBalance $ b - n
processTransaction ApplyInterest (BankBalance b) =
  BankBalance (truncate $ fromIntegral b * 1.1)

-- not clear why do we need to use all that Endo machinery here??
-- instance ApplyAction [AccountAction] BankBalance where
--   applyAction actions balance =
--     let allTransactions :: BankBalance -> BankBalance
--         allTransactions =
--           appEndo $ foldMap (Endo . processTransaction) (reverse actions)
--      in allTransactions balance

instance ApplyAction [AccountAction] BankBalance where
  applyAction = flip . foldl . flip $ processTransaction

useATM :: Update [AccountAction] BankBalance ()
useATM = do
  putAction [Deposit 30]
  putAction [Deposit 20]
  putAction [ApplyInterest]
  putAction [Withdraw 10]

logATM :: Writer [AccountAction] ()
logATM = do
  writer ((), [Deposit 10])
  tell [Deposit 30]
  tell [Deposit 20]
  tell [ApplyInterest]
  tell [Withdraw 10]

main :: IO ()
main = do
  putStrLn . show $ resultWithLogUpdate useATM (BankBalance 0)

  -- writer
  putStrLn 
    . show 
    . flip evalUpdate () 
    . listen 
    $ logATM -- listen demonstartion

  putStrLn 
    . show 
    . swap 
    . flip resultWithLogUpdate () 
    . pass 
    $ ((\v -> (v, reverse)) <$> logATM) -- transforning log with pass
