{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Update
import Data.Monoid

newtype BankBalance = BankBalance Int 
  deriving (Eq, Ord, Show)

data AccountAction = 
    Deposit Int
  | Withdraw Int
  | ApplyInterest deriving (Eq, Ord, Show)

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

main :: IO ()
main = do
  let rp = resultWithLog useATM (BankBalance 0)
  putStrLn . show $ rp
