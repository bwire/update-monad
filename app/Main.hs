{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Update
import Data.Monoid
import Types
import Writer
import Reader
import State
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

-- State behavior imitation with writer

-- I need an Account action to know what operation is going to be performed and 
-- and I produce a Kleisly arrow to work in a monad chain.
writerOp :: AccountAction -> (BankBalance -> Writer [AccountAction] BankBalance)
writerOp w = \b -> writer (processTransaction w b, [w])

-- the same as useATM, but using only Writer functionality (without State part)
-- Important: I don't need to wrap every action inside a list now
useATMWithWriter :: BankBalance -> Writer [AccountAction] BankBalance
useATMWithWriter init = 
  writer (init, mempty)
   >>= writerOp (Deposit 30)
   >>= writerOp (Deposit 20)
   >>= writerOp ApplyInterest
   >>= writerOp (Withdraw 10)

-- I have no idea how to use Reader with all these bank stuff, except of balance changing without logging.
readerOp :: AccountAction -> (BankBalance -> Reader AccountAction BankBalance)
readerOp r = reader . const . processTransaction r 

-- Not a good idea completely thow, because there we need to somehow 
-- provide information about upcoming Action as an input parameter on every step. 
-- In other words - I don't use an environment at all, actually I dpn't care about any environment because all the logic
-- doesn't depend on the environment - which is nonsense for the Reader.. :). But I can't use the environment for this task - 
-- it must be shared through all monadic operations. It's useless in this case.
useATMWithReader :: BankBalance -> Reader AccountAction BankBalance
useATMWithReader init = 
  reader (const init)
    >>= readerOp (Deposit 30)
    >>= readerOp (Deposit 20)
    >>= readerOp ApplyInterest
    >>= readerOp (Withdraw 10)

-- hellper to hide the fact that we need to pass something to extract a closure value from the resulting function 
getBalanceWithReader :: Reader AccountAction BankBalance -> BankBalance
getBalanceWithReader = flip evalUpdate undefined

-- With state 
-- Actually I like it better than the initial Update version with useATM.
-- Using MonadUpdate typeclass force us not to use 'a' parameter of the Update!!!
-- But why not use it? In the initial scheme it always ().
-- And in this case I don't need apply action at all - since it's equal to (flip const) everywhere - thus return the state unchanged! :))
stateOp :: AccountAction -> (BankBalance -> State AccountAction BankBalance)
stateOp s = \b -> Update $ \_ -> ([s], processTransaction s b)
 
useATMWithState :: Update [AccountAction] AccountAction BankBalance
useATMWithState = do
  return (BankBalance 0)
    >>= stateOp (Deposit 30)
    >>= stateOp (Deposit 20)
    >>= stateOp ApplyInterest
    >>= stateOp (Withdraw 10)
 
getResultWithState :: State AccountAction BankBalance -> ([AccountAction], BankBalance)
getResultWithState = flip runUpdate undefined

main :: IO ()
main = do
  putStrLn . show $ resultWithLogUpdate useATM (BankBalance 0)

  -- writer
  putStrLn 
    . show 
    . swap
    . flip evalUpdate () 
    . listen 
    $ logATM -- listen demonstartion

  putStrLn 
    . show 
    . flip resultWithLogUpdate () 
    . pass 
    $ ((\v -> (v, reverse)) <$> logATM) -- transforning log with passˇ

  putStrLn 
    . show 
    . swap
    . flip evalUpdate () 
    . listen 
    $ useATMWithWriter (BankBalance 0)

  -- reader
  putStrLn 
    . show 
    . getBalanceWithReader 
    . useATMWithReader 
    $ BankBalance 0

  -- state
  putStrLn . show $ getResultWithState useATMWithState