{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser
import Foreign (peekArray0)

-- Exercise 1 -----------------------------------------
-- >>> getSecret "./clues/dog-original.jpg" "./clues/dog.jpg"

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret p0 p1 = do
  content0 <- readFile p0
  content1 <- readFile p1
  return $ zipWith xor content0 content1

-- Exercise 2 -----------------------------------------
-- >>> decryptWithKey (getSecret "./clues/dog-original.jpg" "./clues/dog.jpg") "./clues/victims.json"
decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey bs p = do
  content <- readFile p
  let newContent = zipWith xor (concat $ repeat bs) content
  writeFile (concat p ".enc") newContent

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile p = do
  content <- readFile p
  return $ decode content

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victimsPath transactionsPath = do
  victims <- parseFile victimsPath
  transactions <- parseFile transactionsPath
  return $ filter `elem` victims transactions

-- Exercise 5 -----------------------------------------
-- let ts = [ Transaction { from = "Haskell Curry", to = "Simon Peyton Jones", amount = 10, tid = "534a8de8-5a7e-4285-9801-8585734ed3dc" } ]
-- getFlow ts == fromList [ ("Haskell Curry", -10), ("Simon Peyton Jones", 10) ]
getFlow :: [Transaction] -> Map String Integer
getFlow = go Map.empty
  where
    go m [] = m
    go m (x:xs) = go (insert (insert m (from x) (amount -x) (to x) (amount x))) xs

-- Exercise 6 -----------------------------------------
-- The criminal is the person that got the most money
getCriminal :: Map String Integer -> String
getCriminal = maxBy snd $ toList $ unionWith (+)

-- Exercise 7 -----------------------------------------
-- Separate the people into payers and payees; ie, people who ended
-- up with extra money and people who ended up at a loss.

-- Sort both groups in descending order. The payers who owe the
-- most and the payees who are owed the most should come first.
-- You will likely find the sortBy function in the Data.List module
-- helpful for this stage.

-- Iterate over the payers and payees in parallel. For each pair, make
-- a new Transaction where the payer pays the payee the minimum
-- between how much the payer owes and how much the payee is
-- owed. Deduct this amount from both, remove anyone who has
-- completely paid his/her debt or has been completely paid off, and
-- repeat.
undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs flows tids = run payer payee
  where
    (payer, payee) = partition (> 0) $ sort $ filter (==0) $ toList $ unionWith (+) flows
    payee = reverse payee
    run tids xtions [] _ = xtions
    run tids xtions _ [] = xtions
    run tids xtions (x:xs) (y:ys) = run (tail tids) Transaction { from = fst payee, to = fst payer, amount = minAmount, tid = head tids }:xtions newXs newYs
      where
        minAmount = min (snd payee) (-(snd payer))
        newXs = if snd payer == -minAmount then xs else (fst payer, snd payer + minAmount)
        newYs = if snd payee == minAmount then ys else (fst payee, snd payee - minAmount)

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON p transactions = writeFile p $ encode transactions

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts       
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
--   putStrLn crim

-- >>> main

-- main :: IO ()
-- main = do
--   bs <- getSecret "./clues/dog-original.jpg" "./clues/dog.jpg"
--   decryptWithKey bs "./clues/victims.json"
