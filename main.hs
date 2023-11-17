{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Data.Map (Map)
import Data.Set (Set, singleton)

data Promise = Pending | Fulfilled Int | Rejected Int deriving (Show)
type Addr = Integer
type Heap = Set Addr
data Reaction = Lambda (Int -> Int) | Default deriving (Show)
type Queue = [(Promise, Reaction, Addr)]
data State = State
  { heap :: Heap
  , promiseMap :: Map Addr Promise
  , fulfillReactions :: Map Addr [(Reaction, Addr)]
  , rejectReactions :: Map Addr [(Reaction, Addr)]
  , queue :: Queue
  }

instance Show State where
  show :: State -> String
  show State {..} =
    "State\n  "
      <> "[ heap: "
      <> show heap
      <> "\n  , promises: "
      <> show promiseMap
      <> "\n  , fulfillReactions: "
      <> show fulfillReactions
      <> "\n  , rejectReactions: "
      <> show rejectReactions
      <> "\n  , queue: "
      <> show queue
      <> "\n  ]"

instance Show (Int -> Int) where
  show :: (Int -> Int) -> String
  show _ = "Function"

startState :: State
startState =
  State
    { fulfillReactions = mempty
    , heap = singleton 0
    , promiseMap = mempty
    , queue = mempty
    , rejectReactions = mempty
    }

main :: IO ()
main = print "Hello"
