{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Rules where

import Data.Map (adjust, keys, lookup)
import qualified Data.Map as Map (insert, member)
import qualified Data.Set as Set (insert, member, notMember, union, fromList)
import Main ( Addr, Promise(..), State(..), Reaction(..) )
import Prelude  hiding (lookup)

-- | [E-PROMISIFY]
promisify :: Addr -> State -> Either String State
promisify addr state@State {heap, promiseMap, fulfillReactions}
  | addr `Set.member` heap && not (addr `Map.member` promiseMap) =
      Right
        state
          { promiseMap = Map.insert addr Pending promiseMap
          , fulfillReactions = Map.insert addr [] fulfillReactions
          , rejectReactions = Map.insert addr [] fulfillReactions
          }
  | otherwise =
      Left ("[E-PROMISIFY]: " <> show addr <> " is not on heap: " <> show heap)

-- | [E-ONRESOLVE-PENDING/FULFILLED]
onResolve :: Addr -> (Int -> Int) -> State -> Either String State
onResolve addr fulfillReaction state@State {..}
  | addr `Set.notMember` heap =
      Left
        ( "[E-ONRESOLVE]: "
            <> show addr
            <> " is not on heap: "
            <> show heap
        )
  -- [E-ONRESOLVE-PENDING]
  | (Just promise) <- addr `lookup` promiseMap = Right $ case promise of
      Pending ->
        state
          { heap = Set.insert addr' heap
          , promiseMap = Map.insert addr' Pending promiseMap
          , fulfillReactions =
              (Map.insert addr' [] . adjust ((Lambda fulfillReaction, addr') :) addr) fulfillReactions
          , rejectReactions = Map.insert addr' [] rejectReactions
          }
      -- [E-ONRESOLVE-FULFILLED]
      (Fulfilled value) ->
        state
          { heap = Set.insert addr' heap
          , promiseMap = Map.insert addr' (Fulfilled value) promiseMap
          , fulfillReactions = Map.insert addr' [] fulfillReactions
          , rejectReactions = Map.insert addr' [] rejectReactions
          , queue = (Fulfilled value, Lambda fulfillReaction, addr') : queue
          }
      -- Do nothing when registering resolve reactions on rejected promises
      (Rejected _) -> state
  | otherwise =
      Left
        ( "[E-ONRESOLVE]: Promise with address "
            <> show addr
            <> " not found in promiseMap: "
            <> show (keys promiseMap)
        )
 where
  addr' = (+ 1) . maximum $ heap

-- |  [E-RESOLVE-/PENDING/SETTLED]
resolve :: Addr -> Int -> State -> Either String State
resolve addr value state@State {promiseMap, heap, fulfillReactions, rejectReactions, queue}
  | addr `Set.notMember` heap =
      Left
        ( "[E-RESOLVE]: "
            <> show addr
            <> " is not on heap: "
            <> show heap
        )
  | (Just promise) <- addr `lookup` promiseMap
  , (Just reactions) <- addr `lookup` fulfillReactions = Right $ case promise of
      -- [E-RESOLVE-PENDING]
      Pending ->
        state
          { promiseMap = Map.insert addr (Fulfilled value) promiseMap
          , fulfillReactions = Map.insert addr [] fulfillReactions
          , rejectReactions = Map.insert addr [] rejectReactions
          , queue =
              queue
                ++ map
                  (\(reaction, dependentPromise) -> (Fulfilled value, reaction, dependentPromise))
                  reactions
          }
      -- [E-RESOLVE-SETTLED]
      (Fulfilled _) -> state
      (Rejected _) -> state
  | otherwise =
      Left
        ( "[E-RESOLVE]: Promise with address "
            <> show addr
            <> " not found in promiseMap: "
            <> show (keys promiseMap)
        )

-- | [E-ONREJECT-PENDING/REJECTED]
onReject :: Addr -> (Int -> Int) -> State -> Either String State
onReject addr rejectReaction state@State {..}
  | addr `Set.notMember` heap =
      Left
        ( "[E-REJECT]: "
            <> show addr
            <> " is not on heap: "
            <> show heap
        )
  -- [E-ONREJECT-PENDING]
  | (Just promise) <- addr `lookup` promiseMap = Right $ case promise of
      Pending ->
        state
          { heap = Set.insert addr' heap
          , promiseMap = Map.insert addr' Pending promiseMap
          , fulfillReactions = Map.insert addr' [] fulfillReactions
          , rejectReactions =
            (Map.insert addr' [] . adjust ((Lambda rejectReaction, addr') :) addr) rejectReactions
          }
      -- [E-ONREJECT-REJECTED]
      (Rejected value) ->
        state
          { heap = Set.insert addr' heap
          , promiseMap = Map.insert addr' (Fulfilled value) promiseMap
          , fulfillReactions = Map.insert addr' [] fulfillReactions
          , rejectReactions = Map.insert addr' [] rejectReactions
          , queue = (Rejected value, Lambda rejectReaction, addr') : queue
          }
      -- Do nothing when registering reject reactions on fulfilled promises
      (Fulfilled _) -> state
  | otherwise =
      Left
        ( "[E-ONREJECT]: Promise with address "
            <> show addr
            <> " not found in promiseMap: "
            <> show (keys promiseMap)
        )
 where
  addr' = (+ 1) . maximum $ heap

-- |  [E-REJECT-/PENDING/SETTLED]
reject :: Addr -> Int -> State -> Either String State
reject addr value state@State {promiseMap, heap, fulfillReactions, rejectReactions, queue}
  | addr `Set.notMember` heap =
      Left
        ( "[E-REJECT]: "
            <> show addr
            <> " is not on heap: "
            <> show heap
        )
  | (Just promise) <- addr `lookup` promiseMap
  , (Just reactions) <- addr `lookup` rejectReactions = Right $ case promise of
      -- [E-REJECT-PENDING]
      Pending ->
        state
          { promiseMap = Map.insert addr (Rejected value) promiseMap
          , fulfillReactions = Map.insert addr [] fulfillReactions
          , rejectReactions = Map.insert addr [] rejectReactions
          , queue =
              queue
                ++ map
                  (\(reaction, dependentPromise) -> (Rejected value, reaction, dependentPromise))
                  reactions
          }
      -- [E-REJECT-SETTLED]
      (Fulfilled _) -> state
      (Rejected _) -> state
  | otherwise =
      Left
        ( "[E-REJECT]: Promise with address "
            <> show addr
            <> " not found in promiseMap: "
            <> show (keys promiseMap)
        )

-- | [E-LINK-PENDING/FULFILLED]
link :: Addr -> Addr -> State -> Either String State
link source dest state@State {heap, promiseMap, fulfillReactions, rejectReactions, queue}
  | source `Set.member` heap
  , dest `Set.member` heap
  , Just sourcePromise <- source `lookup` promiseMap = case sourcePromise of
      -- [E-LINK-PENDING]
      Pending ->
        Right
          ( state
              { fulfillReactions = adjust ((Default, dest) :) source fulfillReactions
              , rejectReactions = adjust ((Default, dest) :) source rejectReactions
              }
          )
      -- [E-LINK-FULFILLED]
      p@(Fulfilled _) -> Right (state {queue = (p, Default, dest) : queue})
      -- [E-LINK-REJECTED] is not a rule
      (Rejected _) -> Left "[E-LINK-<REJECTED>]: Cannot link to rejected promise"
  | otherwise = Left ""

-- | [E-LOOP-FULFILLED/REJECTED-LAMBDA/DEFAULT]
loop :: State -> Either String State
loop state@State {queue} = case queue of
  -- No steps to evaluate
  [] -> Right state
  -- [E-LOOP-PENDING] is not a rule
  ((Pending, _, _) : _) -> Left ("[E-LOOP-<PENDING>]: Pending promise in queue: " <> show queue)
  -- [E-LOOP-FULFILLED-LAMBDA]
  ((Fulfilled value, Lambda reaction, promise) : queue') -> resolve promise (reaction value) (state {queue = queue'})
  -- [E-LOOP-REJECTED-LAMBDA]
  ((Rejected err, Lambda reaction, promise) : queue') -> resolve promise (reaction err) (state {queue = queue'})
  -- [E-LOOP-FULFILLED-DEFAULT]
  ((Fulfilled value, Default, promise) : queue') -> resolve promise value (state {queue = queue'})
  -- [E-LOOP-REJECTED-DEFAULT]
  ((Rejected err, Default, promise) : queue') -> reject promise err (state {queue = queue'})

eval :: State -> Either String State
eval state@State {queue}
  | null queue = Right state
  | otherwise = loop state >>= eval

alloc :: Integer -> State -> State
alloc value state@State {heap} =
  state
    { heap = Set.union (Set.fromList [1..maximum heap + value]) heap
    }
