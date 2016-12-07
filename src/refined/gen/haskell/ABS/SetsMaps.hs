{-# LINE 1 "refined\SetsMaps.abs" #-}
{-# LANGUAGE NoImplicitPrelude, ExistentialQuantification,
  MultiParamTypeClasses, ScopedTypeVariables, FlexibleContexts,
  PartialTypeSignatures, LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC
  -w -Werror -fforce-recomp -fwarn-missing-methods -fno-ignore-asserts#-}
module ABS.SetsMaps (module ABS.SetsMaps) where
import ABS.StdLib
       hiding (Map, map, _emptyMap, put, insert, lookup, lookupMaybe,
               lookupUnsafe, lookupDefault, removeKey, keys, values, Set, set,
               _emptySet, emptySet, size, contains, union, intersection,
               difference, insertElement, remove, take, hasNext, next)
import ABS.Runtime
import Data.Function ((.))
import Control.Applicative ((<*>), (*>))
import Control.Monad ((=<<))
import qualified Control.Applicative as I' (pure)
import qualified Data.IORef as I'
       (newIORef, readIORef, writeIORef, atomicModifyIORef')
import qualified Control.Monad.Trans.Class as I' (lift)
import qualified Control.Monad as I' (when, sequence, join)
import qualified Prelude as I'
       (IO, Eq, Ord(..), Show(..), undefined, error, negate, fromIntegral,
        mapM_)
import qualified Unsafe.Coerce as I' (unsafeCoerce)
import qualified Control.Concurrent as I' (ThreadId)
import qualified Control.Concurrent.MVar as I'
       (isEmptyMVar, readMVar)
import Control.Exception (assert)
import qualified Control.Exception as I'
       (Exception(..), SomeException, throwTo, throw)
import qualified Data.Dynamic as I' (toDyn, fromDynamic)
import qualified Data.Map as I' (lookup)
import qualified Web.Scotty as I' (get, param, json, raise)
import qualified ABS.StdLib as I' (put)

default (Int, Rat)

{-# LINE 15 "refined\SetsMaps.abs" #-}
data Set a = EmptySet
           | Insert !a !(Set a)
           deriving (I'.Eq, I'.Ord, I'.Show)

set :: forall a . _ => List a -> Set a
{-# LINE 21 "refined\SetsMaps.abs" #-}
set l
  = case l of
        [] -> EmptySet
        (x : xs) -> (insertElement (set xs) x)

contains :: forall a . _ => Set a -> a -> Bool
{-# LINE 39 "refined\SetsMaps.abs" #-}
contains ss e
  = case ss of
        EmptySet -> False
        Insert e' _ | e' == e -> True
        Insert x xs -> if (x > e) then False else (contains xs e)

emptySet :: forall a . _ => Set a -> Bool
{-# LINE 59 "refined\SetsMaps.abs" #-}
emptySet xs = (xs == EmptySet)

size :: forall a . _ => Set a -> Int
{-# LINE 69 "refined\SetsMaps.abs" #-}
size xs
  = case xs of
        EmptySet -> 0
        Insert s ss -> (1 + (size ss))

union :: forall a . _ => Set a -> Set a -> Set a
{-# LINE 87 "refined\SetsMaps.abs" #-}
union set1 set2
  = case set1 of
        EmptySet -> set2
        Insert e1 ss1 -> case set2 of
                             EmptySet -> set1
                             Insert e1 ss2 -> (Insert e1 (union ss1 ss2))
                             Insert e2 ss2 -> if (e1 < e2) then (Insert e1 (union ss1 set2))
                                                else (Insert e2 (union set1 ss2))

intersection :: forall a . _ => Set a -> Set a -> Set a
{-# LINE 119 "refined\SetsMaps.abs" #-}
intersection set1 set2
  = case set1 of
        EmptySet -> EmptySet
        Insert e1 ss1 -> case set2 of
                             EmptySet -> EmptySet
                             Insert e1 ss2 -> (Insert e1 (intersection ss1 ss2))
                             Insert e2 ss2 -> if (e1 < e2) then (intersection ss1 set2) else
                                                (intersection set1 ss2)

difference :: forall a . _ => Set a -> Set a -> Set a
{-# LINE 153 "refined\SetsMaps.abs" #-}
difference set1 set2
  = case set1 of
        EmptySet -> EmptySet
        Insert e1 ss1 -> case set2 of
                             EmptySet -> set1
                             Insert e1 ss2 -> (difference ss1 ss2)
                             Insert e2 ss2 -> if (e1 < e2) then
                                                (Insert e1 (difference ss1 set2)) else
                                                (difference set1 ss2)

insertElement :: forall a . _ => Set a -> a -> Set a
{-# LINE 187 "refined\SetsMaps.abs" #-}
insertElement xs e
  = case xs of
        EmptySet -> (Insert e EmptySet)
        Insert e' _ | e' == e -> xs
        Insert x ss -> if (e < x) then (Insert e xs) else
                         (Insert x (insertElement ss e))

remove :: forall a . _ => Set a -> a -> Set a
{-# LINE 207 "refined\SetsMaps.abs" #-}
remove xs e
  = case xs of
        EmptySet -> EmptySet
        Insert e' ss | e' == e -> ss
        Insert x ss -> if (e < x) then xs else (Insert x (remove ss e))

take :: forall a . _ => Set a -> a
{-# LINE 231 "refined\SetsMaps.abs" #-}
take ss
  = case ss of
        Insert e _ -> e

hasNext :: forall a . _ => Set a -> Bool
{-# LINE 243 "refined\SetsMaps.abs" #-}
hasNext s = (not (emptySet s))

next :: forall a . _ => Set a -> Pair (Set a) a
{-# LINE 249 "refined\SetsMaps.abs" #-}
next s
  = case s of
        Insert e set2 -> ((set2, e))

{-# LINE 260 "refined\SetsMaps.abs" #-}
data Map a b = EmptyMap
             | InsertAssoc !(Pair a b) !(Map a b)
             deriving (I'.Eq, I'.Ord, I'.Show)

map :: forall a b . _ => List (Pair a b) -> Map a b
{-# LINE 264 "refined\SetsMaps.abs" #-}
map l
  = case l of
        [] -> EmptyMap
        (hd : tl) -> (InsertAssoc hd (map tl))

removeKey :: forall a b . _ => Map a b -> a -> Map a b
{-# LINE 278 "refined\SetsMaps.abs" #-}
removeKey map key
  = case map of
        EmptyMap -> map
        InsertAssoc (key', _) m | key' == key -> m
        InsertAssoc pair tail -> (InsertAssoc pair (removeKey tail key))

values :: forall a b . _ => Map a b -> List b
{-# LINE 294 "refined\SetsMaps.abs" #-}
values map
  = case map of
        EmptyMap -> []
        InsertAssoc (_, elem) tail -> (elem : (values tail))

keys :: forall a b . _ => Map a b -> Set a
{-# LINE 312 "refined\SetsMaps.abs" #-}
keys map
  = case map of
        EmptyMap -> EmptySet
        InsertAssoc (a, _) tail -> (insertElement (keys tail) a)

lookup :: forall a b . _ => Map a b -> a -> Maybe b
{-# LINE 330 "refined\SetsMaps.abs" #-}
lookup ms k
  = case ms of
        InsertAssoc (k', y) _ | k' == k -> (Just y)
        InsertAssoc _ tm -> (lookup tm k)
        EmptyMap -> Nothing

lookupMaybe :: forall a b . _ => Map a b -> a -> Maybe b
{-# LINE 350 "refined\SetsMaps.abs" #-}
lookupMaybe ms k = (lookup ms k)

lookupUnsafe :: forall a b . _ => Map a b -> a -> b
{-# LINE 362 "refined\SetsMaps.abs" #-}
lookupUnsafe ms k = (fromJust (lookup ms k))

lookupDefault :: forall a b . _ => Map a b -> a -> b -> b
{-# LINE 376 "refined\SetsMaps.abs" #-}
lookupDefault ms k d
  = case ms of
        InsertAssoc (k', y) _ | k' == k -> y
        InsertAssoc _ tm -> (lookupDefault tm k d)
        EmptyMap -> d

insert :: forall a b . _ => Map a b -> Pair a b -> Map a b
{-# LINE 398 "refined\SetsMaps.abs" #-}
insert map p = (InsertAssoc p map)

put :: forall a b . _ => Map a b -> a -> b -> Map a b
{-# LINE 410 "refined\SetsMaps.abs" #-}
put ms k v
  = case ms of
        EmptyMap -> (InsertAssoc ((k, v)) EmptyMap)
        InsertAssoc (k', _) ts | k' == k -> (InsertAssoc ((k, v)) ts)
        InsertAssoc p ts -> (InsertAssoc p (put ts k v))
