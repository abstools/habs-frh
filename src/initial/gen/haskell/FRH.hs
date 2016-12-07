{-# LINE 1 "initial\FRH.abs" #-}
{-# LANGUAGE NoImplicitPrelude, ExistentialQuantification,
  MultiParamTypeClasses, ScopedTypeVariables, FlexibleContexts,
  PartialTypeSignatures, LambdaCase, OverloadedStrings, FlexibleInstances #-}
{-# OPTIONS_GHC
  -w -Werror -fforce-recomp -fwarn-missing-methods -fno-ignore-asserts#-}
module FRH (main) where
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
{-# LINE 3 "initial\FRH.abs" #-}
import ABS.SetsMaps hiding (main)
{-# LINE 4 "initial\FRH.abs" #-}
import ABS.DC hiding (main)
import qualified Data.Functor as I' (fmap)
import qualified Data.Aeson as J (ToJSON (..), Value(..))
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import System.Clock (toNanoSecs)
import Prelude (quot)

instance {-# OVERLAPS #-} J.ToJSON (List (Pair Time (List (Pair String Rat)))) where
  toJSON l = J.Object (H.singleton "result" (J.String (show' l)))

class Show' a where
  show' :: a -> T.Text

instance Show' a => Show' (List a) where
  show' [] = "list[]"
  show' ls = "list[" `T.append` (T.intercalate "," (I'.fmap show' ls)) `T.append` "]"

instance (Show' a, Show' b) => Show' (Pair a b) where
  show' (a,b) = "Pair(" `T.append` show' a `T.append` "," `T.append` show' b `T.append` ")"

instance Show' Rat where
  show' r = T.pack (I'.show (numerator r)) `T.append` "/" `T.append` T.pack (I'.show (denominator r))

instance {-# OVERLAPS #-} Show' String where
  show' s = "\"" `T.append` T.pack s `T.append` "\""

instance Show' Time where
  show' t = "Time(" `T.append` T.pack ( I'.show (toNanoSecs t `quot` 1000000)) `T.append` ")" -- ms

default (Int, Rat)

{-# LINE 9 "initial\FRH.abs" #-}
type Customer = String

{-# LINE 10 "initial\FRH.abs" #-}
data Degradation_States = S
                        deriving (I'.Eq, I'.Ord, I'.Show)

{-# LINE 11 "initial\FRH.abs" #-}
type Id = Int

{-# LINE 12 "initial\FRH.abs" #-}
type Request = Int

{-# LINE 13 "initial\FRH.abs" #-}
type Response = Bool

{-# LINE 14 "initial\FRH.abs" #-}
data ServiceType = FAS
                 | SUGGEST
                 | DM
                 deriving (I'.Eq, I'.Ord, I'.Show)

{-# LINE 15 "initial\FRH.abs" #-}
data Config = Config_ !ServiceType !(List ResourceCapacities)
            deriving (I'.Eq, I'.Show)
instances_ (Config_ _ a) = a
instances_ _
  = I'.throw
      (RecSelError
         (concatenate "Data constructor does not have accessor "
            "instances_"))
serviceType (Config_ a _) = a
serviceType _
  = I'.throw
      (RecSelError
         (concatenate "Data constructor does not have accessor "
            "serviceType"))

{-# LINE 16 "initial\FRH.abs" #-}
data State = RUNNING
           | STOP
           deriving (I'.Eq, I'.Ord, I'.Show)

{-# LINE 17 "initial\FRH.abs" #-}
data VMType = T2_MICRO
            | T2_SMALL
            | T2_MEDIUM
            | M4_LARGE
            | M4_XLARGE
            | M4_2XLARGE
            | M4_10XLARGE
            | M3_MEDIUM
            | M3_LARGE
            | M3_XLARGE
            | M3_2XLARGE
            | C4_LARGE
            | C4_XLARGE
            | C4_2XLARGE
            | C4_4XLARGE
            | C4_8XLARGE
            deriving (I'.Eq, I'.Ord, I'.Show)

{-# LINE 18 "initial\FRH.abs" #-}
type ResourceCapacities = Map Resourcetype Rat

{-# LINE 19 "initial\FRH.abs" #-}
data LBOp = INCR
          | DECR
          deriving (I'.Eq, I'.Ord, I'.Show)

{-# LINE 20 "initial\FRH.abs" #-}
data Rule = Rule_ !Int !Monitor
          deriving (I'.Eq, I'.Show)
monitor_ (Rule_ _ a) = a
monitor_ _
  = I'.throw
      (RecSelError
         (concatenate "Data constructor does not have accessor "
            "monitor_"))
interval (Rule_ a _) = a
interval _
  = I'.throw
      (RecSelError
         (concatenate "Data constructor does not have accessor "
            "interval"))

{-# LINE 21 "initial\FRH.abs" #-}
data Scale = UP
           | DOWN
           deriving (I'.Eq, I'.Ord, I'.Show)

{-# LINE 22 "initial\FRH.abs" #-}
type CustomerConfig = Pair Customer (List (Pair Config Int))

{-# LINE 24 "initial\FRH.abs" #-}
data DeployParamSpecification = Req
                              | List !Int
                              | Default !String
                              | User_
                              | OptList !String
                              deriving (I'.Eq, I'.Ord, I'.Show)

{-# LINE 25 "initial\FRH.abs" #-}
data DeployScenarioElement = MaxUse !Int
                           | Cost !String !Int
                           | Param !String !DeployParamSpecification
                           | Name !String
                           deriving (I'.Eq, I'.Show)

{-# LINE 26 "initial\FRH.abs" #-}
type Deploy = List DeployScenarioElement

{-# LINE 27 "initial\FRH.abs" #-}
type SmartDeploy = String

{-# LINE 28 "initial\FRH.abs" #-}
type SmartDeployCloudProvider = String

init :: _ => Id
{-# LINE 34 "initial\FRH.abs" #-}
init = 1

incr :: _ => Id -> Id
{-# LINE 36 "initial\FRH.abs" #-}
incr id = (id + 1)

cost :: _ => Request -> Int
{-# LINE 38 "initial\FRH.abs" #-}
cost request = request

success :: _ => Response
{-# LINE 40 "initial\FRH.abs" #-}
success = True

isSuccess :: _ => Response -> Bool
{-# LINE 42 "initial\FRH.abs" #-}
isSuccess response = response

vmTypesCollection :: _ => Set VMType
{-# LINE 44 "initial\FRH.abs" #-}
vmTypesCollection
  = (set
       (T2_MICRO :
          (T2_SMALL :
             (T2_MEDIUM :
                (M4_LARGE :
                   (M4_XLARGE :
                      (M4_2XLARGE :
                         (M4_10XLARGE :
                            (M3_MEDIUM :
                               (M3_LARGE :
                                  (M3_XLARGE :
                                     (M3_2XLARGE :
                                        (C4_LARGE :
                                           (C4_XLARGE :
                                              (C4_2XLARGE :
                                                 (C4_4XLARGE : (C4_8XLARGE : [])))))))))))))))))

vmResources :: _ => VMType -> ResourceCapacities
{-# LINE 48 "initial\FRH.abs" #-}
vmResources v
  = case v of
        T2_MICRO -> (map (((Memory, 1)) : (((Speed, 1)) : [])))
        T2_SMALL -> (map (((Memory, 2)) : (((Speed, 1)) : [])))
        T2_MEDIUM -> (map (((Memory, 4)) : (((Speed, 2)) : [])))
        M4_LARGE -> (map (((Memory, 8)) : (((Speed, 2)) : [])))
        M4_XLARGE -> (map (((Memory, 16)) : (((Speed, 4)) : [])))
        M4_2XLARGE -> (map (((Memory, 32)) : (((Speed, 8)) : [])))
        M4_10XLARGE -> (map (((Memory, 160)) : (((Speed, 40)) : [])))
        M3_MEDIUM -> (map
                        (((Memory, (3750 / 1000))) : (((Speed, 1)) : [])))
        M3_LARGE -> (map (((Memory, (7500 / 1000))) : (((Speed, 2)) : [])))
        M3_XLARGE -> (map (((Memory, 15)) : (((Speed, 4)) : [])))
        M3_2XLARGE -> (map (((Memory, 30)) : (((Speed, 8)) : [])))
        C4_LARGE -> (map (((Memory, (3750 / 1000))) : (((Speed, 2)) : [])))
        C4_XLARGE -> (map
                        (((Memory, (7500 / 1000))) : (((Speed, 4)) : [])))
        C4_2XLARGE -> (map (((Memory, 15)) : (((Speed, 8)) : [])))
        C4_4XLARGE -> (map (((Memory, 30)) : (((Speed, 16)) : [])))
        C4_8XLARGE -> (map (((Memory, 60)) : (((Speed, 36)) : [])))

amazonInstances :: _ => Map String ResourceCapacities
{-# LINE 68 "initial\FRH.abs" #-}
amazonInstances
  = (map
       ((("T2_MICRO", (map (((Memory, 1)) : (((Speed, 1)) : []))))) :
          ((("T2_SMALL", (map (((Memory, 2)) : (((Speed, 1)) : []))))) :
             ((("T2_MEDIUM", (map (((Memory, 4)) : (((Speed, 2)) : []))))) :
                ((("M4_LARGE", (map (((Memory, 8)) : (((Speed, 2)) : []))))) :
                   ((("M4_XLARGE", (map (((Memory, 16)) : (((Speed, 4)) : []))))) :
                      ((("M4_2XLARGE", (map (((Memory, 32)) : (((Speed, 8)) : []))))) :
                         ((("M4_10XLARGE", (map (((Memory, 160)) : (((Speed, 40)) : [])))))
                            :
                            ((("M3_MEDIUM",
                               (map (((Memory, (3750 / 1000))) : (((Speed, 1)) : [])))))
                               :
                               ((("M3_LARGE",
                                  (map (((Memory, (7500 / 1000))) : (((Speed, 2)) : [])))))
                                  :
                                  ((("M3_XLARGE", (map (((Memory, 15)) : (((Speed, 4)) : []))))) :
                                     ((("M3_2XLARGE", (map (((Memory, 30)) : (((Speed, 8)) : [])))))
                                        :
                                        ((("C4_LARGE",
                                           (map (((Memory, (3750 / 1000))) : (((Speed, 2)) : [])))))
                                           :
                                           ((("C4_XLARGE",
                                              (map
                                                 (((Memory, (7500 / 1000))) :
                                                    (((Speed, 4)) : [])))))
                                              :
                                              ((("C4_2XLARGE",
                                                 (map (((Memory, 15)) : (((Speed, 8)) : [])))))
                                                 :
                                                 ((("C4_4XLARGE",
                                                    (map (((Memory, 30)) : (((Speed, 16)) : [])))))
                                                    :
                                                    ((("C4_8XLARGE",
                                                       (map
                                                          (((Memory, 60)) : (((Speed, 36)) : [])))))
                                                       : [])))))))))))))))))

mapValue :: forall x a . _ => Map x a -> a -> Maybe x
{-# LINE 75 "initial\FRH.abs" #-}
mapValue ss e
  = case ss of
        EmptyMap -> Nothing
        InsertAssoc p y -> case p of
                               (x, e') | e' == e -> (Just x)
                               _ -> (mapValue y e)

removeFirst :: forall x . _ => List x -> x -> List x
{-# LINE 84 "initial\FRH.abs" #-}
removeFirst list v
  = case list of
        [] -> []
        (v' : vs) | v' == v -> vs
        (x : vs) -> (x : (removeFirst vs v))

inList :: forall x . _ => List x -> x -> Bool
{-# LINE 91 "initial\FRH.abs" #-}
inList list v
  = case list of
        [] -> False
        (v' : vs) | v' == v -> True
        (_ : vs) -> (inList vs v)

unique :: forall x . _ => List x -> Bool
{-# LINE 98 "initial\FRH.abs" #-}
unique list
  = case list of
        [] -> True
        (v : vs) -> if (inList vs v) then False else (unique vs)

add :: forall x . _ => List x -> x -> List x
{-# LINE 104 "initial\FRH.abs" #-}
add list v = if (inList list v) then list else (v : list)

toList :: forall x . _ => Set x -> List x
{-# LINE 106 "initial\FRH.abs" #-}
toList s
  = case s of
        EmptySet -> []
        Insert x xs -> (x : (toList xs))

lookupService ::
                _ => Map Config Int -> ServiceType -> Maybe (Pair Config Int)
{-# LINE 112 "initial\FRH.abs" #-}
lookupService cs s
  = case cs of
        EmptyMap -> Nothing
        InsertAssoc (Config_ s' ls, e) _ | s' == s ->
                                           (Just (((Config_ s ls), e)))
        InsertAssoc _ ms -> (lookupService ms s)

lookupCustomerService ::
                        _ =>
                        Map Customer (Map Config Int) ->
                          Customer -> ServiceType -> Maybe (Pair Config Int)
{-# LINE 119 "initial\FRH.abs" #-}
lookupCustomerService cs c s
  = case (lookupDefault cs c EmptyMap) of
        EmptyMap -> Nothing
        map -> (lookupService map s)

removeLocalEndPoint :: _ => Map Config Int -> Int -> Map Config Int
{-# LINE 125 "initial\FRH.abs" #-}
removeLocalEndPoint cs e
  = case cs of
        EmptyMap -> EmptyMap
        InsertAssoc (_, e') cs' | e' == e, cs' == cs -> cs
        InsertAssoc kv ms -> (InsertAssoc kv
                                (removeLocalEndPoint ms (I'.fromIntegral e)))

removeGlobalEndPoint ::
                       _ =>
                       Map Customer (Map Config Int) ->
                         Int -> Map Customer (Map Config Int)
{-# LINE 132 "initial\FRH.abs" #-}
removeGlobalEndPoint cs e
  = case cs of
        EmptyMap -> EmptyMap
        InsertAssoc (c, map) cs' | cs' == cs ->
                                   case (removeLocalEndPoint map (I'.fromIntegral e)) of
                                       EmptyMap -> (removeGlobalEndPoint cs (I'.fromIntegral e))
                                       newMap -> (InsertAssoc ((c, newMap))
                                                    (removeGlobalEndPoint cs (I'.fromIntegral e)))

keyPairs ::
         forall x y z . _ => Map x (Map y z) -> z -> Maybe (Pair x y)
{-# LINE 141 "initial\FRH.abs" #-}
keyPairs map z
  = case map of
        EmptyMap -> Nothing
        InsertAssoc (x, cm) ms -> case (mapValue cm z) of
                                      Nothing -> (keyPairs ms z)
                                      Just y -> (Just ((x, y)))

updateConfig ::
               _ =>
               Map Customer (Map Config Int) ->
                 Customer -> Config -> Config -> Map Customer (Map Config Int)
{-# LINE 150 "initial\FRH.abs" #-}
updateConfig cs c o n
  = case (lookup cs c) of
        Nothing -> cs
        Just cm -> case (lookup cm o) of
                       Nothing -> cs
                       Just e -> (put cs c (put (removeKey cm o) n e))

getHyphenPosition :: _ => String -> Int -> List Int
{-# LINE 159 "initial\FRH.abs" #-}
getHyphenPosition s v
  = if (s == "") then [] else
      if ((substr s 0 1) == "-") then
        ((I'.fromIntegral v) :
           (getHyphenPosition (substr s 1 ((strlen s) - 1))
              ((I'.fromIntegral v) + 1)))
        else
        (getHyphenPosition (substr s 1 ((strlen s) - 1))
           ((I'.fromIntegral v) + 1))

getInstanceName :: _ => String -> String
{-# LINE 162 "initial\FRH.abs" #-}
getInstanceName s
  = (\ ls ->
       if ((length ls) > 1) then
         (substr s ((head ls) + 1) (((head (tail ls)) - (head ls)) - 1))
         else s)
      ((getHyphenPosition s 0) :: List Int)

strContains :: _ => String -> String -> Bool
{-# LINE 165 "initial\FRH.abs" #-}
strContains s substring
  = (\ l1 ->
       (\ l2 ->
          if ((I'.fromIntegral l2) > (I'.fromIntegral l1)) then False else
            if ((substr s 0 (I'.fromIntegral l2)) == substring) then True else
              (strContains (substr s 1 ((I'.fromIntegral l1) - 1)) substring))
         ((strlen substring) :: Int))
      ((strlen s) :: Int)

filter_lists_by_substr ::
                       forall a . _ => List a -> List String -> String -> List a
{-# LINE 168 "initial\FRH.abs" #-}
filter_lists_by_substr xs ys substring
  = case ys of
        [] -> []
        (z : zs) -> if (strContains z substring) then
                      ((head xs) : (filter_lists_by_substr (tail xs) zs substring)) else
                      (filter_lists_by_substr (tail xs) zs substring)

fst_list :: forall a b . _ => List (Pair a b) -> List a
{-# LINE 174 "initial\FRH.abs" #-}
fst_list ls
  = case ls of
        [] -> []
        (x : xs) -> ((fst x) : (fst_list xs))

snd_list :: forall a b . _ => List (Pair a b) -> List b
{-# LINE 180 "initial\FRH.abs" #-}
snd_list ls
  = case ls of
        [] -> []
        (x : xs) -> ((snd x) : (snd_list xs))

takeFromSet :: forall a . _ => Set a -> a
{-# LINE 193 "initial\FRH.abs" #-}
takeFromSet ss
  = case ss of
        Insert e _ -> e

inListAll :: forall x . _ => List x -> List x -> Bool
{-# LINE 198 "initial\FRH.abs" #-}
inListAll ls ms
  = case ms of
        [] -> True
        (m : mm) -> (inListAll
                       (if (inList ls m) then (removeFirst ls m) else ls)
                       mm)

removeAllinList :: forall x . _ => List x -> List x -> List x
{-# LINE 204 "initial\FRH.abs" #-}
removeAllinList ls ms
  = case ms of
        [] -> []
        (m : mm) -> (removeAllinList (without ls m) mm)

removeAll :: forall x y . _ => Map x y -> List x -> Map x y
{-# LINE 210 "initial\FRH.abs" #-}
removeAll map xs
  = case xs of
        [] -> map
        (x : nx) -> (removeAll (removeKey map x) nx)

difference_ :: forall x . _ => List x -> List x -> List x
{-# LINE 216 "initial\FRH.abs" #-}
difference_ xs ys
  = case ys of
        [] -> []
        (y : ny) -> (difference_ (without xs y) ny)

lookupMap :: forall x y . _ => Map x y -> List x -> List y
{-# LINE 222 "initial\FRH.abs" #-}
lookupMap map xs
  = case xs of
        [] -> []
        (x : nx) -> case (lookup map x) of
                        Nothing -> (lookupMap map nx)
                        Just v -> (v : (lookupMap map nx))

lookupTwoMaps ::
              forall x y z . _ => Map x (List y) -> Map y z -> x -> List z
{-# LINE 231 "initial\FRH.abs" #-}
lookupTwoMaps m1 m2 x
  = case (lookup m1 x) of
        Nothing -> []
        Just vs -> (lookupMap m2 vs)

lookupAllSecond ::
                forall w x y . _ => Map w (Map x (List y)) -> x -> List y
{-# LINE 238 "initial\FRH.abs" #-}
lookupAllSecond map x
  = case map of
        EmptyMap -> []
        InsertAssoc (w, ns) ms -> (concatenate (lookupDefault ns x [])
                                     (lookupAllSecond ms x))

decr1 :: forall y . _ => Map Int y -> Map Int y
{-# LINE 244 "initial\FRH.abs" #-}
decr1 map
  = case map of
        EmptyMap -> EmptyMap
        InsertAssoc (w, y) ms -> (InsertAssoc (((w - 1), y)) (decr1 ms))

decr :: forall x y . _ => Map x (Map Int y) -> Map x (Map Int y)
{-# LINE 251 "initial\FRH.abs" #-}
decr map
  = case map of
        EmptyMap -> EmptyMap
        InsertAssoc (w, ns) ms -> (InsertAssoc ((w, (decr1 ns))) (decr ms))

reset :: forall x y . _ => Map x (Map x y) -> x -> Map x (Map x y)
{-# LINE 257 "initial\FRH.abs" #-}
reset map x
  = case map of
        EmptyMap -> EmptyMap
        InsertAssoc (k, ns) ms -> case (lookup ns x) of
                                      Nothing -> (InsertAssoc ((k, ns)) (reset ms x))
                                      Just y -> (InsertAssoc ((k, (put (removeKey ns x) k y)))
                                                   (reset ms x))

{-# LINE 270 "initial\FRH.abs" #-}
class Monitor' a where
        {-# LINE 271 "initial\FRH.abs" #-}
        monitor :: Obj' a -> ABS' Unit
        
        {-# LINE 272 "initial\FRH.abs" #-}
        metricHistory ::
                      Obj' a -> ABS' (List (Pair Time (List (Pair String Rat))))

data Monitor = forall a . Monitor' a => Monitor (Obj' a)

instance I'.Show Monitor where
        show _ = "Monitor"

instance I'.Eq Monitor where
        Monitor (Obj' ref1' _ _) == Monitor (Obj' ref2' _ _)
          = ref1' == I'.unsafeCoerce ref2'

instance Monitor' Null' where
        monitor = I'.undefined
        metricHistory = I'.undefined

instance Monitor' a => Sub' (Obj' a) Monitor where
        up' = Monitor

{-# LINE 274 "initial\FRH.abs" #-}
class Monitor' a => DegradationMonitorIf' a where
        {-# LINE 275 "initial\FRH.abs" #-}
        notify_query_Mon :: Time -> String -> Rat -> Obj' a -> ABS' Unit

data DegradationMonitorIf = forall a . DegradationMonitorIf' a =>
                              DegradationMonitorIf (Obj' a)

instance I'.Show DegradationMonitorIf where
        show _ = "DegradationMonitorIf"

instance I'.Eq DegradationMonitorIf where
        DegradationMonitorIf (Obj' ref1' _ _) ==
          DegradationMonitorIf (Obj' ref2' _ _)
          = ref1' == I'.unsafeCoerce ref2'

instance DegradationMonitorIf' Null' where
        notify_query_Mon = I'.undefined

instance DegradationMonitorIf' a => Sub' (Obj' a)
         DegradationMonitorIf where
        up' = DegradationMonitorIf

instance Sub' DegradationMonitorIf Monitor where
        up' (DegradationMonitorIf x') = Monitor x'

{-# LINE 278 "initial\FRH.abs" #-}
data DegradationMonitorImpl = DegradationMonitorImpl{deployer'DegradationMonitorImpl
                                                     :: DeployerIF,
                                                     metric'DegradationMonitorImpl ::
                                                     DegradationMetricIf,
                                                     metricHist'DegradationMonitorImpl ::
                                                     List (Pair Time (List (Pair String Rat)))}

smart'DegradationMonitorImpl ::
                             DeployerIF -> DegradationMonitorImpl
smart'DegradationMonitorImpl deployer'this
  = (\ metricHist'this ->
       (\ metric'this ->
          (DegradationMonitorImpl (up' deployer'this) (up' metric'this)
             metricHist'this))
         ((up' null) :: DegradationMetricIf))
      ([] :: List (Pair Time (List (Pair String Rat))))

init'DegradationMonitorImpl ::
                            Obj' DegradationMonitorImpl -> I'.IO ()
{-# LINE 278 "initial\FRH.abs" #-}
init'DegradationMonitorImpl this@(Obj' this' _ thisDC)
  = do I'.writeIORef this' =<<
         ((\ this'' ->
             (\ v' ->
                this''{metric'DegradationMonitorImpl = DegradationMetricIf v'})
               <$!>
               newlocal' this init'DegradationMetricImpl
                 smart'DegradationMetricImpl)
            =<< I'.readIORef this')

instance DegradationMonitorIf' DegradationMonitorImpl where
        notify_query_Mon t customer procTime this@(Obj' this' _ thisDC)
          = do measurement :: IORef' (List (Pair String Rat)) <- (\ this'' ->
                                                                    (I'.lift . I'.newIORef) =<<
                                                                      ((\ (DegradationMetricIf obj')
                                                                          ->
                                                                          sync' this obj'
                                                                            (notify_query_Met t
                                                                               customer
                                                                               procTime))
                                                                         (metric'DegradationMonitorImpl
                                                                            this'')))
                                                                   =<< I'.lift (I'.readIORef this')
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{metricHist'DegradationMonitorImpl = v'}) <$!>
                          ((:) <$!> ((,) <$!> I'.pure t <*> I'.readIORef measurement) <*>
                             I'.pure (metricHist'DegradationMonitorImpl this'')))
                       =<< I'.readIORef this'))

instance Monitor' DegradationMonitorImpl where
        monitor this@(Obj' this' _ thisDC) = I'.pure ()
        metricHistory this@(Obj' this' _ thisDC)
          = do copy ::
                 IORef' (List (Pair Time (List (Pair String Rat)))) <- I'.lift
                                                                         ((\ this'' ->
                                                                             I'.newIORef
                                                                               (metricHist'DegradationMonitorImpl
                                                                                  this''))
                                                                            =<< I'.readIORef this')
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' -> this''{metricHist'DegradationMonitorImpl = []}) <$!>
                       I'.readIORef this'))
               I'.lift (I'.readIORef copy)

{-# LINE 317 "initial\FRH.abs" #-}
class DegradationMetricIf' a where
        {-# LINE 318 "initial\FRH.abs" #-}
        notify_query_Met ::
                         Time -> String -> Rat -> Obj' a -> ABS' (List (Pair String Rat))

data DegradationMetricIf = forall a . DegradationMetricIf' a =>
                             DegradationMetricIf (Obj' a)

instance I'.Show DegradationMetricIf where
        show _ = "DegradationMetricIf"

instance I'.Eq DegradationMetricIf where
        DegradationMetricIf (Obj' ref1' _ _) ==
          DegradationMetricIf (Obj' ref2' _ _)
          = ref1' == I'.unsafeCoerce ref2'

instance DegradationMetricIf' Null' where
        notify_query_Met = I'.undefined

instance DegradationMetricIf' a => Sub' (Obj' a)
         DegradationMetricIf where
        up' = DegradationMetricIf

{-# LINE 321 "initial\FRH.abs" #-}
data DegradationMetricImpl = DegradationMetricImpl{cnt'DegradationMetricImpl
                                                   :: Map Customer Int,
                                                   curState'DegradationMetricImpl ::
                                                   Degradation_States,
                                                   degradationSLA'DegradationMetricImpl ::
                                                   Map String Rat,
                                                   slow200Cnt'DegradationMetricImpl ::
                                                   Map Customer Int,
                                                   slow500Cnt'DegradationMetricImpl ::
                                                   Map Customer Int,
                                                   slowQpct'DegradationMetricImpl ::
                                                   List (Pair String Rat)}

smart'DegradationMetricImpl :: DegradationMetricImpl
smart'DegradationMetricImpl
  = (\ slowQpct'this ->
       (\ degradationSLA'this ->
          (\ cnt'this ->
             (\ slow200Cnt'this ->
                (\ slow500Cnt'this ->
                   (\ curState'this ->
                      (DegradationMetricImpl cnt'this curState'this degradationSLA'this
                         slow200Cnt'this
                         slow500Cnt'this
                         slowQpct'this))
                     (S :: Degradation_States))
                  (EmptyMap :: Map Customer Int))
               (EmptyMap :: Map Customer Int))
            (EmptyMap :: Map Customer Int))
         ((map
             ((("asos.fas.live.200ms", (5 / 1000))) :
                ((("asos.fas.live.500ms", (1 / 1000))) :
                   ((("gamma.fas.live.500ms", (5 / 100))) :
                      ((("gamma.fas.live.500ms", (1 / 100))) : [])))))
            :: Map String Rat))
      ([] :: List (Pair String Rat))

init'DegradationMetricImpl ::
                           Obj' DegradationMetricImpl -> I'.IO ()
{-# LINE 321 "initial\FRH.abs" #-}
init'DegradationMetricImpl this@(Obj' this' _ thisDC) = I'.pure ()

instance DegradationMetricIf' DegradationMetricImpl where
        notify_query_Met t customer procTime this@(Obj' this' _ thisDC)
          = do (\ this'' ->
                  if ((curState'DegradationMetricImpl this'') == S) then
                    do I'.lift
                         (I'.writeIORef this' =<<
                            ((\ this'' -> this''{curState'DegradationMetricImpl = S}) <$!>
                               I'.readIORef this'))
                       do old200 :: IORef' Int <- I'.lift (I'.newIORef 0)
                          old500 :: IORef' Int <- I'.lift (I'.newIORef 0)
                          custCnt :: IORef' Int <- I'.lift (I'.newIORef 0)
                          mold200 :: IORef' (Maybe Int) <- I'.lift
                                                             ((\ this'' ->
                                                                 I'.newIORef
                                                                   (lookup
                                                                      (slow200Cnt'DegradationMetricImpl
                                                                         this'')
                                                                      customer))
                                                                =<< I'.readIORef this')
                          mold500 :: IORef' (Maybe Int) <- I'.lift
                                                             ((\ this'' ->
                                                                 I'.newIORef
                                                                   (lookup
                                                                      (slow500Cnt'DegradationMetricImpl
                                                                         this'')
                                                                      customer))
                                                                =<< I'.readIORef this')
                          mcustCnt :: IORef' (Maybe Int) <- I'.lift
                                                              ((\ this'' ->
                                                                  I'.newIORef
                                                                    (lookup
                                                                       (cnt'DegradationMetricImpl
                                                                          this'')
                                                                       customer))
                                                                 =<< I'.readIORef this')
                          case' <- I'.lift (I'.readIORef mold200)
                          case case' of
                              Nothing -> I'.pure ()
                              Just val -> do I'.lift (I'.writeIORef old200 val)
                          case' <- I'.lift (I'.readIORef mold500)
                          case case' of
                              Nothing -> I'.pure ()
                              Just val -> do I'.lift (I'.writeIORef old500 val)
                          case' <- I'.lift (I'.readIORef mcustCnt)
                          case case' of
                              Nothing -> I'.pure ()
                              Just val -> do I'.lift (I'.writeIORef custCnt val)
                          new200 :: IORef' Int <- I'.lift
                                                    (I'.newIORef =<<
                                                       ((+) <$!>
                                                          (I'.fromIntegral <$!> I'.readIORef old200)
                                                          <*>
                                                          do of' <- ((>) <$!> I'.pure procTime <*>
                                                                       I'.pure 20)
                                                             case of' of
                                                                 True -> I'.pure 1
                                                                 False -> I'.pure 0))
                          new500 :: IORef' Int <- I'.lift
                                                    (I'.newIORef =<<
                                                       ((+) <$!>
                                                          (I'.fromIntegral <$!> I'.readIORef old500)
                                                          <*>
                                                          do of' <- ((>) <$!> I'.pure procTime <*>
                                                                       I'.pure 50)
                                                             case of' of
                                                                 True -> I'.pure 1
                                                                 False -> I'.pure 0))
                          I'.lift
                            (I'.writeIORef this' =<<
                               ((\ this'' ->
                                   (\ v' -> this''{slow200Cnt'DegradationMetricImpl = v'}) <$!>
                                     (I'.pure put <*>
                                        I'.pure (slow200Cnt'DegradationMetricImpl this'')
                                        <*> I'.pure customer
                                        <*> (I'.fromIntegral <$!> I'.readIORef new200)))
                                  =<< I'.readIORef this'))
                          I'.lift
                            (I'.writeIORef this' =<<
                               ((\ this'' ->
                                   (\ v' -> this''{slow500Cnt'DegradationMetricImpl = v'}) <$!>
                                     (I'.pure put <*>
                                        I'.pure (slow500Cnt'DegradationMetricImpl this'')
                                        <*> I'.pure customer
                                        <*> (I'.fromIntegral <$!> I'.readIORef new500)))
                                  =<< I'.readIORef this'))
                          I'.lift
                            (I'.writeIORef custCnt =<<
                               ((+) <$!> (I'.fromIntegral <$!> I'.readIORef custCnt) <*>
                                  I'.pure 1))
                          I'.lift
                            (I'.writeIORef this' =<<
                               ((\ this'' ->
                                   (\ v' -> this''{cnt'DegradationMetricImpl = v'}) <$!>
                                     (I'.pure put <*> I'.pure (cnt'DegradationMetricImpl this'') <*>
                                        I'.pure customer
                                        <*> (I'.fromIntegral <$!> I'.readIORef custCnt)))
                                  =<< I'.readIORef this'))
                          twoPct :: IORef' Int <- I'.lift
                                                    (I'.newIORef =<<
                                                       (I'.pure truncate <*>
                                                          ((/) <$!>
                                                             ((*) <$!> I'.pure 2 <*>
                                                                (I'.fromIntegral <$!>
                                                                   I'.readIORef custCnt))
                                                             <*> I'.pure 100)))
                          I'.lift
                            (I'.writeIORef this' =<<
                               ((\ this'' ->
                                   (\ v' -> this''{slowQpct'DegradationMetricImpl = v'}) <$!>
                                     (I'.pure list <*>
                                        ((:) <$!>
                                           ((,) <$!>
                                              ((+) <$!> I'.pure customer <*>
                                                 I'.pure ".fas.live.200ms")
                                              <*>
                                              ((/) <$!>
                                                 ((*) <$!> I'.pure 100 <*>
                                                    (I'.pure max <*> I'.pure 0 <*>
                                                       ((-) <$!>
                                                          (I'.fromIntegral <$!> I'.readIORef new200)
                                                          <*>
                                                          (I'.fromIntegral <$!>
                                                             I'.readIORef twoPct))))
                                                 <*> (I'.fromIntegral <$!> I'.readIORef custCnt)))
                                           <*>
                                           ((:) <$!>
                                              ((,) <$!>
                                                 ((+) <$!> I'.pure customer <*>
                                                    I'.pure ".fas.live.500ms")
                                                 <*>
                                                 ((/) <$!>
                                                    ((*) <$!> I'.pure 100 <*>
                                                       (I'.pure max <*> I'.pure 0 <*>
                                                          ((-) <$!>
                                                             (I'.fromIntegral <$!>
                                                                I'.readIORef new500)
                                                             <*>
                                                             (I'.fromIntegral <$!>
                                                                I'.readIORef twoPct))))
                                                    <*>
                                                    (I'.fromIntegral <$!> I'.readIORef custCnt)))
                                              <*> I'.pure []))))
                                  =<< I'.readIORef this'))
                    else do I'.lift (assert False (I'.pure ())))
                 =<< I'.lift (I'.readIORef this')
               I'.lift
                 ((\ this'' -> I'.pure (slowQpct'DegradationMetricImpl this'')) =<<
                    I'.readIORef this')

{-# LINE 388 "initial\FRH.abs" #-}
class SmartDeployInterface' a where
        {-# LINE 389 "initial\FRH.abs" #-}
        getEndPoint ::
                    Obj' a -> ABS' (List (Pair EndPoint DeploymentComponent))
        
        {-# LINE 390 "initial\FRH.abs" #-}
        getIQueryService ::
                         Obj' a -> ABS' (List (Pair IQueryService DeploymentComponent))
        
        {-# LINE 391 "initial\FRH.abs" #-}
        getService ::
                   Obj' a -> ABS' (List (Pair Service DeploymentComponent))
        
        {-# LINE 392 "initial\FRH.abs" #-}
        getServiceProvider ::
                           Obj' a -> ABS' (List (Pair ServiceProvider DeploymentComponent))
        
        {-# LINE 393 "initial\FRH.abs" #-}
        getDeploymentAgent ::
                           Obj' a -> ABS' (List (Pair DeploymentAgent DeploymentComponent))
        
        {-# LINE 394 "initial\FRH.abs" #-}
        getLoadBalancerService ::
                               Obj' a ->
                                 ABS' (List (Pair LoadBalancerService DeploymentComponent))
        
        {-# LINE 395 "initial\FRH.abs" #-}
        getLoadBalancerEndPoint ::
                                Obj' a ->
                                  ABS' (List (Pair LoadBalancerEndPoint DeploymentComponent))
        
        {-# LINE 396 "initial\FRH.abs" #-}
        getDeploymentService ::
                             Obj' a -> ABS' (List (Pair DeploymentService DeploymentComponent))
        
        {-# LINE 397 "initial\FRH.abs" #-}
        getPlatformService ::
                           Obj' a -> ABS' (List (Pair PlatformService DeploymentComponent))
        
        {-# LINE 398 "initial\FRH.abs" #-}
        getMonitorPlatformService ::
                                  Obj' a ->
                                    ABS' (List (Pair MonitorPlatformService DeploymentComponent))
        
        {-# LINE 399 "initial\FRH.abs" #-}
        getDeploymentComponent :: Obj' a -> ABS' (List DeploymentComponent)
        
        {-# LINE 400 "initial\FRH.abs" #-}
        deploy :: Obj' a -> ABS' Unit
        
        {-# LINE 401 "initial\FRH.abs" #-}
        undeploy :: Obj' a -> ABS' Unit

data SmartDeployInterface = forall a . SmartDeployInterface' a =>
                              SmartDeployInterface (Obj' a)

instance I'.Show SmartDeployInterface where
        show _ = "SmartDeployInterface"

instance I'.Eq SmartDeployInterface where
        SmartDeployInterface (Obj' ref1' _ _) ==
          SmartDeployInterface (Obj' ref2' _ _)
          = ref1' == I'.unsafeCoerce ref2'

instance SmartDeployInterface' Null' where
        getEndPoint = I'.undefined
        getIQueryService = I'.undefined
        getService = I'.undefined
        getServiceProvider = I'.undefined
        getDeploymentAgent = I'.undefined
        getLoadBalancerService = I'.undefined
        getLoadBalancerEndPoint = I'.undefined
        getDeploymentService = I'.undefined
        getPlatformService = I'.undefined
        getMonitorPlatformService = I'.undefined
        getDeploymentComponent = I'.undefined
        deploy = I'.undefined
        undeploy = I'.undefined

instance SmartDeployInterface' a => Sub' (Obj' a)
         SmartDeployInterface where
        up' = SmartDeployInterface

{-# LINE 403 "initial\FRH.abs" #-}
data AddQueryDeployer = AddQueryDeployer{cloudProvider'AddQueryDeployer
                                         :: CloudProvider,
                                         deploymentServiceObjEu'AddQueryDeployer ::
                                         DeploymentService,
                                         deploymentServiceObjUs'AddQueryDeployer ::
                                         DeploymentService,
                                         loadBalancerEndPointObjEu1'AddQueryDeployer ::
                                         LoadBalancerEndPoint,
                                         loadBalancerEndPointObjUs1'AddQueryDeployer ::
                                         LoadBalancerEndPoint,
                                         loadBalancerEndPointObjUs2'AddQueryDeployer ::
                                         LoadBalancerEndPoint,
                                         ls_DeploymentAgent'AddQueryDeployer ::
                                         List (Pair DeploymentAgent DeploymentComponent),
                                         ls_DeploymentComponent'AddQueryDeployer ::
                                         List DeploymentComponent,
                                         ls_DeploymentService'AddQueryDeployer ::
                                         List (Pair DeploymentService DeploymentComponent),
                                         ls_EndPoint'AddQueryDeployer ::
                                         List (Pair EndPoint DeploymentComponent),
                                         ls_IQueryService'AddQueryDeployer ::
                                         List (Pair IQueryService DeploymentComponent),
                                         ls_LoadBalancerEndPoint'AddQueryDeployer ::
                                         List (Pair LoadBalancerEndPoint DeploymentComponent),
                                         ls_LoadBalancerService'AddQueryDeployer ::
                                         List (Pair LoadBalancerService DeploymentComponent),
                                         ls_MonitorPlatformService'AddQueryDeployer ::
                                         List (Pair MonitorPlatformService DeploymentComponent),
                                         ls_PlatformService'AddQueryDeployer ::
                                         List (Pair PlatformService DeploymentComponent),
                                         ls_Service'AddQueryDeployer ::
                                         List (Pair Service DeploymentComponent),
                                         ls_ServiceProvider'AddQueryDeployer ::
                                         List (Pair ServiceProvider DeploymentComponent),
                                         platformServiceObjEu'AddQueryDeployer ::
                                         MonitorPlatformService,
                                         platformServiceObjUs'AddQueryDeployer ::
                                         MonitorPlatformService}

smart'AddQueryDeployer ::
                       CloudProvider ->
                         MonitorPlatformService ->
                           MonitorPlatformService ->
                             DeploymentService ->
                               DeploymentService ->
                                 LoadBalancerEndPoint ->
                                   LoadBalancerEndPoint -> LoadBalancerEndPoint -> AddQueryDeployer
smart'AddQueryDeployer cloudProvider'this platformServiceObjEu'this
  platformServiceObjUs'this deploymentServiceObjEu'this
  deploymentServiceObjUs'this loadBalancerEndPointObjEu1'this
  loadBalancerEndPointObjUs1'this loadBalancerEndPointObjUs2'this
  = (\ ls_EndPoint'this ->
       (\ ls_IQueryService'this ->
          (\ ls_Service'this ->
             (\ ls_ServiceProvider'this ->
                (\ ls_DeploymentAgent'this ->
                   (\ ls_LoadBalancerService'this ->
                      (\ ls_LoadBalancerEndPoint'this ->
                         (\ ls_DeploymentService'this ->
                            (\ ls_PlatformService'this ->
                               (\ ls_MonitorPlatformService'this ->
                                  (\ ls_DeploymentComponent'this ->
                                     (AddQueryDeployer cloudProvider'this
                                        (up' deploymentServiceObjEu'this)
                                        (up' deploymentServiceObjUs'this)
                                        (up' loadBalancerEndPointObjEu1'this)
                                        (up' loadBalancerEndPointObjUs1'this)
                                        (up' loadBalancerEndPointObjUs2'this)
                                        ls_DeploymentAgent'this
                                        ls_DeploymentComponent'this
                                        ls_DeploymentService'this
                                        ls_EndPoint'this
                                        ls_IQueryService'this
                                        ls_LoadBalancerEndPoint'this
                                        ls_LoadBalancerService'this
                                        ls_MonitorPlatformService'this
                                        ls_PlatformService'this
                                        ls_Service'this
                                        ls_ServiceProvider'this
                                        (up' platformServiceObjEu'this)
                                        (up' platformServiceObjUs'this)))
                                    ([] :: List DeploymentComponent))
                                 ([] :: List (Pair MonitorPlatformService DeploymentComponent)))
                              ([] :: List (Pair PlatformService DeploymentComponent)))
                           ([] :: List (Pair DeploymentService DeploymentComponent)))
                        ([] :: List (Pair LoadBalancerEndPoint DeploymentComponent)))
                     ([] :: List (Pair LoadBalancerService DeploymentComponent)))
                  ([] :: List (Pair DeploymentAgent DeploymentComponent)))
               ([] :: List (Pair ServiceProvider DeploymentComponent)))
            ([] :: List (Pair Service DeploymentComponent)))
         ([] :: List (Pair IQueryService DeploymentComponent)))
      ([] :: List (Pair EndPoint DeploymentComponent))

init'AddQueryDeployer :: Obj' AddQueryDeployer -> I'.IO ()
{-# LINE 403 "initial\FRH.abs" #-}
init'AddQueryDeployer this@(Obj' this' _ thisDC) = I'.pure ()

instance SmartDeployInterface' AddQueryDeployer where
        getEndPoint this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' -> I'.pure (ls_EndPoint'AddQueryDeployer this'')) =<<
                    I'.readIORef this')
        getIQueryService this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' -> I'.pure (ls_IQueryService'AddQueryDeployer this''))
                    =<< I'.readIORef this')
        getService this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' -> I'.pure (ls_Service'AddQueryDeployer this'')) =<<
                    I'.readIORef this')
        getServiceProvider this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' -> I'.pure (ls_ServiceProvider'AddQueryDeployer this''))
                    =<< I'.readIORef this')
        getDeploymentAgent this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' -> I'.pure (ls_DeploymentAgent'AddQueryDeployer this''))
                    =<< I'.readIORef this')
        getLoadBalancerService this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' ->
                     I'.pure (ls_LoadBalancerService'AddQueryDeployer this''))
                    =<< I'.readIORef this')
        getLoadBalancerEndPoint this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' ->
                     I'.pure (ls_LoadBalancerEndPoint'AddQueryDeployer this''))
                    =<< I'.readIORef this')
        getDeploymentService this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' ->
                     I'.pure (ls_DeploymentService'AddQueryDeployer this''))
                    =<< I'.readIORef this')
        getPlatformService this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' -> I'.pure (ls_PlatformService'AddQueryDeployer this''))
                    =<< I'.readIORef this')
        getMonitorPlatformService this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' ->
                     I'.pure (ls_MonitorPlatformService'AddQueryDeployer this''))
                    =<< I'.readIORef this')
        getDeploymentComponent this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' ->
                     I'.pure (ls_DeploymentComponent'AddQueryDeployer this''))
                    =<< I'.readIORef this')
        deploy this@(Obj' this' _ thisDC)
          = do m4_large_us2_0 :: IORef' DeploymentComponent <- (\ this'' ->
                                                                  (I'.lift . I'.newIORef) =<<
                                                                    ((\ (CloudProvider obj') ->
                                                                        sync' this obj'
                                                                          (prelaunchInstanceNamed
                                                                             "m4_xlarge_us2"))
                                                                       (cloudProvider'AddQueryDeployer
                                                                          this'')))
                                                                 =<< I'.lift (I'.readIORef this')
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_DeploymentComponent'AddQueryDeployer = v'}) <$!>
                          ((:) <$!> (up' <$!> I'.readIORef m4_large_us2_0) <*>
                             I'.pure (ls_DeploymentComponent'AddQueryDeployer this'')))
                       =<< I'.readIORef this'))
               m4_large_us1_0 :: IORef' DeploymentComponent <- (\ this'' ->
                                                                  (I'.lift . I'.newIORef) =<<
                                                                    ((\ (CloudProvider obj') ->
                                                                        sync' this obj'
                                                                          (prelaunchInstanceNamed
                                                                             "m4_xlarge_us1"))
                                                                       (cloudProvider'AddQueryDeployer
                                                                          this'')))
                                                                 =<< I'.lift (I'.readIORef this')
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_DeploymentComponent'AddQueryDeployer = v'}) <$!>
                          ((:) <$!> (up' <$!> I'.readIORef m4_large_us1_0) <*>
                             I'.pure (ls_DeploymentComponent'AddQueryDeployer this'')))
                       =<< I'.readIORef this'))
               oDef___DeploymentAgentImpl_0_m4_large_us2_0 ::
                 IORef' DeploymentAgent <- I'.lift
                                             ((\ new'DC ->
                                                 (I'.newIORef . DeploymentAgent) =<<
                                                   (new new'DC init'DeploymentAgentImpl =<<
                                                      I'.pure smart'DeploymentAgentImpl))
                                                =<< (up' <$!> I'.readIORef m4_large_us2_0))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_DeploymentAgent'AddQueryDeployer = v'}) <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!> I'.readIORef oDef___DeploymentAgentImpl_0_m4_large_us2_0)
                                <*> (up' <$!> I'.readIORef m4_large_us2_0))
                             <*> I'.pure (ls_DeploymentAgent'AddQueryDeployer this'')))
                       =<< I'.readIORef this'))
               oDef___DeploymentAgentImpl_0_m4_large_us1_0 ::
                 IORef' DeploymentAgent <- I'.lift
                                             ((\ new'DC ->
                                                 (I'.newIORef . DeploymentAgent) =<<
                                                   (new new'DC init'DeploymentAgentImpl =<<
                                                      I'.pure smart'DeploymentAgentImpl))
                                                =<< (up' <$!> I'.readIORef m4_large_us1_0))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_DeploymentAgent'AddQueryDeployer = v'}) <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!> I'.readIORef oDef___DeploymentAgentImpl_0_m4_large_us1_0)
                                <*> (up' <$!> I'.readIORef m4_large_us1_0))
                             <*> I'.pure (ls_DeploymentAgent'AddQueryDeployer this'')))
                       =<< I'.readIORef this'))
               olive___QueryServiceImpl_0_m4_large_us1_0 ::
                 IORef' IQueryService <- I'.lift
                                           ((\ new'DC ->
                                               (I'.newIORef . IQueryService) =<<
                                                 (new new'DC init'QueryServiceImpl =<<
                                                    I'.pure smart'QueryServiceImpl <*>
                                                      (up' <$!>
                                                         I'.readIORef
                                                           oDef___DeploymentAgentImpl_0_m4_large_us1_0)
                                                      <*> I'.pure "Customer X"
                                                      <*> I'.pure False))
                                              =<< (up' <$!> I'.readIORef m4_large_us1_0))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_IQueryService'AddQueryDeployer = v'}) <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!> I'.readIORef olive___QueryServiceImpl_0_m4_large_us1_0)
                                <*> (up' <$!> I'.readIORef m4_large_us1_0))
                             <*> I'.pure (ls_IQueryService'AddQueryDeployer this'')))
                       =<< I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_Service'AddQueryDeployer = v'}) <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!> I'.readIORef olive___QueryServiceImpl_0_m4_large_us1_0)
                                <*> (up' <$!> I'.readIORef m4_large_us1_0))
                             <*> I'.pure (ls_Service'AddQueryDeployer this'')))
                       =<< I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_EndPoint'AddQueryDeployer = v'}) <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!> I'.readIORef olive___QueryServiceImpl_0_m4_large_us1_0)
                                <*> (up' <$!> I'.readIORef m4_large_us1_0))
                             <*> I'.pure (ls_EndPoint'AddQueryDeployer this'')))
                       =<< I'.readIORef this'))
               olive___QueryServiceImpl_0_m4_large_us2_0 ::
                 IORef' IQueryService <- I'.lift
                                           ((\ new'DC ->
                                               (I'.newIORef . IQueryService) =<<
                                                 (new new'DC init'QueryServiceImpl =<<
                                                    I'.pure smart'QueryServiceImpl <*>
                                                      (up' <$!>
                                                         I'.readIORef
                                                           oDef___DeploymentAgentImpl_0_m4_large_us2_0)
                                                      <*> I'.pure "Customer X"
                                                      <*> I'.pure False))
                                              =<< (up' <$!> I'.readIORef m4_large_us2_0))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_IQueryService'AddQueryDeployer = v'}) <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!> I'.readIORef olive___QueryServiceImpl_0_m4_large_us2_0)
                                <*> (up' <$!> I'.readIORef m4_large_us2_0))
                             <*> I'.pure (ls_IQueryService'AddQueryDeployer this'')))
                       =<< I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_Service'AddQueryDeployer = v'}) <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!> I'.readIORef olive___QueryServiceImpl_0_m4_large_us2_0)
                                <*> (up' <$!> I'.readIORef m4_large_us2_0))
                             <*> I'.pure (ls_Service'AddQueryDeployer this'')))
                       =<< I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_EndPoint'AddQueryDeployer = v'}) <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!> I'.readIORef olive___QueryServiceImpl_0_m4_large_us2_0)
                                <*> (up' <$!> I'.readIORef m4_large_us2_0))
                             <*> I'.pure (ls_EndPoint'AddQueryDeployer this'')))
                       =<< I'.readIORef this'))
               _ <- (\ this'' ->
                       (\ (LoadBalancerEndPoint obj') ->
                          awaitSugar'' this obj' =<<
                            I'.lift
                              (I'.pure addLBE <*>
                                 (up' <$!> I'.readIORef olive___QueryServiceImpl_0_m4_large_us2_0)))
                         (loadBalancerEndPointObjUs1'AddQueryDeployer this''))
                      =<< I'.lift (I'.readIORef this')
               _ <- (\ this'' ->
                       (\ (LoadBalancerEndPoint obj') ->
                          awaitSugar'' this obj' =<<
                            I'.lift
                              (I'.pure addLBE <*>
                                 (up' <$!> I'.readIORef olive___QueryServiceImpl_0_m4_large_us1_0)))
                         (loadBalancerEndPointObjUs1'AddQueryDeployer this''))
                      =<< I'.lift (I'.readIORef this')
               _ <- (\ this'' ->
                       (\ (LoadBalancerEndPoint obj') ->
                          awaitSugar'' this obj' =<<
                            I'.lift
                              (I'.pure addLBE <*>
                                 (up' <$!> I'.readIORef olive___QueryServiceImpl_0_m4_large_us2_0)))
                         (loadBalancerEndPointObjUs2'AddQueryDeployer this''))
                      =<< I'.lift (I'.readIORef this')
               _ <- (\ this'' ->
                       (\ (LoadBalancerEndPoint obj') ->
                          awaitSugar'' this obj' =<<
                            I'.lift
                              (I'.pure addLBE <*>
                                 (up' <$!> I'.readIORef olive___QueryServiceImpl_0_m4_large_us1_0)))
                         (loadBalancerEndPointObjUs2'AddQueryDeployer this''))
                      =<< I'.lift (I'.readIORef this')
               _ <- (\ this'' ->
                       (\ (MonitorPlatformService obj') ->
                          awaitSugar'' this obj' =<<
                            I'.lift
                              (I'.pure addServiceInstance <*>
                                 (up' <$!> I'.readIORef olive___QueryServiceImpl_0_m4_large_us2_0)))
                         (platformServiceObjUs'AddQueryDeployer this''))
                      =<< I'.lift (I'.readIORef this')
               _ <- (\ this'' ->
                       (\ (MonitorPlatformService obj') ->
                          awaitSugar'' this obj' =<<
                            I'.lift
                              (I'.pure addServiceInstance <*>
                                 (up' <$!> I'.readIORef olive___QueryServiceImpl_0_m4_large_us1_0)))
                         (platformServiceObjUs'AddQueryDeployer this''))
                      =<< I'.lift (I'.readIORef this')
               _ <- (\ (DeploymentAgent obj') ->
                       awaitSugar'' this obj' =<<
                         I'.lift
                           (I'.pure installDA <*>
                              (up' <$!> I'.readIORef olive___QueryServiceImpl_0_m4_large_us2_0)))
                      =<<
                      I'.lift (I'.readIORef oDef___DeploymentAgentImpl_0_m4_large_us2_0)
               _ <- (\ (DeploymentAgent obj') ->
                       awaitSugar'' this obj' =<<
                         I'.lift
                           (I'.pure installDA <*>
                              (up' <$!> I'.readIORef olive___QueryServiceImpl_0_m4_large_us1_0)))
                      =<<
                      I'.lift (I'.readIORef oDef___DeploymentAgentImpl_0_m4_large_us1_0)
               _ <- (\ this'' ->
                       (\ (DeploymentService obj') ->
                          awaitSugar'' this obj' =<<
                            I'.lift
                              (I'.pure addDS <*>
                                 (up' <$!>
                                    I'.readIORef oDef___DeploymentAgentImpl_0_m4_large_us2_0)))
                         (deploymentServiceObjUs'AddQueryDeployer this''))
                      =<< I'.lift (I'.readIORef this')
               _ <- (\ this'' ->
                       (\ (DeploymentService obj') ->
                          awaitSugar'' this obj' =<<
                            I'.lift
                              (I'.pure addDS <*>
                                 (up' <$!>
                                    I'.readIORef oDef___DeploymentAgentImpl_0_m4_large_us1_0)))
                         (deploymentServiceObjUs'AddQueryDeployer this''))
                      =<< I'.lift (I'.readIORef this')
               I'.pure ()
        undeploy this@(Obj' this' _ thisDC)
          = do I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' -> this''{ls_EndPoint'AddQueryDeployer = []}) <$!>
                       I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' -> this''{ls_IQueryService'AddQueryDeployer = []}) <$!>
                       I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' -> this''{ls_Service'AddQueryDeployer = []}) <$!>
                       I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' -> this''{ls_ServiceProvider'AddQueryDeployer = []})
                       <$!> I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' -> this''{ls_DeploymentAgent'AddQueryDeployer = []})
                       <$!> I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' -> this''{ls_LoadBalancerService'AddQueryDeployer = []})
                       <$!> I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        this''{ls_LoadBalancerEndPoint'AddQueryDeployer = []})
                       <$!> I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' -> this''{ls_DeploymentService'AddQueryDeployer = []})
                       <$!> I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' -> this''{ls_PlatformService'AddQueryDeployer = []})
                       <$!> I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        this''{ls_MonitorPlatformService'AddQueryDeployer = []})
                       <$!> I'.readIORef this'))
               while
                 ((\ this'' ->
                     ((not) <$!>
                        (I'.pure isEmpty <*>
                           I'.pure (ls_DeploymentComponent'AddQueryDeployer this''))))
                    =<< I'.readIORef this')
                 (do _ <- (\ this'' ->
                             (\ (CloudProvider obj') ->
                                sync' this obj'
                                  (shutdownInstance
                                     (head (ls_DeploymentComponent'AddQueryDeployer this''))))
                               (cloudProvider'AddQueryDeployer this''))
                            =<< I'.lift (I'.readIORef this')
                     I'.lift
                       (I'.writeIORef this' =<<
                          ((\ this'' ->
                              this''{ls_DeploymentComponent'AddQueryDeployer =
                                       (tail (ls_DeploymentComponent'AddQueryDeployer this''))})
                             <$!> I'.readIORef this')))

{-# LINE 500 "initial\FRH.abs" #-}
data MainSmartDeployer = MainSmartDeployer{cloudProvider'MainSmartDeployer
                                           :: CloudProvider,
                                           ls_DeploymentAgent'MainSmartDeployer ::
                                           List (Pair DeploymentAgent DeploymentComponent),
                                           ls_DeploymentComponent'MainSmartDeployer ::
                                           List DeploymentComponent,
                                           ls_DeploymentService'MainSmartDeployer ::
                                           List (Pair DeploymentService DeploymentComponent),
                                           ls_EndPoint'MainSmartDeployer ::
                                           List (Pair EndPoint DeploymentComponent),
                                           ls_IQueryService'MainSmartDeployer ::
                                           List (Pair IQueryService DeploymentComponent),
                                           ls_LoadBalancerEndPoint'MainSmartDeployer ::
                                           List (Pair LoadBalancerEndPoint DeploymentComponent),
                                           ls_LoadBalancerService'MainSmartDeployer ::
                                           List (Pair LoadBalancerService DeploymentComponent),
                                           ls_MonitorPlatformService'MainSmartDeployer ::
                                           List (Pair MonitorPlatformService DeploymentComponent),
                                           ls_PlatformService'MainSmartDeployer ::
                                           List (Pair PlatformService DeploymentComponent),
                                           ls_Service'MainSmartDeployer ::
                                           List (Pair Service DeploymentComponent),
                                           ls_ServiceProvider'MainSmartDeployer ::
                                           List (Pair ServiceProvider DeploymentComponent)}

smart'MainSmartDeployer :: CloudProvider -> MainSmartDeployer
smart'MainSmartDeployer cloudProvider'this
  = (\ ls_EndPoint'this ->
       (\ ls_IQueryService'this ->
          (\ ls_Service'this ->
             (\ ls_ServiceProvider'this ->
                (\ ls_DeploymentAgent'this ->
                   (\ ls_LoadBalancerService'this ->
                      (\ ls_LoadBalancerEndPoint'this ->
                         (\ ls_DeploymentService'this ->
                            (\ ls_PlatformService'this ->
                               (\ ls_MonitorPlatformService'this ->
                                  (\ ls_DeploymentComponent'this ->
                                     (MainSmartDeployer cloudProvider'this ls_DeploymentAgent'this
                                        ls_DeploymentComponent'this
                                        ls_DeploymentService'this
                                        ls_EndPoint'this
                                        ls_IQueryService'this
                                        ls_LoadBalancerEndPoint'this
                                        ls_LoadBalancerService'this
                                        ls_MonitorPlatformService'this
                                        ls_PlatformService'this
                                        ls_Service'this
                                        ls_ServiceProvider'this))
                                    ([] :: List DeploymentComponent))
                                 ([] :: List (Pair MonitorPlatformService DeploymentComponent)))
                              ([] :: List (Pair PlatformService DeploymentComponent)))
                           ([] :: List (Pair DeploymentService DeploymentComponent)))
                        ([] :: List (Pair LoadBalancerEndPoint DeploymentComponent)))
                     ([] :: List (Pair LoadBalancerService DeploymentComponent)))
                  ([] :: List (Pair DeploymentAgent DeploymentComponent)))
               ([] :: List (Pair ServiceProvider DeploymentComponent)))
            ([] :: List (Pair Service DeploymentComponent)))
         ([] :: List (Pair IQueryService DeploymentComponent)))
      ([] :: List (Pair EndPoint DeploymentComponent))

init'MainSmartDeployer :: Obj' MainSmartDeployer -> I'.IO ()
{-# LINE 500 "initial\FRH.abs" #-}
init'MainSmartDeployer this@(Obj' this' _ thisDC) = I'.pure ()

instance SmartDeployInterface' MainSmartDeployer where
        getEndPoint this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' -> I'.pure (ls_EndPoint'MainSmartDeployer this'')) =<<
                    I'.readIORef this')
        getIQueryService this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' -> I'.pure (ls_IQueryService'MainSmartDeployer this''))
                    =<< I'.readIORef this')
        getService this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' -> I'.pure (ls_Service'MainSmartDeployer this'')) =<<
                    I'.readIORef this')
        getServiceProvider this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' ->
                     I'.pure (ls_ServiceProvider'MainSmartDeployer this''))
                    =<< I'.readIORef this')
        getDeploymentAgent this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' ->
                     I'.pure (ls_DeploymentAgent'MainSmartDeployer this''))
                    =<< I'.readIORef this')
        getLoadBalancerService this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' ->
                     I'.pure (ls_LoadBalancerService'MainSmartDeployer this''))
                    =<< I'.readIORef this')
        getLoadBalancerEndPoint this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' ->
                     I'.pure (ls_LoadBalancerEndPoint'MainSmartDeployer this''))
                    =<< I'.readIORef this')
        getDeploymentService this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' ->
                     I'.pure (ls_DeploymentService'MainSmartDeployer this''))
                    =<< I'.readIORef this')
        getPlatformService this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' ->
                     I'.pure (ls_PlatformService'MainSmartDeployer this''))
                    =<< I'.readIORef this')
        getMonitorPlatformService this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' ->
                     I'.pure (ls_MonitorPlatformService'MainSmartDeployer this''))
                    =<< I'.readIORef this')
        getDeploymentComponent this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' ->
                     I'.pure (ls_DeploymentComponent'MainSmartDeployer this''))
                    =<< I'.readIORef this')
        deploy this@(Obj' this' _ thisDC)
          = do c4_2xlarge_eu_0 :: IORef' DeploymentComponent <- (\ this'' ->
                                                                   (I'.lift . I'.newIORef) =<<
                                                                     ((\ (CloudProvider obj') ->
                                                                         sync' this obj'
                                                                           (prelaunchInstanceNamed
                                                                              "c4_2xlarge_eu"))
                                                                        (cloudProvider'MainSmartDeployer
                                                                           this'')))
                                                                  =<< I'.lift (I'.readIORef this')
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_DeploymentComponent'MainSmartDeployer = v'})
                          <$!>
                          ((:) <$!> (up' <$!> I'.readIORef c4_2xlarge_eu_0) <*>
                             I'.pure (ls_DeploymentComponent'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               m4_large_eu_0 :: IORef' DeploymentComponent <- (\ this'' ->
                                                                 (I'.lift . I'.newIORef) =<<
                                                                   ((\ (CloudProvider obj') ->
                                                                       sync' this obj'
                                                                         (prelaunchInstanceNamed
                                                                            "m4_large_eu"))
                                                                      (cloudProvider'MainSmartDeployer
                                                                         this'')))
                                                                =<< I'.lift (I'.readIORef this')
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_DeploymentComponent'MainSmartDeployer = v'})
                          <$!>
                          ((:) <$!> (up' <$!> I'.readIORef m4_large_eu_0) <*>
                             I'.pure (ls_DeploymentComponent'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               m4_xlarge_eu_0 :: IORef' DeploymentComponent <- (\ this'' ->
                                                                  (I'.lift . I'.newIORef) =<<
                                                                    ((\ (CloudProvider obj') ->
                                                                        sync' this obj'
                                                                          (prelaunchInstanceNamed
                                                                             "m4_xlarge_eu"))
                                                                       (cloudProvider'MainSmartDeployer
                                                                          this'')))
                                                                 =<< I'.lift (I'.readIORef this')
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_DeploymentComponent'MainSmartDeployer = v'})
                          <$!>
                          ((:) <$!> (up' <$!> I'.readIORef m4_xlarge_eu_0) <*>
                             I'.pure (ls_DeploymentComponent'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               c4_2xlarge_us1_0 :: IORef' DeploymentComponent <- (\ this'' ->
                                                                    (I'.lift . I'.newIORef) =<<
                                                                      ((\ (CloudProvider obj') ->
                                                                          sync' this obj'
                                                                            (prelaunchInstanceNamed
                                                                               "c4_2xlarge_us1"))
                                                                         (cloudProvider'MainSmartDeployer
                                                                            this'')))
                                                                   =<< I'.lift (I'.readIORef this')
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_DeploymentComponent'MainSmartDeployer = v'})
                          <$!>
                          ((:) <$!> (up' <$!> I'.readIORef c4_2xlarge_us1_0) <*>
                             I'.pure (ls_DeploymentComponent'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               m4_large_us1_0 :: IORef' DeploymentComponent <- (\ this'' ->
                                                                  (I'.lift . I'.newIORef) =<<
                                                                    ((\ (CloudProvider obj') ->
                                                                        sync' this obj'
                                                                          (prelaunchInstanceNamed
                                                                             "m3_medium_us1"))
                                                                       (cloudProvider'MainSmartDeployer
                                                                          this'')))
                                                                 =<< I'.lift (I'.readIORef this')
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_DeploymentComponent'MainSmartDeployer = v'})
                          <$!>
                          ((:) <$!> (up' <$!> I'.readIORef m4_large_us1_0) <*>
                             I'.pure (ls_DeploymentComponent'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               c4_xlarge_eu_0 :: IORef' DeploymentComponent <- (\ this'' ->
                                                                  (I'.lift . I'.newIORef) =<<
                                                                    ((\ (CloudProvider obj') ->
                                                                        sync' this obj'
                                                                          (prelaunchInstanceNamed
                                                                             "c4_xlarge_eu"))
                                                                       (cloudProvider'MainSmartDeployer
                                                                          this'')))
                                                                 =<< I'.lift (I'.readIORef this')
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_DeploymentComponent'MainSmartDeployer = v'})
                          <$!>
                          ((:) <$!> (up' <$!> I'.readIORef c4_xlarge_eu_0) <*>
                             I'.pure (ls_DeploymentComponent'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               c4_xlarge_us1_0 :: IORef' DeploymentComponent <- (\ this'' ->
                                                                   (I'.lift . I'.newIORef) =<<
                                                                     ((\ (CloudProvider obj') ->
                                                                         sync' this obj'
                                                                           (prelaunchInstanceNamed
                                                                              "c4_xlarge_us1"))
                                                                        (cloudProvider'MainSmartDeployer
                                                                           this'')))
                                                                  =<< I'.lift (I'.readIORef this')
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_DeploymentComponent'MainSmartDeployer = v'})
                          <$!>
                          ((:) <$!> (up' <$!> I'.readIORef c4_xlarge_us1_0) <*>
                             I'.pure (ls_DeploymentComponent'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               c4_xlarge_us2_0 :: IORef' DeploymentComponent <- (\ this'' ->
                                                                   (I'.lift . I'.newIORef) =<<
                                                                     ((\ (CloudProvider obj') ->
                                                                         sync' this obj'
                                                                           (prelaunchInstanceNamed
                                                                              "c4_xlarge_us2"))
                                                                        (cloudProvider'MainSmartDeployer
                                                                           this'')))
                                                                  =<< I'.lift (I'.readIORef this')
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_DeploymentComponent'MainSmartDeployer = v'})
                          <$!>
                          ((:) <$!> (up' <$!> I'.readIORef c4_xlarge_us2_0) <*>
                             I'.pure (ls_DeploymentComponent'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               m4_xlarge_us2_0 :: IORef' DeploymentComponent <- (\ this'' ->
                                                                   (I'.lift . I'.newIORef) =<<
                                                                     ((\ (CloudProvider obj') ->
                                                                         sync' this obj'
                                                                           (prelaunchInstanceNamed
                                                                              "m3_medium_us2"))
                                                                        (cloudProvider'MainSmartDeployer
                                                                           this'')))
                                                                  =<< I'.lift (I'.readIORef this')
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_DeploymentComponent'MainSmartDeployer = v'})
                          <$!>
                          ((:) <$!> (up' <$!> I'.readIORef m4_xlarge_us2_0) <*>
                             I'.pure (ls_DeploymentComponent'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               oDef___LoadBalancerEndPointImpl_0_c4_xlarge_us1_0 ::
                 IORef' LoadBalancerEndPoint <- I'.lift
                                                  ((\ new'DC ->
                                                      (I'.newIORef . LoadBalancerEndPoint) =<<
                                                        (new new'DC init'LoadBalancerEndPointImpl
                                                           =<<
                                                           I'.pure smart'LoadBalancerEndPointImpl))
                                                     =<< (up' <$!> I'.readIORef c4_xlarge_us1_0))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_LoadBalancerEndPoint'MainSmartDeployer = v'})
                          <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!>
                                   I'.readIORef oDef___LoadBalancerEndPointImpl_0_c4_xlarge_us1_0)
                                <*> (up' <$!> I'.readIORef c4_xlarge_us1_0))
                             <*> I'.pure (ls_LoadBalancerEndPoint'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_EndPoint'MainSmartDeployer = v'}) <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!>
                                   I'.readIORef oDef___LoadBalancerEndPointImpl_0_c4_xlarge_us1_0)
                                <*> (up' <$!> I'.readIORef c4_xlarge_us1_0))
                             <*> I'.pure (ls_EndPoint'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               oDef___LoadBalancerServiceImpl_0_c4_2xlarge_us1_0 ::
                 IORef' LoadBalancerService <- I'.lift
                                                 ((\ new'DC ->
                                                     (I'.newIORef . LoadBalancerService) =<<
                                                       (new new'DC init'LoadBalancerServiceImpl =<<
                                                          I'.pure smart'LoadBalancerServiceImpl))
                                                    =<< (up' <$!> I'.readIORef c4_2xlarge_us1_0))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_LoadBalancerService'MainSmartDeployer = v'})
                          <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!>
                                   I'.readIORef oDef___LoadBalancerServiceImpl_0_c4_2xlarge_us1_0)
                                <*> (up' <$!> I'.readIORef c4_2xlarge_us1_0))
                             <*> I'.pure (ls_LoadBalancerService'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               oDef___DeploymentServiceImpl_0_c4_2xlarge_eu_0 ::
                 IORef' DeploymentService <- I'.lift
                                               ((\ new'DC ->
                                                   (I'.newIORef . DeploymentService) =<<
                                                     (new new'DC init'DeploymentServiceImpl =<<
                                                        I'.pure smart'DeploymentServiceImpl))
                                                  =<< (up' <$!> I'.readIORef c4_2xlarge_eu_0))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_DeploymentService'MainSmartDeployer = v'}) <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!>
                                   I'.readIORef oDef___DeploymentServiceImpl_0_c4_2xlarge_eu_0)
                                <*> (up' <$!> I'.readIORef c4_2xlarge_eu_0))
                             <*> I'.pure (ls_DeploymentService'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               oDef___DeploymentAgentImpl_0_m4_xlarge_eu_0 ::
                 IORef' DeploymentAgent <- I'.lift
                                             ((\ new'DC ->
                                                 (I'.newIORef . DeploymentAgent) =<<
                                                   (new new'DC init'DeploymentAgentImpl =<<
                                                      I'.pure smart'DeploymentAgentImpl))
                                                =<< (up' <$!> I'.readIORef m4_xlarge_eu_0))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_DeploymentAgent'MainSmartDeployer = v'}) <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!> I'.readIORef oDef___DeploymentAgentImpl_0_m4_xlarge_eu_0)
                                <*> (up' <$!> I'.readIORef m4_xlarge_eu_0))
                             <*> I'.pure (ls_DeploymentAgent'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               oDef___LoadBalancerEndPointImpl_0_c4_xlarge_us2_0 ::
                 IORef' LoadBalancerEndPoint <- I'.lift
                                                  ((\ new'DC ->
                                                      (I'.newIORef . LoadBalancerEndPoint) =<<
                                                        (new new'DC init'LoadBalancerEndPointImpl
                                                           =<<
                                                           I'.pure smart'LoadBalancerEndPointImpl))
                                                     =<< (up' <$!> I'.readIORef c4_xlarge_us2_0))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_LoadBalancerEndPoint'MainSmartDeployer = v'})
                          <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!>
                                   I'.readIORef oDef___LoadBalancerEndPointImpl_0_c4_xlarge_us2_0)
                                <*> (up' <$!> I'.readIORef c4_xlarge_us2_0))
                             <*> I'.pure (ls_LoadBalancerEndPoint'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_EndPoint'MainSmartDeployer = v'}) <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!>
                                   I'.readIORef oDef___LoadBalancerEndPointImpl_0_c4_xlarge_us2_0)
                                <*> (up' <$!> I'.readIORef c4_xlarge_us2_0))
                             <*> I'.pure (ls_EndPoint'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               oDef___LoadBalancerEndPointImpl_0_c4_xlarge_eu_0 ::
                 IORef' LoadBalancerEndPoint <- I'.lift
                                                  ((\ new'DC ->
                                                      (I'.newIORef . LoadBalancerEndPoint) =<<
                                                        (new new'DC init'LoadBalancerEndPointImpl
                                                           =<<
                                                           I'.pure smart'LoadBalancerEndPointImpl))
                                                     =<< (up' <$!> I'.readIORef c4_xlarge_eu_0))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_LoadBalancerEndPoint'MainSmartDeployer = v'})
                          <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!>
                                   I'.readIORef oDef___LoadBalancerEndPointImpl_0_c4_xlarge_eu_0)
                                <*> (up' <$!> I'.readIORef c4_xlarge_eu_0))
                             <*> I'.pure (ls_LoadBalancerEndPoint'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_EndPoint'MainSmartDeployer = v'}) <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!>
                                   I'.readIORef oDef___LoadBalancerEndPointImpl_0_c4_xlarge_eu_0)
                                <*> (up' <$!> I'.readIORef c4_xlarge_eu_0))
                             <*> I'.pure (ls_EndPoint'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               oDef___DeploymentAgentImpl_0_m4_large_eu_0 ::
                 IORef' DeploymentAgent <- I'.lift
                                             ((\ new'DC ->
                                                 (I'.newIORef . DeploymentAgent) =<<
                                                   (new new'DC init'DeploymentAgentImpl =<<
                                                      I'.pure smart'DeploymentAgentImpl))
                                                =<< (up' <$!> I'.readIORef m4_large_eu_0))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_DeploymentAgent'MainSmartDeployer = v'}) <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!> I'.readIORef oDef___DeploymentAgentImpl_0_m4_large_eu_0)
                                <*> (up' <$!> I'.readIORef m4_large_eu_0))
                             <*> I'.pure (ls_DeploymentAgent'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               oDef___LoadBalancerServiceImpl_0_c4_2xlarge_eu_0 ::
                 IORef' LoadBalancerService <- I'.lift
                                                 ((\ new'DC ->
                                                     (I'.newIORef . LoadBalancerService) =<<
                                                       (new new'DC init'LoadBalancerServiceImpl =<<
                                                          I'.pure smart'LoadBalancerServiceImpl))
                                                    =<< (up' <$!> I'.readIORef c4_2xlarge_eu_0))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_LoadBalancerService'MainSmartDeployer = v'})
                          <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!>
                                   I'.readIORef oDef___LoadBalancerServiceImpl_0_c4_2xlarge_eu_0)
                                <*> (up' <$!> I'.readIORef c4_2xlarge_eu_0))
                             <*> I'.pure (ls_LoadBalancerService'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               oDef___DeploymentAgentImpl_0_m4_large_us1_0 ::
                 IORef' DeploymentAgent <- I'.lift
                                             ((\ new'DC ->
                                                 (I'.newIORef . DeploymentAgent) =<<
                                                   (new new'DC init'DeploymentAgentImpl =<<
                                                      I'.pure smart'DeploymentAgentImpl))
                                                =<< (up' <$!> I'.readIORef m4_large_us1_0))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_DeploymentAgent'MainSmartDeployer = v'}) <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!> I'.readIORef oDef___DeploymentAgentImpl_0_m4_large_us1_0)
                                <*> (up' <$!> I'.readIORef m4_large_us1_0))
                             <*> I'.pure (ls_DeploymentAgent'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               oDef___DeploymentAgentImpl_0_m4_xlarge_us2_0 ::
                 IORef' DeploymentAgent <- I'.lift
                                             ((\ new'DC ->
                                                 (I'.newIORef . DeploymentAgent) =<<
                                                   (new new'DC init'DeploymentAgentImpl =<<
                                                      I'.pure smart'DeploymentAgentImpl))
                                                =<< (up' <$!> I'.readIORef m4_xlarge_us2_0))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_DeploymentAgent'MainSmartDeployer = v'}) <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!>
                                   I'.readIORef oDef___DeploymentAgentImpl_0_m4_xlarge_us2_0)
                                <*> (up' <$!> I'.readIORef m4_xlarge_us2_0))
                             <*> I'.pure (ls_DeploymentAgent'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               oDef___DeploymentServiceImpl_0_c4_2xlarge_us1_0 ::
                 IORef' DeploymentService <- I'.lift
                                               ((\ new'DC ->
                                                   (I'.newIORef . DeploymentService) =<<
                                                     (new new'DC init'DeploymentServiceImpl =<<
                                                        I'.pure smart'DeploymentServiceImpl))
                                                  =<< (up' <$!> I'.readIORef c4_2xlarge_us1_0))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_DeploymentService'MainSmartDeployer = v'}) <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!>
                                   I'.readIORef oDef___DeploymentServiceImpl_0_c4_2xlarge_us1_0)
                                <*> (up' <$!> I'.readIORef c4_2xlarge_us1_0))
                             <*> I'.pure (ls_DeploymentService'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               ostaging___QueryServiceImpl_0_m4_xlarge_us2_0 ::
                 IORef' IQueryService <- I'.lift
                                           ((\ new'DC ->
                                               (I'.newIORef . IQueryService) =<<
                                                 (new new'DC init'QueryServiceImpl =<<
                                                    I'.pure smart'QueryServiceImpl <*>
                                                      (up' <$!>
                                                         I'.readIORef
                                                           oDef___DeploymentAgentImpl_0_m4_xlarge_us2_0)
                                                      <*> I'.pure "Customer X"
                                                      <*> I'.pure True))
                                              =<< (up' <$!> I'.readIORef m4_xlarge_us2_0))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_IQueryService'MainSmartDeployer = v'}) <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!>
                                   I'.readIORef ostaging___QueryServiceImpl_0_m4_xlarge_us2_0)
                                <*> (up' <$!> I'.readIORef m4_xlarge_us2_0))
                             <*> I'.pure (ls_IQueryService'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_Service'MainSmartDeployer = v'}) <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!>
                                   I'.readIORef ostaging___QueryServiceImpl_0_m4_xlarge_us2_0)
                                <*> (up' <$!> I'.readIORef m4_xlarge_us2_0))
                             <*> I'.pure (ls_Service'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_EndPoint'MainSmartDeployer = v'}) <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!>
                                   I'.readIORef ostaging___QueryServiceImpl_0_m4_xlarge_us2_0)
                                <*> (up' <$!> I'.readIORef m4_xlarge_us2_0))
                             <*> I'.pure (ls_EndPoint'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               oDef___PlatformServiceImpl_0_c4_2xlarge_eu_0 ::
                 IORef' MonitorPlatformService <- I'.lift
                                                    ((\ new'DC ->
                                                        (I'.newIORef . MonitorPlatformService) =<<
                                                          (new new'DC init'PlatformServiceImpl =<<
                                                             I'.pure smart'PlatformServiceImpl <*>
                                                               (up' <$!>
                                                                  I'.readIORef
                                                                    oDef___DeploymentServiceImpl_0_c4_2xlarge_eu_0)
                                                               <*>
                                                               (up' <$!>
                                                                  I'.readIORef
                                                                    oDef___LoadBalancerServiceImpl_0_c4_2xlarge_eu_0)))
                                                       =<< (up' <$!> I'.readIORef c4_2xlarge_eu_0))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_MonitorPlatformService'MainSmartDeployer = v'})
                          <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!>
                                   I'.readIORef oDef___PlatformServiceImpl_0_c4_2xlarge_eu_0)
                                <*> (up' <$!> I'.readIORef c4_2xlarge_eu_0))
                             <*> I'.pure (ls_MonitorPlatformService'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_PlatformService'MainSmartDeployer = v'}) <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!>
                                   I'.readIORef oDef___PlatformServiceImpl_0_c4_2xlarge_eu_0)
                                <*> (up' <$!> I'.readIORef c4_2xlarge_eu_0))
                             <*> I'.pure (ls_PlatformService'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               olive___QueryServiceImpl_0_m4_large_us1_0 ::
                 IORef' IQueryService <- I'.lift
                                           ((\ new'DC ->
                                               (I'.newIORef . IQueryService) =<<
                                                 (new new'DC init'QueryServiceImpl =<<
                                                    I'.pure smart'QueryServiceImpl <*>
                                                      (up' <$!>
                                                         I'.readIORef
                                                           oDef___DeploymentAgentImpl_0_m4_large_us1_0)
                                                      <*> I'.pure "Customer X"
                                                      <*> I'.pure False))
                                              =<< (up' <$!> I'.readIORef m4_large_us1_0))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_IQueryService'MainSmartDeployer = v'}) <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!> I'.readIORef olive___QueryServiceImpl_0_m4_large_us1_0)
                                <*> (up' <$!> I'.readIORef m4_large_us1_0))
                             <*> I'.pure (ls_IQueryService'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_Service'MainSmartDeployer = v'}) <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!> I'.readIORef olive___QueryServiceImpl_0_m4_large_us1_0)
                                <*> (up' <$!> I'.readIORef m4_large_us1_0))
                             <*> I'.pure (ls_Service'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_EndPoint'MainSmartDeployer = v'}) <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!> I'.readIORef olive___QueryServiceImpl_0_m4_large_us1_0)
                                <*> (up' <$!> I'.readIORef m4_large_us1_0))
                             <*> I'.pure (ls_EndPoint'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               ostaging___QueryServiceImpl_0_m4_xlarge_eu_0 ::
                 IORef' IQueryService <- I'.lift
                                           ((\ new'DC ->
                                               (I'.newIORef . IQueryService) =<<
                                                 (new new'DC init'QueryServiceImpl =<<
                                                    I'.pure smart'QueryServiceImpl <*>
                                                      (up' <$!>
                                                         I'.readIORef
                                                           oDef___DeploymentAgentImpl_0_m4_xlarge_eu_0)
                                                      <*> I'.pure "Customer X"
                                                      <*> I'.pure True))
                                              =<< (up' <$!> I'.readIORef m4_xlarge_eu_0))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_IQueryService'MainSmartDeployer = v'}) <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!>
                                   I'.readIORef ostaging___QueryServiceImpl_0_m4_xlarge_eu_0)
                                <*> (up' <$!> I'.readIORef m4_xlarge_eu_0))
                             <*> I'.pure (ls_IQueryService'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_Service'MainSmartDeployer = v'}) <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!>
                                   I'.readIORef ostaging___QueryServiceImpl_0_m4_xlarge_eu_0)
                                <*> (up' <$!> I'.readIORef m4_xlarge_eu_0))
                             <*> I'.pure (ls_Service'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_EndPoint'MainSmartDeployer = v'}) <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!>
                                   I'.readIORef ostaging___QueryServiceImpl_0_m4_xlarge_eu_0)
                                <*> (up' <$!> I'.readIORef m4_xlarge_eu_0))
                             <*> I'.pure (ls_EndPoint'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               olive___QueryServiceImpl_0_m4_large_eu_0 ::
                 IORef' IQueryService <- I'.lift
                                           ((\ new'DC ->
                                               (I'.newIORef . IQueryService) =<<
                                                 (new new'DC init'QueryServiceImpl =<<
                                                    I'.pure smart'QueryServiceImpl <*>
                                                      (up' <$!>
                                                         I'.readIORef
                                                           oDef___DeploymentAgentImpl_0_m4_large_eu_0)
                                                      <*> I'.pure "Customer X"
                                                      <*> I'.pure False))
                                              =<< (up' <$!> I'.readIORef m4_large_eu_0))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_IQueryService'MainSmartDeployer = v'}) <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!> I'.readIORef olive___QueryServiceImpl_0_m4_large_eu_0)
                                <*> (up' <$!> I'.readIORef m4_large_eu_0))
                             <*> I'.pure (ls_IQueryService'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_Service'MainSmartDeployer = v'}) <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!> I'.readIORef olive___QueryServiceImpl_0_m4_large_eu_0)
                                <*> (up' <$!> I'.readIORef m4_large_eu_0))
                             <*> I'.pure (ls_Service'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_EndPoint'MainSmartDeployer = v'}) <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!> I'.readIORef olive___QueryServiceImpl_0_m4_large_eu_0)
                                <*> (up' <$!> I'.readIORef m4_large_eu_0))
                             <*> I'.pure (ls_EndPoint'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               oDef___PlatformServiceImpl_0_c4_2xlarge_us1_0 ::
                 IORef' MonitorPlatformService <- I'.lift
                                                    ((\ new'DC ->
                                                        (I'.newIORef . MonitorPlatformService) =<<
                                                          (new new'DC init'PlatformServiceImpl =<<
                                                             I'.pure smart'PlatformServiceImpl <*>
                                                               (up' <$!>
                                                                  I'.readIORef
                                                                    oDef___DeploymentServiceImpl_0_c4_2xlarge_us1_0)
                                                               <*>
                                                               (up' <$!>
                                                                  I'.readIORef
                                                                    oDef___LoadBalancerServiceImpl_0_c4_2xlarge_us1_0)))
                                                       =<< (up' <$!> I'.readIORef c4_2xlarge_us1_0))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_MonitorPlatformService'MainSmartDeployer = v'})
                          <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!>
                                   I'.readIORef oDef___PlatformServiceImpl_0_c4_2xlarge_us1_0)
                                <*> (up' <$!> I'.readIORef c4_2xlarge_us1_0))
                             <*> I'.pure (ls_MonitorPlatformService'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{ls_PlatformService'MainSmartDeployer = v'}) <$!>
                          ((:) <$!>
                             ((,) <$!>
                                (up' <$!>
                                   I'.readIORef oDef___PlatformServiceImpl_0_c4_2xlarge_us1_0)
                                <*> (up' <$!> I'.readIORef c4_2xlarge_us1_0))
                             <*> I'.pure (ls_PlatformService'MainSmartDeployer this'')))
                       =<< I'.readIORef this'))
               _ <- (\ (MonitorPlatformService obj') ->
                       awaitSugar'' this obj' =<<
                         I'.lift
                           (I'.pure addEndPoint <*>
                              (up' <$!>
                                 I'.readIORef oDef___LoadBalancerEndPointImpl_0_c4_xlarge_eu_0)))
                      =<<
                      I'.lift (I'.readIORef oDef___PlatformServiceImpl_0_c4_2xlarge_eu_0)
               _ <- (\ (MonitorPlatformService obj') ->
                       awaitSugar'' this obj' =<<
                         I'.lift
                           (I'.pure addEndPoint <*>
                              (up' <$!>
                                 I'.readIORef oDef___LoadBalancerEndPointImpl_0_c4_xlarge_us1_0)))
                      =<<
                      I'.lift
                        (I'.readIORef oDef___PlatformServiceImpl_0_c4_2xlarge_us1_0)
               _ <- (\ (MonitorPlatformService obj') ->
                       awaitSugar'' this obj' =<<
                         I'.lift
                           (I'.pure addEndPoint <*>
                              (up' <$!>
                                 I'.readIORef oDef___LoadBalancerEndPointImpl_0_c4_xlarge_us2_0)))
                      =<<
                      I'.lift
                        (I'.readIORef oDef___PlatformServiceImpl_0_c4_2xlarge_us1_0)
               _ <- (\ (LoadBalancerEndPoint obj') ->
                       awaitSugar'' this obj' =<<
                         I'.lift
                           (I'.pure addLBE <*>
                              (up' <$!> I'.readIORef olive___QueryServiceImpl_0_m4_large_eu_0)))
                      =<<
                      I'.lift
                        (I'.readIORef oDef___LoadBalancerEndPointImpl_0_c4_xlarge_eu_0)
               _ <- (\ (LoadBalancerEndPoint obj') ->
                       awaitSugar'' this obj' =<<
                         I'.lift
                           (I'.pure addLBE <*>
                              (up' <$!>
                                 I'.readIORef ostaging___QueryServiceImpl_0_m4_xlarge_eu_0)))
                      =<<
                      I'.lift
                        (I'.readIORef oDef___LoadBalancerEndPointImpl_0_c4_xlarge_eu_0)
               _ <- (\ (LoadBalancerEndPoint obj') ->
                       awaitSugar'' this obj' =<<
                         I'.lift
                           (I'.pure addLBE <*>
                              (up' <$!> I'.readIORef olive___QueryServiceImpl_0_m4_large_us1_0)))
                      =<<
                      I'.lift
                        (I'.readIORef oDef___LoadBalancerEndPointImpl_0_c4_xlarge_us1_0)
               _ <- (\ (LoadBalancerEndPoint obj') ->
                       awaitSugar'' this obj' =<<
                         I'.lift
                           (I'.pure addLBE <*>
                              (up' <$!> I'.readIORef olive___QueryServiceImpl_0_m4_large_us1_0)))
                      =<<
                      I'.lift
                        (I'.readIORef oDef___LoadBalancerEndPointImpl_0_c4_xlarge_us2_0)
               _ <- (\ (LoadBalancerEndPoint obj') ->
                       awaitSugar'' this obj' =<<
                         I'.lift
                           (I'.pure addLBE <*>
                              (up' <$!>
                                 I'.readIORef ostaging___QueryServiceImpl_0_m4_xlarge_us2_0)))
                      =<<
                      I'.lift
                        (I'.readIORef oDef___LoadBalancerEndPointImpl_0_c4_xlarge_us1_0)
               _ <- (\ (LoadBalancerEndPoint obj') ->
                       awaitSugar'' this obj' =<<
                         I'.lift
                           (I'.pure addLBE <*>
                              (up' <$!>
                                 I'.readIORef ostaging___QueryServiceImpl_0_m4_xlarge_us2_0)))
                      =<<
                      I'.lift
                        (I'.readIORef oDef___LoadBalancerEndPointImpl_0_c4_xlarge_us2_0)
               _ <- (\ (MonitorPlatformService obj') ->
                       awaitSugar'' this obj' =<<
                         I'.lift
                           (I'.pure addServiceInstance <*>
                              (up' <$!> I'.readIORef olive___QueryServiceImpl_0_m4_large_eu_0)))
                      =<<
                      I'.lift (I'.readIORef oDef___PlatformServiceImpl_0_c4_2xlarge_eu_0)
               _ <- (\ (MonitorPlatformService obj') ->
                       awaitSugar'' this obj' =<<
                         I'.lift
                           (I'.pure addServiceInstance <*>
                              (up' <$!>
                                 I'.readIORef ostaging___QueryServiceImpl_0_m4_xlarge_eu_0)))
                      =<<
                      I'.lift (I'.readIORef oDef___PlatformServiceImpl_0_c4_2xlarge_eu_0)
               _ <- (\ (MonitorPlatformService obj') ->
                       awaitSugar'' this obj' =<<
                         I'.lift
                           (I'.pure addServiceInstance <*>
                              (up' <$!> I'.readIORef olive___QueryServiceImpl_0_m4_large_us1_0)))
                      =<<
                      I'.lift
                        (I'.readIORef oDef___PlatformServiceImpl_0_c4_2xlarge_us1_0)
               _ <- (\ (MonitorPlatformService obj') ->
                       awaitSugar'' this obj' =<<
                         I'.lift
                           (I'.pure addServiceInstance <*>
                              (up' <$!>
                                 I'.readIORef ostaging___QueryServiceImpl_0_m4_xlarge_us2_0)))
                      =<<
                      I'.lift
                        (I'.readIORef oDef___PlatformServiceImpl_0_c4_2xlarge_us1_0)
               _ <- (\ (DeploymentAgent obj') ->
                       awaitSugar'' this obj' =<<
                         I'.lift
                           (I'.pure installDA <*>
                              (up' <$!> I'.readIORef olive___QueryServiceImpl_0_m4_large_eu_0)))
                      =<<
                      I'.lift (I'.readIORef oDef___DeploymentAgentImpl_0_m4_large_eu_0)
               _ <- (\ (DeploymentAgent obj') ->
                       awaitSugar'' this obj' =<<
                         I'.lift
                           (I'.pure installDA <*>
                              (up' <$!>
                                 I'.readIORef ostaging___QueryServiceImpl_0_m4_xlarge_eu_0)))
                      =<<
                      I'.lift (I'.readIORef oDef___DeploymentAgentImpl_0_m4_xlarge_eu_0)
               _ <- (\ (DeploymentAgent obj') ->
                       awaitSugar'' this obj' =<<
                         I'.lift
                           (I'.pure installDA <*>
                              (up' <$!> I'.readIORef olive___QueryServiceImpl_0_m4_large_us1_0)))
                      =<<
                      I'.lift (I'.readIORef oDef___DeploymentAgentImpl_0_m4_large_us1_0)
               _ <- (\ (DeploymentAgent obj') ->
                       awaitSugar'' this obj' =<<
                         I'.lift
                           (I'.pure installDA <*>
                              (up' <$!>
                                 I'.readIORef ostaging___QueryServiceImpl_0_m4_xlarge_us2_0)))
                      =<<
                      I'.lift (I'.readIORef oDef___DeploymentAgentImpl_0_m4_xlarge_us2_0)
               _ <- (\ (DeploymentService obj') ->
                       awaitSugar'' this obj' =<<
                         I'.lift
                           (I'.pure addDS <*>
                              (up' <$!>
                                 I'.readIORef oDef___DeploymentAgentImpl_0_m4_large_eu_0)))
                      =<<
                      I'.lift
                        (I'.readIORef oDef___DeploymentServiceImpl_0_c4_2xlarge_eu_0)
               _ <- (\ (DeploymentService obj') ->
                       awaitSugar'' this obj' =<<
                         I'.lift
                           (I'.pure addDS <*>
                              (up' <$!>
                                 I'.readIORef oDef___DeploymentAgentImpl_0_m4_xlarge_eu_0)))
                      =<<
                      I'.lift
                        (I'.readIORef oDef___DeploymentServiceImpl_0_c4_2xlarge_eu_0)
               _ <- (\ (DeploymentService obj') ->
                       awaitSugar'' this obj' =<<
                         I'.lift
                           (I'.pure addDS <*>
                              (up' <$!>
                                 I'.readIORef oDef___DeploymentAgentImpl_0_m4_large_us1_0)))
                      =<<
                      I'.lift
                        (I'.readIORef oDef___DeploymentServiceImpl_0_c4_2xlarge_us1_0)
               _ <- (\ (DeploymentService obj') ->
                       awaitSugar'' this obj' =<<
                         I'.lift
                           (I'.pure addDS <*>
                              (up' <$!>
                                 I'.readIORef oDef___DeploymentAgentImpl_0_m4_xlarge_us2_0)))
                      =<<
                      I'.lift
                        (I'.readIORef oDef___DeploymentServiceImpl_0_c4_2xlarge_us1_0)
               I'.pure ()
        undeploy this@(Obj' this' _ thisDC)
          = do I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' -> this''{ls_EndPoint'MainSmartDeployer = []}) <$!>
                       I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' -> this''{ls_IQueryService'MainSmartDeployer = []}) <$!>
                       I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' -> this''{ls_Service'MainSmartDeployer = []}) <$!>
                       I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' -> this''{ls_ServiceProvider'MainSmartDeployer = []})
                       <$!> I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' -> this''{ls_DeploymentAgent'MainSmartDeployer = []})
                       <$!> I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        this''{ls_LoadBalancerService'MainSmartDeployer = []})
                       <$!> I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        this''{ls_LoadBalancerEndPoint'MainSmartDeployer = []})
                       <$!> I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' -> this''{ls_DeploymentService'MainSmartDeployer = []})
                       <$!> I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' -> this''{ls_PlatformService'MainSmartDeployer = []})
                       <$!> I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        this''{ls_MonitorPlatformService'MainSmartDeployer = []})
                       <$!> I'.readIORef this'))
               while
                 ((\ this'' ->
                     ((not) <$!>
                        (I'.pure isEmpty <*>
                           I'.pure (ls_DeploymentComponent'MainSmartDeployer this''))))
                    =<< I'.readIORef this')
                 (do _ <- (\ this'' ->
                             (\ (CloudProvider obj') ->
                                sync' this obj'
                                  (shutdownInstance
                                     (head (ls_DeploymentComponent'MainSmartDeployer this''))))
                               (cloudProvider'MainSmartDeployer this''))
                            =<< I'.lift (I'.readIORef this')
                     I'.lift
                       (I'.writeIORef this' =<<
                          ((\ this'' ->
                              this''{ls_DeploymentComponent'MainSmartDeployer =
                                       (tail (ls_DeploymentComponent'MainSmartDeployer this''))})
                             <$!> I'.readIORef this')))

{-# LINE 673 "initial\FRH.abs" #-}
class DeployerIF' a where
        {-# LINE 674 "initial\FRH.abs" #-}
        scaleUp :: Obj' a -> ABS' Unit
        
        {-# LINE 675 "initial\FRH.abs" #-}
        scaleDown :: Obj' a -> ABS' Unit

data DeployerIF = forall a . DeployerIF' a => DeployerIF (Obj' a)

instance I'.Show DeployerIF where
        show _ = "DeployerIF"

instance I'.Eq DeployerIF where
        DeployerIF (Obj' ref1' _ _) == DeployerIF (Obj' ref2' _ _)
          = ref1' == I'.unsafeCoerce ref2'

instance DeployerIF' Null' where
        scaleUp = I'.undefined
        scaleDown = I'.undefined

instance DeployerIF' a => Sub' (Obj' a) DeployerIF where
        up' = DeployerIF

{-# LINE 678 "initial\FRH.abs" #-}
class InfrastructureService' a where
        {-# LINE 679 "initial\FRH.abs" #-}
        acquireInstance ::
                        Id -> VMType -> Obj' a -> ABS' DeploymentComponent
        
        {-# LINE 680 "initial\FRH.abs" #-}
        release :: DeploymentComponent -> Obj' a -> ABS' Unit

data InfrastructureService = forall a . InfrastructureService' a =>
                               InfrastructureService (Obj' a)

instance I'.Show InfrastructureService where
        show _ = "InfrastructureService"

instance I'.Eq InfrastructureService where
        InfrastructureService (Obj' ref1' _ _) ==
          InfrastructureService (Obj' ref2' _ _)
          = ref1' == I'.unsafeCoerce ref2'

instance InfrastructureService' Null' where
        acquireInstance = I'.undefined
        release = I'.undefined

instance InfrastructureService' a => Sub' (Obj' a)
         InfrastructureService where
        up' = InfrastructureService

{-# LINE 683 "initial\FRH.abs" #-}
class EndPoint' a where
        {-# LINE 684 "initial\FRH.abs" #-}
        invoke :: Request -> Obj' a -> ABS' Response
        
        {-# LINE 685 "initial\FRH.abs" #-}
        setStatus :: State -> Obj' a -> ABS' Unit
        
        {-# LINE 686 "initial\FRH.abs" #-}
        getStatus :: Obj' a -> ABS' State

data EndPoint = forall a . EndPoint' a => EndPoint (Obj' a)

instance I'.Show EndPoint where
        show _ = "EndPoint"

instance I'.Eq EndPoint where
        EndPoint (Obj' ref1' _ _) == EndPoint (Obj' ref2' _ _)
          = ref1' == I'.unsafeCoerce ref2'

instance EndPoint' Null' where
        invoke = I'.undefined
        setStatus = I'.undefined
        getStatus = I'.undefined

instance EndPoint' a => Sub' (Obj' a) EndPoint where
        up' = EndPoint

{-# LINE 689 "initial\FRH.abs" #-}
class EndPoint' a => LoadBalancerEndPoint' a where
        {-# LINE 690 "initial\FRH.abs" #-}
        removeLBE :: Service -> Obj' a -> ABS' Bool
        
        {-# LINE 691 "initial\FRH.abs" #-}
        addLBE :: Service -> Obj' a -> ABS' Bool
        
        {-# LINE 692 "initial\FRH.abs" #-}
        getServices :: Obj' a -> ABS' (List Service)

data LoadBalancerEndPoint = forall a . LoadBalancerEndPoint' a =>
                              LoadBalancerEndPoint (Obj' a)

instance I'.Show LoadBalancerEndPoint where
        show _ = "LoadBalancerEndPoint"

instance I'.Eq LoadBalancerEndPoint where
        LoadBalancerEndPoint (Obj' ref1' _ _) ==
          LoadBalancerEndPoint (Obj' ref2' _ _)
          = ref1' == I'.unsafeCoerce ref2'

instance LoadBalancerEndPoint' Null' where
        removeLBE = I'.undefined
        addLBE = I'.undefined
        getServices = I'.undefined

instance LoadBalancerEndPoint' a => Sub' (Obj' a)
         LoadBalancerEndPoint where
        up' = LoadBalancerEndPoint

instance Sub' LoadBalancerEndPoint EndPoint where
        up' (LoadBalancerEndPoint x') = EndPoint x'

{-# LINE 695 "initial\FRH.abs" #-}
class EndPoint' a => Service' a where
        {-# LINE 696 "initial\FRH.abs" #-}
        getServiceId :: Obj' a -> ABS' Id
        
        {-# LINE 697 "initial\FRH.abs" #-}
        setServiceId :: Id -> Obj' a -> ABS' Unit
        
        {-# LINE 698 "initial\FRH.abs" #-}
        getServiceType :: Obj' a -> ABS' ServiceType
        
        {-# LINE 699 "initial\FRH.abs" #-}
        getCustomer :: Obj' a -> ABS' Customer
        
        {-# LINE 700 "initial\FRH.abs" #-}
        getLatency :: Obj' a -> ABS' Int
        
        {-# LINE 701 "initial\FRH.abs" #-}
        getRequestCount :: Obj' a -> ABS' Int
        
        {-# LINE 702 "initial\FRH.abs" #-}
        getCPU :: Obj' a -> ABS' Int
        
        {-# LINE 703 "initial\FRH.abs" #-}
        getBandwidth :: Obj' a -> ABS' Int
        
        {-# LINE 704 "initial\FRH.abs" #-}
        getMemory :: Obj' a -> ABS' Int
        
        {-# LINE 705 "initial\FRH.abs" #-}
        getResource :: Resourcetype -> Obj' a -> ABS' InfRat

data Service = forall a . Service' a => Service (Obj' a)

instance I'.Show Service where
        show _ = "Service"

instance I'.Eq Service where
        Service (Obj' ref1' _ _) == Service (Obj' ref2' _ _)
          = ref1' == I'.unsafeCoerce ref2'

instance Service' Null' where
        getServiceId = I'.undefined
        setServiceId = I'.undefined
        getServiceType = I'.undefined
        getCustomer = I'.undefined
        getLatency = I'.undefined
        getRequestCount = I'.undefined
        getCPU = I'.undefined
        getBandwidth = I'.undefined
        getMemory = I'.undefined
        getResource = I'.undefined

instance Service' a => Sub' (Obj' a) Service where
        up' = Service

instance Sub' Service EndPoint where
        up' (Service x') = EndPoint x'

{-# LINE 708 "initial\FRH.abs" #-}
class DeploymentService' a where
        {-# LINE 709 "initial\FRH.abs" #-}
        installDS ::
                  Customer -> ServiceType -> Id -> VMType -> Obj' a -> ABS' Service
        
        {-# LINE 710 "initial\FRH.abs" #-}
        uninstallDS :: Id -> Obj' a -> ABS' Unit
        
        {-# LINE 711 "initial\FRH.abs" #-}
        startDS :: Id -> Obj' a -> ABS' Unit
        
        {-# LINE 712 "initial\FRH.abs" #-}
        stopDS :: Id -> Obj' a -> ABS' Unit
        
        {-# LINE 713 "initial\FRH.abs" #-}
        addDS :: DeploymentAgent -> Obj' a -> ABS' Unit

data DeploymentService = forall a . DeploymentService' a =>
                           DeploymentService (Obj' a)

instance I'.Show DeploymentService where
        show _ = "DeploymentService"

instance I'.Eq DeploymentService where
        DeploymentService (Obj' ref1' _ _) ==
          DeploymentService (Obj' ref2' _ _) = ref1' == I'.unsafeCoerce ref2'

instance DeploymentService' Null' where
        installDS = I'.undefined
        uninstallDS = I'.undefined
        startDS = I'.undefined
        stopDS = I'.undefined
        addDS = I'.undefined

instance DeploymentService' a => Sub' (Obj' a) DeploymentService
         where
        up' = DeploymentService

{-# LINE 716 "initial\FRH.abs" #-}
class DeploymentAgent' a where
        {-# LINE 717 "initial\FRH.abs" #-}
        installDA :: Service -> Obj' a -> ABS' Unit
        
        {-# LINE 718 "initial\FRH.abs" #-}
        uninstallDA :: Obj' a -> ABS' Unit
        
        {-# LINE 719 "initial\FRH.abs" #-}
        startDA :: Obj' a -> ABS' Unit
        
        {-# LINE 720 "initial\FRH.abs" #-}
        stopDA :: Obj' a -> ABS' Unit
        
        {-# LINE 721 "initial\FRH.abs" #-}
        getServiceDA :: Obj' a -> ABS' Service

data DeploymentAgent = forall a . DeploymentAgent' a =>
                         DeploymentAgent (Obj' a)

instance I'.Show DeploymentAgent where
        show _ = "DeploymentAgent"

instance I'.Eq DeploymentAgent where
        DeploymentAgent (Obj' ref1' _ _) ==
          DeploymentAgent (Obj' ref2' _ _) = ref1' == I'.unsafeCoerce ref2'

instance DeploymentAgent' Null' where
        installDA = I'.undefined
        uninstallDA = I'.undefined
        startDA = I'.undefined
        stopDA = I'.undefined
        getServiceDA = I'.undefined

instance DeploymentAgent' a => Sub' (Obj' a) DeploymentAgent where
        up' = DeploymentAgent

{-# LINE 724 "initial\FRH.abs" #-}
class LoadBalancerService' a where
        {-# LINE 725 "initial\FRH.abs" #-}
        disable :: Id -> Obj' a -> ABS' Bool
        
        {-# LINE 726 "initial\FRH.abs" #-}
        enable :: Id -> Obj' a -> ABS' Bool
        
        {-# LINE 727 "initial\FRH.abs" #-}
        create :: List Service -> Int -> Obj' a -> ABS' Bool
        
        {-# LINE 728 "initial\FRH.abs" #-}
        addLBS :: Int -> LoadBalancerEndPoint -> Obj' a -> ABS' Bool
        
        {-# LINE 729 "initial\FRH.abs" #-}
        removeLBS :: Id -> Obj' a -> ABS' Bool
        
        {-# LINE 730 "initial\FRH.abs" #-}
        getEndPointId :: LoadBalancerEndPoint -> Obj' a -> ABS' (Maybe Id)
        
        {-# LINE 731 "initial\FRH.abs" #-}
        getEndPointById ::
                        Int -> Obj' a -> ABS' (Maybe LoadBalancerEndPoint)
        
        {-# LINE 732 "initial\FRH.abs" #-}
        getEndPointIdsByService :: Service -> Obj' a -> ABS' (List Id)
        
        {-# LINE 733 "initial\FRH.abs" #-}
        decrease :: Id -> List Service -> Obj' a -> ABS' Bool
        
        {-# LINE 734 "initial\FRH.abs" #-}
        increase :: Id -> List Service -> Obj' a -> ABS' Bool

data LoadBalancerService = forall a . LoadBalancerService' a =>
                             LoadBalancerService (Obj' a)

instance I'.Show LoadBalancerService where
        show _ = "LoadBalancerService"

instance I'.Eq LoadBalancerService where
        LoadBalancerService (Obj' ref1' _ _) ==
          LoadBalancerService (Obj' ref2' _ _)
          = ref1' == I'.unsafeCoerce ref2'

instance LoadBalancerService' Null' where
        disable = I'.undefined
        enable = I'.undefined
        create = I'.undefined
        addLBS = I'.undefined
        removeLBS = I'.undefined
        getEndPointId = I'.undefined
        getEndPointById = I'.undefined
        getEndPointIdsByService = I'.undefined
        decrease = I'.undefined
        increase = I'.undefined

instance LoadBalancerService' a => Sub' (Obj' a)
         LoadBalancerService where
        up' = LoadBalancerService

{-# LINE 737 "initial\FRH.abs" #-}
class PlatformService' a where
        {-# LINE 738 "initial\FRH.abs" #-}
        createService :: Config -> Customer -> Obj' a -> ABS' Id
        
        {-# LINE 739 "initial\FRH.abs" #-}
        removeService :: Id -> Obj' a -> ABS' Unit

data PlatformService = forall a . PlatformService' a =>
                         PlatformService (Obj' a)

instance I'.Show PlatformService where
        show _ = "PlatformService"

instance I'.Eq PlatformService where
        PlatformService (Obj' ref1' _ _) ==
          PlatformService (Obj' ref2' _ _) = ref1' == I'.unsafeCoerce ref2'

instance PlatformService' Null' where
        createService = I'.undefined
        removeService = I'.undefined

instance PlatformService' a => Sub' (Obj' a) PlatformService where
        up' = PlatformService

{-# LINE 742 "initial\FRH.abs" #-}
class PlatformService' a => MonitorPlatformService' a where
        {-# LINE 743 "initial\FRH.abs" #-}
        incrService :: Id -> List ResourceCapacities -> Obj' a -> ABS' Unit
        
        {-# LINE 744 "initial\FRH.abs" #-}
        decrService :: Id -> List Id -> Obj' a -> ABS' Unit
        
        {-# LINE 745 "initial\FRH.abs" #-}
        getEndPoints :: Obj' a -> ABS' (List Id)
        
        {-# LINE 746 "initial\FRH.abs" #-}
        getServiceMPS :: Id -> Obj' a -> ABS' (Maybe Service)
        
        {-# LINE 747 "initial\FRH.abs" #-}
        getServiceIds :: Id -> Obj' a -> ABS' (List Id)
        
        {-# LINE 748 "initial\FRH.abs" #-}
        alterResource :: Id -> Resourcetype -> Rat -> Obj' a -> ABS' Unit
        
        {-# LINE 749 "initial\FRH.abs" #-}
        addEndPoint :: LoadBalancerEndPoint -> Obj' a -> ABS' Id
        
        {-# LINE 750 "initial\FRH.abs" #-}
        removeEndPoint :: LoadBalancerEndPoint -> Obj' a -> ABS' Id
        
        {-# LINE 751 "initial\FRH.abs" #-}
        addServiceInstance :: Service -> Obj' a -> ABS' Id

data MonitorPlatformService = forall a .
                                MonitorPlatformService' a => MonitorPlatformService (Obj' a)

instance I'.Show MonitorPlatformService where
        show _ = "MonitorPlatformService"

instance I'.Eq MonitorPlatformService where
        MonitorPlatformService (Obj' ref1' _ _) ==
          MonitorPlatformService (Obj' ref2' _ _)
          = ref1' == I'.unsafeCoerce ref2'

instance MonitorPlatformService' Null' where
        incrService = I'.undefined
        decrService = I'.undefined
        getEndPoints = I'.undefined
        getServiceMPS = I'.undefined
        getServiceIds = I'.undefined
        alterResource = I'.undefined
        addEndPoint = I'.undefined
        removeEndPoint = I'.undefined
        addServiceInstance = I'.undefined

instance MonitorPlatformService' a => Sub' (Obj' a)
         MonitorPlatformService where
        up' = MonitorPlatformService

instance Sub' MonitorPlatformService PlatformService where
        up' (MonitorPlatformService x') = PlatformService x'

{-# LINE 758 "initial\FRH.abs" #-}
class MonitoringService' a where
        {-# LINE 759 "initial\FRH.abs" #-}
        addMS :: Rule -> Obj' a -> ABS' Unit
        
        {-# LINE 760 "initial\FRH.abs" #-}
        removeMS :: Rule -> Obj' a -> ABS' Unit

data MonitoringService = forall a . MonitoringService' a =>
                           MonitoringService (Obj' a)

instance I'.Show MonitoringService where
        show _ = "MonitoringService"

instance I'.Eq MonitoringService where
        MonitoringService (Obj' ref1' _ _) ==
          MonitoringService (Obj' ref2' _ _) = ref1' == I'.unsafeCoerce ref2'

instance MonitoringService' Null' where
        addMS = I'.undefined
        removeMS = I'.undefined

instance MonitoringService' a => Sub' (Obj' a) MonitoringService
         where
        up' = MonitoringService

{-# LINE 763 "initial\FRH.abs" #-}
class ServiceProvider' a where
        {-# LINE 764 "initial\FRH.abs" #-}
        addCustomer :: Config -> Customer -> Obj' a -> ABS' EndPoint
        
        {-# LINE 765 "initial\FRH.abs" #-}
        removeCustomer :: Config -> Customer -> Obj' a -> ABS' Unit

data ServiceProvider = forall a . ServiceProvider' a =>
                         ServiceProvider (Obj' a)

instance I'.Show ServiceProvider where
        show _ = "ServiceProvider"

instance I'.Eq ServiceProvider where
        ServiceProvider (Obj' ref1' _ _) ==
          ServiceProvider (Obj' ref2' _ _) = ref1' == I'.unsafeCoerce ref2'

instance ServiceProvider' Null' where
        addCustomer = I'.undefined
        removeCustomer = I'.undefined

instance ServiceProvider' a => Sub' (Obj' a) ServiceProvider where
        up' = ServiceProvider

{-# LINE 768 "initial\FRH.abs" #-}
class Item' a

data Item = forall a . Item' a => Item (Obj' a)

instance I'.Show Item where
        show _ = "Item"

instance I'.Eq Item where
        Item (Obj' ref1' _ _) == Item (Obj' ref2' _ _)
          = ref1' == I'.unsafeCoerce ref2'

instance Item' Null'

instance Item' a => Sub' (Obj' a) Item where
        up' = Item

{-# LINE 772 "initial\FRH.abs" #-}
class Service' a => IQueryService' a where
        {-# LINE 773 "initial\FRH.abs" #-}
        doQuery :: String -> Obj' a -> ABS' (List Item)

data IQueryService = forall a . IQueryService' a =>
                       IQueryService (Obj' a)

instance I'.Show IQueryService where
        show _ = "IQueryService"

instance I'.Eq IQueryService where
        IQueryService (Obj' ref1' _ _) == IQueryService (Obj' ref2' _ _)
          = ref1' == I'.unsafeCoerce ref2'

instance IQueryService' Null' where
        doQuery = I'.undefined

instance IQueryService' a => Sub' (Obj' a) IQueryService where
        up' = IQueryService

instance Sub' IQueryService EndPoint where
        up' (IQueryService x') = EndPoint x'

instance Sub' IQueryService Service where
        up' (IQueryService x') = Service x'

{-# LINE 776 "initial\FRH.abs" #-}
class EndPoint' a => MonitoringQueryEndpoint' a where
        {-# LINE 777 "initial\FRH.abs" #-}
        invokeWithDelay ::
                        Int -> Customer -> Int -> Int -> Obj' a -> ABS' Unit

data MonitoringQueryEndpoint = forall a .
                                 MonitoringQueryEndpoint' a => MonitoringQueryEndpoint (Obj' a)

instance I'.Show MonitoringQueryEndpoint where
        show _ = "MonitoringQueryEndpoint"

instance I'.Eq MonitoringQueryEndpoint where
        MonitoringQueryEndpoint (Obj' ref1' _ _) ==
          MonitoringQueryEndpoint (Obj' ref2' _ _)
          = ref1' == I'.unsafeCoerce ref2'

instance MonitoringQueryEndpoint' Null' where
        invokeWithDelay = I'.undefined

instance MonitoringQueryEndpoint' a => Sub' (Obj' a)
         MonitoringQueryEndpoint where
        up' = MonitoringQueryEndpoint

instance Sub' MonitoringQueryEndpoint EndPoint where
        up' (MonitoringQueryEndpoint x') = EndPoint x'

{-# LINE 780 "initial\FRH.abs" #-}
class UtilityFunctions' a where
        {-# LINE 781 "initial\FRH.abs" #-}
        getdc_instance_names ::
                             List DeploymentComponent -> Obj' a -> ABS' (List String)

data UtilityFunctions = forall a . UtilityFunctions' a =>
                          UtilityFunctions (Obj' a)

instance I'.Show UtilityFunctions where
        show _ = "UtilityFunctions"

instance I'.Eq UtilityFunctions where
        UtilityFunctions (Obj' ref1' _ _) ==
          UtilityFunctions (Obj' ref2' _ _) = ref1' == I'.unsafeCoerce ref2'

instance UtilityFunctions' Null' where
        getdc_instance_names = I'.undefined

instance UtilityFunctions' a => Sub' (Obj' a) UtilityFunctions
         where
        up' = UtilityFunctions

{-# LINE 788 "initial\FRH.abs" #-}
data DeployerImpl = DeployerImpl{}

smart'DeployerImpl :: DeployerImpl
smart'DeployerImpl = (DeployerImpl)

init'DeployerImpl :: Obj' DeployerImpl -> I'.IO ()
{-# LINE 788 "initial\FRH.abs" #-}
init'DeployerImpl this@(Obj' this' _ thisDC) = I'.pure ()

instance DeployerIF' DeployerImpl where
        scaleUp this@(Obj' this' _ thisDC) = I'.pure ()
        scaleDown this@(Obj' this' _ thisDC) = I'.pure ()

{-# LINE 793 "initial\FRH.abs" #-}
data InfrastructureServiceImpl = InfrastructureServiceImpl{cp'InfrastructureServiceImpl
                                                           :: CloudProvider,
                                                           inUse'InfrastructureServiceImpl ::
                                                           Map Id DeploymentComponent,
                                                           total'InfrastructureServiceImpl :: Int}

smart'InfrastructureServiceImpl ::
                                CloudProvider -> InfrastructureServiceImpl
smart'InfrastructureServiceImpl cp'this
  = (\ total'this ->
       (\ inUse'this ->
          (InfrastructureServiceImpl cp'this inUse'this
             (I'.fromIntegral total'this)))
         (EmptyMap :: Map Id DeploymentComponent))
      (0 :: Int)

init'InfrastructureServiceImpl ::
                               Obj' InfrastructureServiceImpl -> I'.IO ()
{-# LINE 793 "initial\FRH.abs" #-}
init'InfrastructureServiceImpl this@(Obj' this' _ thisDC)
  = I'.pure ()

instance InfrastructureService' InfrastructureServiceImpl where
        acquireInstance id vmType this@(Obj' this' _ thisDC)
          = do vm :: IORef' DeploymentComponent <- I'.lift
                                                     (I'.newIORef (up' null))
               resourceConfig :: IORef' ResourceCapacities <- I'.lift
                                                                (I'.newIORef (vmResources vmType))
               md :: IORef' (Maybe DeploymentComponent) <- I'.lift
                                                             ((\ this'' ->
                                                                 I'.newIORef
                                                                   (lookup
                                                                      (inUse'InfrastructureServiceImpl
                                                                         this'')
                                                                      id))
                                                                =<< I'.readIORef this')
               case' <- I'.lift (I'.readIORef md)
               case case' of
                   Nothing -> do tmp1619402333 ::
                                   IORef' (Fut DeploymentComponent) <- I'.lift
                                                                         ((\ this'' ->
                                                                             I'.newIORef =<<
                                                                               ((\ (CloudProvider
                                                                                      obj')
                                                                                   ->
                                                                                   (obj' <!>
                                                                                      launchInstanceNamed
                                                                                        (toString
                                                                                           vmType)))
                                                                                  (cp'InfrastructureServiceImpl
                                                                                     this'')))
                                                                            =<< I'.readIORef this')
                                 awaitFuture' this =<< I'.lift (I'.readIORef tmp1619402333)
                                 I'.lift (I'.writeIORef vm =<< (get =<< I'.readIORef tmp1619402333))
                                 I'.lift
                                   (I'.writeIORef this' =<<
                                      ((\ this'' ->
                                          (\ v' -> this''{inUse'InfrastructureServiceImpl = v'})
                                            <$!>
                                            (I'.pure InsertAssoc <*>
                                               ((,) <$!> I'.pure id <*> (up' <$!> I'.readIORef vm))
                                               <*>
                                               I'.pure (inUse'InfrastructureServiceImpl this'')))
                                         =<< I'.readIORef this'))
                   Just d -> do I'.lift (I'.writeIORef vm d)
               I'.lift ((up' <$!> I'.readIORef vm))
        release component this@(Obj' this' _ thisDC)
          = do id :: IORef' (Maybe Int) <- I'.lift
                                             ((\ this'' ->
                                                 I'.newIORef
                                                   (mapValue
                                                      (inUse'InfrastructureServiceImpl this'')
                                                      (up' component)))
                                                =<< I'.readIORef this')
               I'.lift
                 ((\ b' -> assert b' (I'.pure ())) =<<
                    ((not) <$!> ((==) <$!> I'.readIORef id <*> I'.pure Nothing)))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{inUse'InfrastructureServiceImpl = v'}) <$!>
                          (I'.pure removeKey <*>
                             I'.pure (inUse'InfrastructureServiceImpl this'')
                             <*> (I'.pure fromJust <*> I'.readIORef id)))
                       =<< I'.readIORef this'))
               cpu :: IORef' Int <- (I'.lift . I'.newIORef) =<<
                                      (this <..> cpu''InfrastructureServiceImpl (up' component))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{total'InfrastructureServiceImpl = v'}) <$!>
                          ((-) <$!>
                             I'.pure (I'.fromIntegral (total'InfrastructureServiceImpl this''))
                             <*> (I'.fromIntegral <$!> I'.readIORef cpu)))
                       =<< I'.readIORef this'))

cpu''InfrastructureServiceImpl ::
                               DeploymentComponent -> Obj' InfrastructureServiceImpl -> ABS' Int
cpu''InfrastructureServiceImpl dc this@(Obj' this' _ thisDC)
  = do cf :: IORef' (Fut InfRat) <- I'.lift
                                      (I'.newIORef =<<
                                         ((\ (DeploymentComponent obj') -> (obj' <!> total Speed))
                                            dc))
       cpu :: IORef' InfRat <- I'.lift
                                 (I'.newIORef =<< (get =<< I'.readIORef cf))
       I'.lift
         ((I'.pure truncate <*> (I'.pure finvalue <*> I'.readIORef cpu)))

{-# LINE 829 "initial\FRH.abs" #-}
data LoadBalancerEndPointImpl = LoadBalancerEndPointImpl{current'LoadBalancerEndPointImpl
                                                         :: List Service,
                                                         log'LoadBalancerEndPointImpl :: Int,
                                                         services'LoadBalancerEndPointImpl ::
                                                         List Service,
                                                         state'LoadBalancerEndPointImpl :: State}

smart'LoadBalancerEndPointImpl :: LoadBalancerEndPointImpl
smart'LoadBalancerEndPointImpl
  = (\ log'this ->
       (\ state'this ->
          (\ services'this ->
             (\ current'this ->
                (LoadBalancerEndPointImpl current'this (I'.fromIntegral log'this)
                   services'this
                   state'this))
               ([] :: List Service))
            ([] :: List Service))
         (STOP :: State))
      (0 :: Int)

init'LoadBalancerEndPointImpl ::
                              Obj' LoadBalancerEndPointImpl -> I'.IO ()
{-# LINE 829 "initial\FRH.abs" #-}
init'LoadBalancerEndPointImpl this@(Obj' this' _ thisDC)
  = do (\ this'' ->
          assert (unique (services'LoadBalancerEndPointImpl this''))
            (I'.pure ()))
         =<< I'.readIORef this'

instance LoadBalancerEndPoint' LoadBalancerEndPointImpl where
        removeLBE service this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' ->
                     assert ((length (services'LoadBalancerEndPointImpl this'')) > 1)
                       (I'.pure ()))
                    =<< I'.readIORef this')
               result :: IORef' Bool <- I'.lift (I'.newIORef False)
               (\ this'' ->
                  I'.when
                    (inList (services'LoadBalancerEndPointImpl this'') (up' service))
                    (do I'.lift
                          (I'.writeIORef this' =<<
                             ((\ this'' ->
                                 this''{services'LoadBalancerEndPointImpl =
                                          (removeFirst (services'LoadBalancerEndPointImpl this'')
                                             (up' service))})
                                <$!> I'.readIORef this'))
                        I'.lift
                          (I'.writeIORef this' =<<
                             ((\ this'' ->
                                 this''{current'LoadBalancerEndPointImpl =
                                          (removeFirst (current'LoadBalancerEndPointImpl this'')
                                             (up' service))})
                                <$!> I'.readIORef this'))
                        I'.lift (I'.writeIORef result True)))
                 =<< I'.lift (I'.readIORef this')
               I'.lift (I'.readIORef result)
        addLBE service this@(Obj' this' _ thisDC)
          = do result :: IORef' Bool <- I'.lift (I'.newIORef False)
               (\ this'' ->
                  I'.when
                    (not
                       (inList (services'LoadBalancerEndPointImpl this'') (up' service)))
                    (do I'.lift
                          (I'.writeIORef this' =<<
                             ((\ this'' ->
                                 this''{services'LoadBalancerEndPointImpl =
                                          ((up' service) :
                                             (services'LoadBalancerEndPointImpl this''))})
                                <$!> I'.readIORef this'))
                        I'.lift
                          (I'.writeIORef this' =<<
                             ((\ this'' ->
                                 this''{current'LoadBalancerEndPointImpl =
                                          ((up' service) :
                                             (services'LoadBalancerEndPointImpl this''))})
                                <$!> I'.readIORef this'))
                        I'.lift (I'.writeIORef result True)))
                 =<< I'.lift (I'.readIORef this')
               I'.lift (I'.readIORef result)
        getServices this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' -> I'.pure (current'LoadBalancerEndPointImpl this''))
                    =<< I'.readIORef this')

instance EndPoint' LoadBalancerEndPointImpl where
        invoke request this@(Obj' this' _ thisDC)
          = do I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        this''{log'LoadBalancerEndPointImpl =
                                 ((I'.fromIntegral (log'LoadBalancerEndPointImpl this'')) + 1)})
                       <$!> I'.readIORef this'))
               I'.lift
                 ((\ this'' ->
                     assert ((state'LoadBalancerEndPointImpl this'') == RUNNING)
                       (I'.pure ()))
                    =<< I'.readIORef this')
               (\ this'' ->
                  I'.when ((current'LoadBalancerEndPointImpl this'') == [])
                    (do I'.lift
                          (I'.writeIORef this' =<<
                             ((\ this'' ->
                                 this''{current'LoadBalancerEndPointImpl =
                                          (services'LoadBalancerEndPointImpl this'')})
                                <$!> I'.readIORef this'))))
                 =<< I'.lift (I'.readIORef this')
               p :: IORef' EndPoint <- I'.lift
                                         ((\ this'' ->
                                             I'.newIORef
                                               (up' (head (current'LoadBalancerEndPointImpl this''))))
                                            =<< I'.readIORef this')
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        this''{current'LoadBalancerEndPointImpl =
                                 (tail (current'LoadBalancerEndPointImpl this''))})
                       <$!> I'.readIORef this'))
               tmp296831715 :: IORef' (Fut Bool) <- I'.lift
                                                      (I'.newIORef =<<
                                                         ((\ (EndPoint obj') ->
                                                             (obj' <!> invoke request))
                                                            =<< I'.readIORef p))
               awaitFuture' this =<< I'.lift (I'.readIORef tmp296831715)
               I'.lift (get =<< I'.readIORef tmp296831715)
        setStatus status this@(Obj' this' _ thisDC)
          = do I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' -> this''{state'LoadBalancerEndPointImpl = status}) <$!>
                       I'.readIORef this'))
        getStatus this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' -> I'.pure (state'LoadBalancerEndPointImpl this'')) =<<
                    I'.readIORef this')

{-# LINE 880 "initial\FRH.abs" #-}
data ServiceImpl = ServiceImpl{c'ServiceImpl :: Customer,
                               cost'ServiceImpl :: Int, latency'ServiceImpl :: Int,
                               log'ServiceImpl :: Int, serviceId'ServiceImpl :: Id,
                               st'ServiceImpl :: ServiceType, state'ServiceImpl :: State}

smart'ServiceImpl ::
                  Id -> ServiceType -> Customer -> Int -> ServiceImpl
smart'ServiceImpl serviceId'this st'this c'this cost'this
  = (\ latency'this ->
       (\ log'this ->
          (\ state'this ->
             (ServiceImpl c'this (I'.fromIntegral cost'this)
                (I'.fromIntegral latency'this)
                (I'.fromIntegral log'this)
                serviceId'this
                st'this
                state'this))
            (STOP :: State))
         (0 :: Int))
      (0 :: Int)

init'ServiceImpl :: Obj' ServiceImpl -> I'.IO ()
{-# LINE 880 "initial\FRH.abs" #-}
init'ServiceImpl this@(Obj' this' _ thisDC) = I'.pure ()

instance Service' ServiceImpl where
        getServiceId this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' -> I'.pure (serviceId'ServiceImpl this'')) =<<
                    I'.readIORef this')
        setServiceId id this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' ->
                     assert ((serviceId'ServiceImpl this'') == 0) (I'.pure ()))
                    =<< I'.readIORef this')
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' -> this''{serviceId'ServiceImpl = id}) <$!>
                       I'.readIORef this'))
        getServiceType this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' -> I'.pure (st'ServiceImpl this'')) =<<
                    I'.readIORef this')
        getCustomer this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' -> I'.pure (c'ServiceImpl this'')) =<<
                    I'.readIORef this')
        getLatency this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' ->
                     I'.pure (I'.fromIntegral (latency'ServiceImpl this'')))
                    =<< I'.readIORef this')
        getRequestCount this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' -> I'.pure (I'.fromIntegral (log'ServiceImpl this'')))
                    =<< I'.readIORef this')
        getCPU this@(Obj' this' _ thisDC)
          = do dc :: IORef' DeploymentComponent <- I'.lift
                                                     (I'.newIORef thisDC)
               fdt :: IORef' (Fut InfRat) <- I'.lift
                                               (I'.newIORef =<<
                                                  ((\ (DeploymentComponent obj') ->
                                                      (obj' <!> total Speed))
                                                     =<< I'.readIORef dc))
               dt :: IORef' InfRat <- I'.lift
                                        (I'.newIORef =<< (get =<< I'.readIORef fdt))
               I'.lift
                 ((I'.pure truncate <*> (I'.pure finvalue <*> I'.readIORef dt)))
        getBandwidth this@(Obj' this' _ thisDC)
          = do dc :: IORef' DeploymentComponent <- I'.lift
                                                     (I'.newIORef thisDC)
               fdt :: IORef' (Fut InfRat) <- I'.lift
                                               (I'.newIORef =<<
                                                  ((\ (DeploymentComponent obj') ->
                                                      (obj' <!> total Bandwidth))
                                                     =<< I'.readIORef dc))
               dt :: IORef' InfRat <- I'.lift
                                        (I'.newIORef =<< (get =<< I'.readIORef fdt))
               I'.lift
                 ((I'.pure truncate <*> (I'.pure finvalue <*> I'.readIORef dt)))
        getMemory this@(Obj' this' _ thisDC)
          = do dc :: IORef' DeploymentComponent <- I'.lift
                                                     (I'.newIORef thisDC)
               fdt :: IORef' (Fut InfRat) <- I'.lift
                                               (I'.newIORef =<<
                                                  ((\ (DeploymentComponent obj') ->
                                                      (obj' <!> total Memory))
                                                     =<< I'.readIORef dc))
               dt :: IORef' InfRat <- I'.lift
                                        (I'.newIORef =<< (get =<< I'.readIORef fdt))
               I'.lift
                 ((I'.pure truncate <*> (I'.pure finvalue <*> I'.readIORef dt)))
        getResource t this@(Obj' this' _ thisDC)
          = do dc :: IORef' DeploymentComponent <- I'.lift
                                                     (I'.newIORef thisDC)
               fdt :: IORef' (Fut InfRat) <- I'.lift
                                               (I'.newIORef =<<
                                                  ((\ (DeploymentComponent obj') ->
                                                      (obj' <!> total t))
                                                     =<< I'.readIORef dc))
               dt :: IORef' InfRat <- I'.lift
                                        (I'.newIORef =<< (get =<< I'.readIORef fdt))
               I'.lift (I'.readIORef dt)

instance EndPoint' ServiceImpl where
        invoke request this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' ->
                     assert ((state'ServiceImpl this'') == RUNNING) (I'.pure ()))
                    =<< I'.readIORef this')
               cost :: IORef' Int <- I'.lift (I'.newIORef (cost request))
               time :: IORef' Rat <- I'.lift (I'.newIORef =<< currentms)
               (\ (DeploymentComponent obj') ->
                  awaitSugar'' this obj' =<<
                    I'.lift
                      (I'.pure request__ <*> (I'.fromIntegral <$!> I'.readIORef cost)))
                 thisDC
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        this''{log'ServiceImpl =
                                 ((I'.fromIntegral (log'ServiceImpl this'')) + 1)})
                       <$!> I'.readIORef this'))
               time_ :: IORef' Rat <- I'.lift (I'.newIORef =<< currentms)
               I'.lift
                 (I'.writeIORef time =<<
                    ((-) <$!> I'.readIORef time_ <*> I'.readIORef time))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{latency'ServiceImpl = v'}) <$!>
                          (I'.pure max <*>
                             I'.pure (I'.fromIntegral (latency'ServiceImpl this''))
                             <*> (I'.pure truncate <*> I'.readIORef time)))
                       =<< I'.readIORef this'))
               I'.lift (I'.pure (success))
        setStatus state_ this@(Obj' this' _ thisDC)
          = do I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' -> this''{state'ServiceImpl = state_}) <$!>
                       I'.readIORef this'))
        getStatus this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' -> I'.pure (state'ServiceImpl this'')) =<<
                    I'.readIORef this')

{-# LINE 948 "initial\FRH.abs" #-}
data DeploymentAgentImpl = DeploymentAgentImpl{service'DeploymentAgentImpl
                                               :: Service}

smart'DeploymentAgentImpl :: DeploymentAgentImpl
smart'DeploymentAgentImpl
  = (\ service'this -> (DeploymentAgentImpl (up' service'this)))
      ((up' null) :: Service)

init'DeploymentAgentImpl :: Obj' DeploymentAgentImpl -> I'.IO ()
{-# LINE 948 "initial\FRH.abs" #-}
init'DeploymentAgentImpl this@(Obj' this' _ thisDC) = I'.pure ()

instance DeploymentAgent' DeploymentAgentImpl where
        installDA s this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' ->
                     assert (Service null == (up' (service'DeploymentAgentImpl this'')))
                       (I'.pure ()))
                    =<< I'.readIORef this')
               I'.lift (assert (not (Service null == (up' s))) (I'.pure ()))
               tmp935704808 :: IORef' (Fut State) <- I'.lift
                                                       (I'.newIORef =<<
                                                          ((\ (Service obj') ->
                                                              (obj' <!> getStatus))
                                                             s))
               awaitFuture' this =<< I'.lift (I'.readIORef tmp935704808)
               state :: IORef' State <- I'.lift
                                          (I'.newIORef =<< (get =<< I'.readIORef tmp935704808))
               I'.lift
                 ((\ b' -> assert b' (I'.pure ())) =<<
                    ((==) <$!> I'.readIORef state <*> I'.pure STOP))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' -> this''{service'DeploymentAgentImpl = (up' s)}) <$!>
                       I'.readIORef this'))
        uninstallDA this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' ->
                     assert
                       (not (Service null == (up' (service'DeploymentAgentImpl this''))))
                       (I'.pure ()))
                    =<< I'.readIORef this')
               tmp1938106144 :: IORef' (Fut State) <- I'.lift
                                                        ((\ this'' ->
                                                            I'.newIORef =<<
                                                              ((\ (Service obj') ->
                                                                  (obj' <!> getStatus))
                                                                 (service'DeploymentAgentImpl
                                                                    this'')))
                                                           =<< I'.readIORef this')
               awaitFuture' this =<< I'.lift (I'.readIORef tmp1938106144)
               state :: IORef' State <- I'.lift
                                          (I'.newIORef =<< (get =<< I'.readIORef tmp1938106144))
               I'.lift
                 ((\ b' -> assert b' (I'.pure ())) =<<
                    ((==) <$!> I'.readIORef state <*> I'.pure STOP))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' -> this''{service'DeploymentAgentImpl = (up' null)})
                       <$!> I'.readIORef this'))
        startDA this@(Obj' this' _ thisDC)
          = do tmp1409058575 :: IORef' (Fut Unit) <- I'.lift
                                                       ((\ this'' ->
                                                           I'.newIORef =<<
                                                             ((\ (Service obj') ->
                                                                 (obj' <!> setStatus RUNNING))
                                                                (service'DeploymentAgentImpl
                                                                   this'')))
                                                          =<< I'.readIORef this')
               awaitFuture' this =<< I'.lift (I'.readIORef tmp1409058575)
               _ <- I'.lift (get =<< I'.readIORef tmp1409058575)
               I'.pure ()
        stopDA this@(Obj' this' _ thisDC)
          = do tmp1691696909 :: IORef' (Fut Unit) <- I'.lift
                                                       ((\ this'' ->
                                                           I'.newIORef =<<
                                                             ((\ (Service obj') ->
                                                                 (obj' <!> setStatus STOP))
                                                                (service'DeploymentAgentImpl
                                                                   this'')))
                                                          =<< I'.readIORef this')
               awaitFuture' this =<< I'.lift (I'.readIORef tmp1691696909)
               _ <- I'.lift (get =<< I'.readIORef tmp1691696909)
               I'.pure ()
        getServiceDA this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' -> I'.pure (up' (service'DeploymentAgentImpl this'')))
                    =<< I'.readIORef this')

{-# LINE 984 "initial\FRH.abs" #-}
data DeploymentServiceImpl = DeploymentServiceImpl{allocations'DeploymentServiceImpl
                                                   :: Map Service DeploymentAgent,
                                                   services'DeploymentServiceImpl :: Map Id Service}

smart'DeploymentServiceImpl :: DeploymentServiceImpl
smart'DeploymentServiceImpl
  = (\ allocations'this ->
       (\ services'this ->
          (DeploymentServiceImpl allocations'this services'this))
         (EmptyMap :: Map Id Service))
      (EmptyMap :: Map Service DeploymentAgent)

init'DeploymentServiceImpl ::
                           Obj' DeploymentServiceImpl -> I'.IO ()
{-# LINE 984 "initial\FRH.abs" #-}
init'DeploymentServiceImpl this@(Obj' this' _ thisDC) = I'.pure ()

instance DeploymentService' DeploymentServiceImpl where
        installDS customer st serviceId v this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' ->
                     assert
                       ((lookup (services'DeploymentServiceImpl this'') serviceId) ==
                          Nothing)
                       (I'.pure ()))
                    =<< I'.readIORef this')
               service :: IORef' Service <- I'.lift
                                              (((I'.newIORef . Service) =<<
                                                  new thisDC init'ServiceImpl
                                                    (smart'ServiceImpl serviceId st customer 2)))
               agent :: IORef' DeploymentAgent <- I'.lift
                                                    (((I'.newIORef . DeploymentAgent) =<<
                                                        new thisDC init'DeploymentAgentImpl
                                                          smart'DeploymentAgentImpl))
               tmp1193797116 :: IORef' (Fut Unit) <- I'.lift
                                                       (I'.newIORef =<<
                                                          ((\ (DeploymentAgent obj') ->
                                                              (obj' <!>) =<<
                                                                I'.pure installDA <*>
                                                                  (up' <$!> I'.readIORef service))
                                                             =<< I'.readIORef agent))
               awaitFuture' this =<< I'.lift (I'.readIORef tmp1193797116)
               _ <- I'.lift (get =<< I'.readIORef tmp1193797116)
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{allocations'DeploymentServiceImpl = v'}) <$!>
                          (I'.pure InsertAssoc <*>
                             ((,) <$!> (up' <$!> I'.readIORef service) <*>
                                (up' <$!> I'.readIORef agent))
                             <*> I'.pure (allocations'DeploymentServiceImpl this'')))
                       =<< I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{services'DeploymentServiceImpl = v'}) <$!>
                          (I'.pure InsertAssoc <*>
                             ((,) <$!> I'.pure serviceId <*> (up' <$!> I'.readIORef service))
                             <*> I'.pure (services'DeploymentServiceImpl this'')))
                       =<< I'.readIORef this'))
               I'.lift ((up' <$!> I'.readIORef service))
        uninstallDS serviceId this@(Obj' this' _ thisDC)
          = do service :: IORef' Service <- (I'.lift . I'.newIORef) =<<
                                              (this <..> lookup''DeploymentServiceImpl serviceId)
               tmp1240537166 :: IORef' (Fut State) <- I'.lift
                                                        (I'.newIORef =<<
                                                           ((\ (Service obj') ->
                                                               (obj' <!> getStatus))
                                                              =<< I'.readIORef service))
               awaitFuture' this =<< I'.lift (I'.readIORef tmp1240537166)
               state :: IORef' State <- I'.lift
                                          (I'.newIORef =<< (get =<< I'.readIORef tmp1240537166))
               I'.lift
                 ((\ b' -> assert b' (I'.pure ())) =<<
                    ((==) <$!> I'.readIORef state <*> I'.pure STOP))
               mAgent :: IORef' (Maybe DeploymentAgent) <- I'.lift
                                                             (I'.newIORef =<<
                                                                (\ this'' ->
                                                                   (I'.pure lookup <*>
                                                                      I'.pure
                                                                        (allocations'DeploymentServiceImpl
                                                                           this'')
                                                                      <*>
                                                                      (up' <$!>
                                                                         I'.readIORef service)))
                                                                  =<< I'.readIORef this')
               I'.lift
                 ((\ b' -> assert b' (I'.pure ())) =<<
                    ((not) <$!> ((==) <$!> I'.readIORef mAgent <*> I'.pure Nothing)))
               agent :: IORef' DeploymentAgent <- I'.lift
                                                    (I'.newIORef =<<
                                                       (I'.pure fromJust <*> I'.readIORef mAgent))
               tmp1638969247 :: IORef' (Fut Unit) <- I'.lift
                                                       (I'.newIORef =<<
                                                          ((\ (DeploymentAgent obj') ->
                                                              (obj' <!> uninstallDA))
                                                             =<< I'.readIORef agent))
               awaitFuture' this =<< I'.lift (I'.readIORef tmp1638969247)
               _ <- I'.lift (get =<< I'.readIORef tmp1638969247)
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{allocations'DeploymentServiceImpl = v'}) <$!>
                          (I'.pure removeKey <*>
                             I'.pure (allocations'DeploymentServiceImpl this'')
                             <*> (up' <$!> I'.readIORef service)))
                       =<< I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        this''{services'DeploymentServiceImpl =
                                 (removeKey (services'DeploymentServiceImpl this'') serviceId)})
                       <$!> I'.readIORef this'))
        startDS serviceId this@(Obj' this' _ thisDC)
          = do _ <- this <..> invoke''DeploymentServiceImpl serviceId RUNNING
               I'.pure ()
        stopDS serviceId this@(Obj' this' _ thisDC)
          = do _ <- this <..> invoke''DeploymentServiceImpl serviceId STOP
               I'.pure ()
        addDS agent this@(Obj' this' _ thisDC)
          = do tmp908802124 :: IORef' (Fut Service) <- I'.lift
                                                         (I'.newIORef =<<
                                                            ((\ (DeploymentAgent obj') ->
                                                                (obj' <!> getServiceDA))
                                                               agent))
               awaitFuture' this =<< I'.lift (I'.readIORef tmp908802124)
               s :: IORef' Service <- I'.lift
                                        (I'.newIORef =<< (get =<< I'.readIORef tmp908802124))
               tmp433699465 :: IORef' (Fut Int) <- I'.lift
                                                     (I'.newIORef =<<
                                                        ((\ (Service obj') ->
                                                            (obj' <!> getServiceId))
                                                           =<< I'.readIORef s))
               awaitFuture' this =<< I'.lift (I'.readIORef tmp433699465)
               serviceId :: IORef' Id <- I'.lift
                                           (I'.newIORef =<< (get =<< I'.readIORef tmp433699465))
               I'.lift
                 ((\ b' -> assert b' (I'.pure ())) =<<
                    ((not) <$!> ((==) <$!> I'.readIORef serviceId <*> I'.pure 0)))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{allocations'DeploymentServiceImpl = v'}) <$!>
                          (I'.pure InsertAssoc <*>
                             ((,) <$!> (up' <$!> I'.readIORef s) <*> I'.pure (up' agent))
                             <*> I'.pure (allocations'DeploymentServiceImpl this'')))
                       =<< I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{services'DeploymentServiceImpl = v'}) <$!>
                          (I'.pure InsertAssoc <*>
                             ((,) <$!> I'.readIORef serviceId <*> (up' <$!> I'.readIORef s))
                             <*> I'.pure (services'DeploymentServiceImpl this'')))
                       =<< I'.readIORef this'))
               _ <- (this <..>) =<<
                      I'.lift (I'.pure startDS <*> I'.readIORef serviceId)
               I'.pure ()

invoke''DeploymentServiceImpl ::
                              Id -> State -> Obj' DeploymentServiceImpl -> ABS' Unit
invoke''DeploymentServiceImpl serviceId state
  this@(Obj' this' _ thisDC)
  = do service :: IORef' Service <- (I'.lift . I'.newIORef) =<<
                                      (this <..> lookup''DeploymentServiceImpl serviceId)
       tmp1937294500 :: IORef' (Fut Unit) <- I'.lift
                                               (I'.newIORef =<<
                                                  ((\ (Service obj') -> (obj' <!> setStatus state))
                                                     =<< I'.readIORef service))
       awaitFuture' this =<< I'.lift (I'.readIORef tmp1937294500)
       _ <- I'.lift (get =<< I'.readIORef tmp1937294500)
       I'.pure ()

lookup''DeploymentServiceImpl ::
                              Id -> Obj' DeploymentServiceImpl -> ABS' Service
lookup''DeploymentServiceImpl id this@(Obj' this' _ thisDC)
  = do s :: IORef' (Maybe Service) <- I'.lift
                                        ((\ this'' ->
                                            I'.newIORef
                                              (lookup (services'DeploymentServiceImpl this'') id))
                                           =<< I'.readIORef this')
       I'.lift
         ((\ b' -> assert b' (I'.pure ())) =<<
            ((not) <$!> ((==) <$!> I'.readIORef s <*> I'.pure Nothing)))
       I'.lift ((I'.pure fromJust <*> I'.readIORef s))

{-# LINE 1047 "initial\FRH.abs" #-}
data LoadBalancerServiceImpl = LoadBalancerServiceImpl{endPoints'LoadBalancerServiceImpl
                                                       :: Map Int LoadBalancerEndPoint}

smart'LoadBalancerServiceImpl :: LoadBalancerServiceImpl
smart'LoadBalancerServiceImpl
  = (\ endPoints'this -> (LoadBalancerServiceImpl endPoints'this))
      (EmptyMap :: Map Int LoadBalancerEndPoint)

init'LoadBalancerServiceImpl ::
                             Obj' LoadBalancerServiceImpl -> I'.IO ()
{-# LINE 1047 "initial\FRH.abs" #-}
init'LoadBalancerServiceImpl this@(Obj' this' _ thisDC)
  = I'.pure ()

instance LoadBalancerService' LoadBalancerServiceImpl where
        disable id this@(Obj' this' _ thisDC)
          = do this <..> status''LoadBalancerServiceImpl id STOP
        enable id this@(Obj' this' _ thisDC)
          = do this <..> status''LoadBalancerServiceImpl id RUNNING
        create services_ endPointId this@(Obj' this' _ thisDC)
          = do added :: IORef' Bool <- I'.lift (I'.newIORef False)
               services :: IORef' (List Service) <- I'.lift
                                                      (I'.newIORef services_)
               mep :: IORef' (Maybe EndPoint) <- I'.lift
                                                   ((\ this'' ->
                                                       I'.newIORef
                                                         (I'.fmap up' (lookup
                                                            (endPoints'LoadBalancerServiceImpl
                                                               this'')
                                                            (I'.fromIntegral endPointId))))
                                                      =<< I'.readIORef this')
               when' <- I'.lift (((==) <$!> I'.readIORef mep <*> I'.pure Nothing))
               I'.when when'
                 (do ep :: IORef' LoadBalancerEndPoint <- I'.lift
                                                            (((I'.newIORef . LoadBalancerEndPoint)
                                                                =<<
                                                                new thisDC
                                                                  init'LoadBalancerEndPointImpl
                                                                  smart'LoadBalancerEndPointImpl))
                     while ((not) <$!> ((==) <$!> I'.readIORef services <*> I'.pure []))
                       (do s :: IORef' Service <- I'.lift
                                                    (I'.newIORef =<<
                                                       (I'.pure head <*> I'.readIORef services))
                           tmp1839130458 :: IORef' (Fut Bool) <- I'.lift
                                                                   (I'.newIORef =<<
                                                                      ((\ (LoadBalancerEndPoint
                                                                             obj')
                                                                          ->
                                                                          (obj' <!>) =<<
                                                                            I'.pure addLBE <*>
                                                                              (up' <$!>
                                                                                 I'.readIORef s))
                                                                         =<< I'.readIORef ep))
                           awaitFuture' this =<< I'.lift (I'.readIORef tmp1839130458)
                           _ <- I'.lift (get =<< I'.readIORef tmp1839130458)
                           I'.lift
                             (I'.writeIORef services =<<
                                (I'.pure tail <*> I'.readIORef services)))
                     I'.lift
                       (I'.writeIORef this' =<<
                          ((\ this'' ->
                              (\ v' -> this''{endPoints'LoadBalancerServiceImpl = v'}) <$!>
                                (I'.pure put <*> I'.pure (endPoints'LoadBalancerServiceImpl this'')
                                   <*> I'.pure (I'.fromIntegral endPointId)
                                   <*> (up' <$!> I'.readIORef ep)))
                             =<< I'.readIORef this'))
                     I'.lift (I'.writeIORef added True))
               I'.lift (I'.readIORef added)
        addLBS endPointId ep this@(Obj' this' _ thisDC)
          = do added :: IORef' Bool <- I'.lift (I'.newIORef False)
               mep :: IORef' (Maybe EndPoint) <- I'.lift
                                                   ((\ this'' ->
                                                       I'.newIORef
                                                         (I'.fmap up' (lookup
                                                            (endPoints'LoadBalancerServiceImpl
                                                               this'')
                                                            (I'.fromIntegral endPointId))))
                                                      =<< I'.readIORef this')
               when' <- I'.lift (((==) <$!> I'.readIORef mep <*> I'.pure Nothing))
               I'.when when'
                 (do I'.lift
                       (I'.writeIORef this' =<<
                          ((\ this'' ->
                              this''{endPoints'LoadBalancerServiceImpl =
                                       (put (endPoints'LoadBalancerServiceImpl this'')
                                          (I'.fromIntegral endPointId)
                                          (up' ep))})
                             <$!> I'.readIORef this'))
                     I'.lift (I'.writeIORef added True))
               I'.lift (I'.readIORef added)
        removeLBS endPointId this@(Obj' this' _ thisDC)
          = do removed :: IORef' Bool <- I'.lift (I'.newIORef False)
               mep :: IORef' (Maybe EndPoint) <- I'.lift
                                                   ((\ this'' ->
                                                       I'.newIORef
                                                         (I'.fmap up' (lookup
                                                            (endPoints'LoadBalancerServiceImpl
                                                               this'')
                                                            endPointId)))
                                                      =<< I'.readIORef this')
               when' <- I'.lift
                          (((not) <$!> ((==) <$!> I'.readIORef mep <*> I'.pure Nothing)))
               I'.when when'
                 (do point :: IORef' EndPoint <- I'.lift
                                                   (I'.newIORef =<<
                                                      (I'.pure fromJust <*> I'.readIORef mep))
                     tmp1199303255 :: IORef' (Fut State) <- I'.lift
                                                              (I'.newIORef =<<
                                                                 ((\ (EndPoint obj') ->
                                                                     (obj' <!> getStatus))
                                                                    =<< I'.readIORef point))
                     awaitFuture' this =<< I'.lift (I'.readIORef tmp1199303255)
                     state :: IORef' State <- I'.lift
                                                (I'.newIORef =<<
                                                   (get =<< I'.readIORef tmp1199303255))
                     I'.lift
                       ((\ b' -> assert b' (I'.pure ())) =<<
                          ((==) <$!> I'.readIORef state <*> I'.pure STOP))
                     I'.lift
                       (I'.writeIORef this' =<<
                          ((\ this'' ->
                              this''{endPoints'LoadBalancerServiceImpl =
                                       (removeKey (endPoints'LoadBalancerServiceImpl this'')
                                          endPointId)})
                             <$!> I'.readIORef this'))
                     I'.lift (I'.writeIORef removed True))
               I'.lift (I'.readIORef removed)
        getEndPointId lb this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' ->
                     I'.pure
                       (mapValue (endPoints'LoadBalancerServiceImpl this'') (up' lb)))
                    =<< I'.readIORef this')
        getEndPointById endPointId this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' ->
                     I'.pure
                       (lookup (endPoints'LoadBalancerServiceImpl this'') endPointId))
                    =<< I'.readIORef this')
        getEndPointIdsByService s this@(Obj' this' _ thisDC)
          = do endPointsToCheck ::
                 IORef' (List LoadBalancerEndPoint) <- I'.lift
                                                         ((\ this'' ->
                                                             I'.newIORef
                                                               (values
                                                                  (endPoints'LoadBalancerServiceImpl
                                                                     this'')))
                                                            =<< I'.readIORef this')
               foundEndPoints :: IORef' (List Id) <- I'.lift (I'.newIORef [])
               while
                 ((not) <$!>
                    ((==) <$!> I'.readIORef endPointsToCheck <*> I'.pure []))
                 (do lb :: IORef' LoadBalancerEndPoint <- I'.lift
                                                            (I'.newIORef =<<
                                                               (I'.pure head <*>
                                                                  I'.readIORef endPointsToCheck))
                     tmp379407034 :: IORef' (Fut (List Service)) <- I'.lift
                                                                      (I'.newIORef =<<
                                                                         ((\ (LoadBalancerEndPoint
                                                                                obj')
                                                                             ->
                                                                             (obj' <!> getServices))
                                                                            =<< I'.readIORef lb))
                     awaitFuture' this =<< I'.lift (I'.readIORef tmp379407034)
                     services :: IORef' (List Service) <- I'.lift
                                                            (I'.newIORef =<<
                                                               (get =<< I'.readIORef tmp379407034))
                     when' <- I'.lift
                                ((I'.pure inList <*> I'.readIORef services <*> I'.pure (up' s)))
                     I'.when when'
                       (do mep :: IORef' (Maybe Id) <- I'.lift
                                                         (I'.newIORef =<<
                                                            (\ this'' ->
                                                               (I'.pure mapValue <*>
                                                                  I'.pure
                                                                    (endPoints'LoadBalancerServiceImpl
                                                                       this'')
                                                                  <*> (up' <$!> I'.readIORef lb)))
                                                              =<< I'.readIORef this')
                           case' <- I'.lift (I'.readIORef mep)
                           case case' of
                               Nothing -> do I'.lift (I'.writeIORef foundEndPoints [])
                               Just id -> do I'.lift
                                               (I'.writeIORef foundEndPoints =<<
                                                  ((:) <$!> I'.pure id <*>
                                                     I'.readIORef foundEndPoints)))
                     I'.lift
                       (I'.writeIORef endPointsToCheck =<<
                          (I'.pure tail <*> I'.readIORef endPointsToCheck)))
               I'.lift (I'.readIORef foundEndPoints)
        decrease endPointId services this@(Obj' this' _ thisDC)
          = do result :: IORef' Bool <- (I'.lift . I'.newIORef) =<<
                                          (this <..>
                                             change''LoadBalancerServiceImpl endPointId services
                                               DECR)
               I'.lift (I'.readIORef result)
        increase endPointId services this@(Obj' this' _ thisDC)
          = do result :: IORef' Bool <- (I'.lift . I'.newIORef) =<<
                                          (this <..>
                                             change''LoadBalancerServiceImpl endPointId services
                                               INCR)
               I'.lift (I'.readIORef result)

change''LoadBalancerServiceImpl ::
                                Id ->
                                  List Service -> LBOp -> Obj' LoadBalancerServiceImpl -> ABS' Bool
change''LoadBalancerServiceImpl endPointId services_ op
  this@(Obj' this' _ thisDC)
  = do result :: IORef' Bool <- I'.lift (I'.newIORef False)
       services :: IORef' (List Service) <- I'.lift
                                              (I'.newIORef services_)
       ep :: IORef' (Maybe LoadBalancerEndPoint) <- I'.lift
                                                      ((\ this'' ->
                                                          I'.newIORef
                                                            (lookup
                                                               (endPoints'LoadBalancerServiceImpl
                                                                  this'')
                                                               endPointId))
                                                         =<< I'.readIORef this')
       when' <- I'.lift
                  (((not) <$!> ((==) <$!> I'.readIORef ep <*> I'.pure Nothing)))
       I'.when when'
         (do endPoint :: IORef' LoadBalancerEndPoint <- I'.lift
                                                          (I'.newIORef =<<
                                                             (I'.pure fromJust <*> I'.readIORef ep))
             while ((not) <$!> ((==) <$!> I'.readIORef services <*> I'.pure []))
               (do fb :: IORef' (Fut Bool) <- I'.lift (I'.newIORef nullFuture')
                   if (op == INCR) then
                     do I'.lift
                          (I'.writeIORef fb =<<
                             ((\ (LoadBalancerEndPoint obj') ->
                                 (obj' <!>) =<<
                                   I'.pure addLBE <*> (I'.pure head <*> I'.readIORef services))
                                =<< I'.readIORef endPoint))
                     else
                     do I'.lift
                          (I'.writeIORef fb =<<
                             ((\ (LoadBalancerEndPoint obj') ->
                                 (obj' <!>) =<<
                                   I'.pure removeLBE <*> (I'.pure head <*> I'.readIORef services))
                                =<< I'.readIORef endPoint))
                   b :: IORef' Bool <- I'.lift
                                         (I'.newIORef =<< (get =<< I'.readIORef fb))
                   I'.lift
                     (I'.writeIORef result =<<
                        ((||) <$!> I'.readIORef result <*> I'.readIORef b))
                   I'.lift
                     (I'.writeIORef services =<<
                        (I'.pure tail <*> I'.readIORef services))))
       I'.lift (I'.readIORef result)

status''LoadBalancerServiceImpl ::
                                Id -> State -> Obj' LoadBalancerServiceImpl -> ABS' Bool
status''LoadBalancerServiceImpl id state this@(Obj' this' _ thisDC)
  = do mep :: IORef' (Maybe EndPoint) <- I'.lift
                                           ((\ this'' ->
                                               I'.newIORef
                                                 (I'.fmap up' (lookup (endPoints'LoadBalancerServiceImpl this'')
                                                    id)))
                                              =<< I'.readIORef this')
       success :: IORef' Bool <- I'.lift (I'.newIORef False)
       when' <- I'.lift
                  (((not) <$!> ((==) <$!> I'.readIORef mep <*> I'.pure Nothing)))
       I'.when when'
         (do point :: IORef' EndPoint <- I'.lift
                                           (I'.newIORef =<< (I'.pure fromJust <*> I'.readIORef mep))
             tmp1838342951 :: IORef' (Fut Unit) <- I'.lift
                                                     (I'.newIORef =<<
                                                        ((\ (EndPoint obj') ->
                                                            (obj' <!> setStatus state))
                                                           =<< I'.readIORef point))
             awaitFuture' this =<< I'.lift (I'.readIORef tmp1838342951)
             _ <- I'.lift (get =<< I'.readIORef tmp1838342951)
             I'.lift (I'.writeIORef success True))
       I'.lift (I'.readIORef success)

{-# LINE 1169 "initial\FRH.abs" #-}
data PlatformServiceImpl = PlatformServiceImpl{customers'PlatformServiceImpl
                                               :: Map Customer (Map Config Id),
                                               ds'PlatformServiceImpl :: DeploymentService,
                                               endPoints'PlatformServiceImpl :: Map Id (List Id),
                                               ls'PlatformServiceImpl :: LoadBalancerService,
                                               serviceId'PlatformServiceImpl :: Id,
                                               serviceToEndPoints'PlatformServiceImpl :: Map Id Int,
                                               services'PlatformServiceImpl :: Map Id Service}

smart'PlatformServiceImpl ::
                          DeploymentService -> LoadBalancerService -> PlatformServiceImpl
smart'PlatformServiceImpl ds'this ls'this
  = (\ services'this ->
       (\ serviceToEndPoints'this ->
          (\ endPoints'this ->
             (\ customers'this ->
                (\ serviceId'this ->
                   (PlatformServiceImpl customers'this (up' ds'this) endPoints'this
                      (up' ls'this)
                      serviceId'this
                      serviceToEndPoints'this
                      services'this))
                  ((init) :: Id))
               (EmptyMap :: Map Customer (Map Config Id)))
            (EmptyMap :: Map Id (List Id)))
         (EmptyMap :: Map Id Int))
      (EmptyMap :: Map Id Service)

init'PlatformServiceImpl :: Obj' PlatformServiceImpl -> I'.IO ()
{-# LINE 1169 "initial\FRH.abs" #-}
init'PlatformServiceImpl this@(Obj' this' _ thisDC) = I'.pure ()

instance MonitorPlatformService' PlatformServiceImpl where
        incrService endPoint instances this@(Obj' this' _ thisDC)
          = do I'.lift (assert (not (instances == [])) (I'.pure ()))
               setting :: IORef' (Maybe (Pair Customer Config)) <- I'.lift
                                                                     ((\ this'' ->
                                                                         I'.newIORef
                                                                           (keyPairs
                                                                              (customers'PlatformServiceImpl
                                                                                 this'')
                                                                              endPoint))
                                                                        =<< I'.readIORef this')
               I'.lift
                 ((\ b' -> assert b' (I'.pure ())) =<<
                    ((not) <$!> ((==) <$!> I'.readIORef setting <*> I'.pure Nothing)))
               customer :: IORef' Customer <- I'.lift
                                                (I'.newIORef =<<
                                                   (I'.pure fst <*>
                                                      (I'.pure fromJust <*> I'.readIORef setting)))
               oldC :: IORef' Config <- I'.lift
                                          (I'.newIORef =<<
                                             (I'.pure snd <*>
                                                (I'.pure fromJust <*> I'.readIORef setting)))
               st :: IORef' ServiceType <- I'.lift
                                             (I'.newIORef =<<
                                                (I'.pure serviceType <*> I'.readIORef oldC))
               es :: IORef' (List Service) <- I'.lift (I'.newIORef [])
               ids :: IORef' (List Id) <- I'.lift (I'.newIORef [])
               remaining :: IORef' (List ResourceCapacities) <- I'.lift
                                                                  (I'.newIORef instances)
               while
                 ((not) <$!> ((==) <$!> I'.readIORef remaining <*> I'.pure []))
                 (do res :: IORef' ResourceCapacities <- I'.lift
                                                           (I'.newIORef =<<
                                                              (I'.pure head <*>
                                                                 I'.readIORef remaining))
                     service :: IORef' Service <- (I'.lift . I'.newIORef) =<<
                                                    ((this <..>) =<<
                                                       I'.lift
                                                         (I'.pure
                                                            createServiceInstance''PlatformServiceImpl
                                                            <*> I'.readIORef customer
                                                            <*> I'.readIORef st
                                                            <*> I'.readIORef res))
                     idf :: IORef' (Fut Int) <- I'.lift
                                                  (I'.newIORef =<<
                                                     ((\ (Service obj') -> (obj' <!> getServiceId))
                                                        =<< I'.readIORef service))
                     id :: IORef' Id <- I'.lift
                                          (I'.newIORef =<< (get =<< I'.readIORef idf))
                     I'.lift
                       (I'.writeIORef this' =<<
                          ((\ this'' ->
                              (\ v' -> this''{serviceToEndPoints'PlatformServiceImpl = v'}) <$!>
                                (I'.pure InsertAssoc <*>
                                   ((,) <$!> I'.readIORef id <*> I'.pure endPoint)
                                   <*> I'.pure (serviceToEndPoints'PlatformServiceImpl this'')))
                             =<< I'.readIORef this'))
                     I'.lift
                       (I'.writeIORef ids =<<
                          ((:) <$!> I'.readIORef id <*> I'.readIORef ids))
                     I'.lift
                       (I'.writeIORef es =<<
                          ((:) <$!> (up' <$!> I'.readIORef service) <*> I'.readIORef es))
                     I'.lift
                       (I'.writeIORef remaining =<<
                          (I'.pure tail <*> I'.readIORef remaining)))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{endPoints'PlatformServiceImpl = v'}) <$!>
                          (I'.pure put <*> I'.pure (endPoints'PlatformServiceImpl this'') <*>
                             I'.pure endPoint
                             <*>
                             (I'.pure concatenate <*> I'.readIORef ids <*>
                                (I'.pure lookupDefault <*>
                                   I'.pure (endPoints'PlatformServiceImpl this'')
                                   <*> I'.pure endPoint
                                   <*> I'.pure []))))
                       =<< I'.readIORef this'))
               fb :: IORef' (Fut Bool) <- I'.lift
                                            ((\ this'' ->
                                                I'.newIORef =<<
                                                  ((\ (LoadBalancerService obj') ->
                                                      (obj' <!>) =<<
                                                        I'.pure increase <*> I'.pure endPoint <*>
                                                          I'.readIORef es)
                                                     (ls'PlatformServiceImpl this'')))
                                               =<< I'.readIORef this')
               increased :: IORef' Bool <- I'.lift
                                             (I'.newIORef =<< (get =<< I'.readIORef fb))
               I'.lift
                 ((\ b' -> assert b' (I'.pure ())) =<< I'.readIORef increased)
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{customers'PlatformServiceImpl = v'}) <$!>
                          (I'.pure updateConfig <*>
                             I'.pure (customers'PlatformServiceImpl this'')
                             <*> I'.readIORef customer
                             <*> I'.readIORef oldC
                             <*>
                             (I'.pure Config_ <*> I'.readIORef st <*>
                                (I'.pure concatenate <*> I'.pure instances <*>
                                   (I'.pure instances_ <*> I'.readIORef oldC)))))
                       =<< I'.readIORef this'))
        decrService endPoint serviceIds this@(Obj' this' _ thisDC)
          = do setting :: IORef' (Maybe (Pair Customer Config)) <- I'.lift
                                                                     ((\ this'' ->
                                                                         I'.newIORef
                                                                           (keyPairs
                                                                              (customers'PlatformServiceImpl
                                                                                 this'')
                                                                              endPoint))
                                                                        =<< I'.readIORef this')
               I'.lift
                 ((\ b' -> assert b' (I'.pure ())) =<<
                    ((not) <$!> ((==) <$!> I'.readIORef setting <*> I'.pure Nothing)))
               customer :: IORef' Customer <- I'.lift
                                                (I'.newIORef =<<
                                                   (I'.pure fst <*>
                                                      (I'.pure fromJust <*> I'.readIORef setting)))
               oldC :: IORef' Config <- I'.lift
                                          (I'.newIORef =<<
                                             (I'.pure snd <*>
                                                (I'.pure fromJust <*> I'.readIORef setting)))
               st :: IORef' ServiceType <- I'.lift
                                             (I'.newIORef =<<
                                                (I'.pure serviceType <*> I'.readIORef oldC))
               msers :: IORef' (Maybe (List Id)) <- I'.lift
                                                      ((\ this'' ->
                                                          I'.newIORef
                                                            (lookup
                                                               (endPoints'PlatformServiceImpl
                                                                  this'')
                                                               endPoint))
                                                         =<< I'.readIORef this')
               I'.lift
                 ((\ b' -> assert b' (I'.pure ())) =<<
                    ((not) <$!> ((==) <$!> I'.readIORef msers <*> I'.pure Nothing)))
               sers :: IORef' (List Id) <- I'.lift
                                             (I'.newIORef =<<
                                                (I'.pure fromJust <*> I'.readIORef msers))
               I'.lift
                 ((\ b' -> assert b' (I'.pure ())) =<<
                    (I'.pure inListAll <*> I'.readIORef sers <*> I'.pure serviceIds))
               deployed :: IORef' (List Service) <- I'.lift (I'.newIORef [])
               remains :: IORef' (List Id) <- I'.lift (I'.newIORef serviceIds)
               while ((not) <$!> ((==) <$!> I'.readIORef remains <*> I'.pure []))
                 (do serviceId :: IORef' Id <- I'.lift
                                                 (I'.newIORef =<<
                                                    (I'.pure head <*> I'.readIORef remains))
                     mdeployed :: IORef' (Maybe Service) <- I'.lift
                                                              (I'.newIORef =<<
                                                                 (\ this'' ->
                                                                    (I'.pure lookup <*>
                                                                       I'.pure
                                                                         (services'PlatformServiceImpl
                                                                            this'')
                                                                       <*> I'.readIORef serviceId))
                                                                   =<< I'.readIORef this')
                     I'.lift
                       ((\ b' -> assert b' (I'.pure ())) =<<
                          ((not) <$!>
                             ((==) <$!> I'.readIORef mdeployed <*> I'.pure Nothing)))
                     I'.lift
                       (I'.writeIORef deployed =<<
                          ((:) <$!> (I'.pure fromJust <*> I'.readIORef mdeployed) <*>
                             I'.readIORef deployed))
                     I'.lift
                       (I'.writeIORef remains =<<
                          (I'.pure tail <*> I'.readIORef remains)))
               fb :: IORef' (Fut Bool) <- I'.lift
                                            ((\ this'' ->
                                                I'.newIORef =<<
                                                  ((\ (LoadBalancerService obj') ->
                                                      (obj' <!>) =<<
                                                        I'.pure decrease <*> I'.pure endPoint <*>
                                                          I'.readIORef deployed)
                                                     (ls'PlatformServiceImpl this'')))
                                               =<< I'.readIORef this')
               decreased :: IORef' Bool <- I'.lift
                                             (I'.newIORef =<< (get =<< I'.readIORef fb))
               I'.lift
                 ((\ b' -> assert b' (I'.pure ())) =<< I'.readIORef decreased)
               I'.lift (I'.writeIORef remains serviceIds)
               current :: IORef' (List ResourceCapacities) <- I'.lift
                                                                (I'.newIORef =<<
                                                                   (I'.pure instances_ <*>
                                                                      I'.readIORef oldC))
               while ((not) <$!> ((==) <$!> I'.readIORef remains <*> I'.pure []))
                 (do serviceId :: IORef' Id <- I'.lift
                                                 (I'.newIORef =<<
                                                    (I'.pure head <*> I'.readIORef remains))
                     sf :: IORef' (Fut Unit) <- I'.lift
                                                  ((\ this'' ->
                                                      I'.newIORef =<<
                                                        ((\ (DeploymentService obj') ->
                                                            (obj' <!>) =<<
                                                              I'.pure stopDS <*>
                                                                I'.readIORef serviceId)
                                                           (ds'PlatformServiceImpl this'')))
                                                     =<< I'.readIORef this')
                     _ <- I'.lift (get =<< I'.readIORef sf)
                     I'.lift
                       ((\ this'' ->
                           I'.writeIORef sf =<<
                             ((\ (DeploymentService obj') ->
                                 (obj' <!>) =<< I'.pure uninstallDS <*> I'.readIORef serviceId)
                                (ds'PlatformServiceImpl this'')))
                          =<< I'.readIORef this')
                     _ <- I'.lift (get =<< I'.readIORef sf)
                     I'.lift
                       (I'.writeIORef this' =<<
                          ((\ this'' ->
                              (\ v' -> this''{serviceToEndPoints'PlatformServiceImpl = v'}) <$!>
                                (I'.pure removeKey <*>
                                   I'.pure (serviceToEndPoints'PlatformServiceImpl this'')
                                   <*> I'.readIORef serviceId))
                             =<< I'.readIORef this'))
                     I'.lift
                       (I'.writeIORef this' =<<
                          ((\ this'' ->
                              (\ v' -> this''{services'PlatformServiceImpl = v'}) <$!>
                                (I'.pure removeKey <*>
                                   I'.pure (services'PlatformServiceImpl this'')
                                   <*> I'.readIORef serviceId))
                             =<< I'.readIORef this'))
                     I'.lift
                       (I'.writeIORef remains =<< (I'.pure tail <*> I'.readIORef remains))
                     I'.lift
                       (I'.writeIORef current =<<
                          (I'.pure tail <*> I'.readIORef current)))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        this''{endPoints'PlatformServiceImpl =
                                 (put (endPoints'PlatformServiceImpl this'') endPoint
                                    (difference_
                                       (lookupDefault (endPoints'PlatformServiceImpl this'')
                                          endPoint
                                          [])
                                       serviceIds))})
                       <$!> I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{customers'PlatformServiceImpl = v'}) <$!>
                          (I'.pure updateConfig <*>
                             I'.pure (customers'PlatformServiceImpl this'')
                             <*> I'.readIORef customer
                             <*> I'.readIORef oldC
                             <*>
                             (I'.pure Config_ <*> I'.readIORef st <*> I'.readIORef current)))
                       =<< I'.readIORef this'))
        getEndPoints this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' ->
                     I'.pure (toList (keys (endPoints'PlatformServiceImpl this''))))
                    =<< I'.readIORef this')
        getServiceMPS serviceId this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' ->
                     I'.pure (lookup (services'PlatformServiceImpl this'') serviceId))
                    =<< I'.readIORef this')
        getServiceIds endPoint this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' ->
                     I'.pure
                       (lookupDefault (endPoints'PlatformServiceImpl this'') endPoint []))
                    =<< I'.readIORef this')
        alterResource serviceId t r this@(Obj' this' _ thisDC)
          = do mservice :: IORef' (Maybe Service) <- I'.lift
                                                       ((\ this'' ->
                                                           I'.newIORef
                                                             (lookup
                                                                (services'PlatformServiceImpl
                                                                   this'')
                                                                serviceId))
                                                          =<< I'.readIORef this')
               I'.lift
                 ((\ b' -> assert b' (I'.pure ())) =<<
                    ((not) <$!> ((==) <$!> I'.readIORef mservice <*> I'.pure Nothing)))
               ser :: IORef' Service <- I'.lift
                                          (I'.newIORef =<<
                                             (I'.pure fromJust <*> I'.readIORef mservice))
               fOldCapacity :: IORef' (Fut InfRat) <- I'.lift
                                                        (I'.newIORef =<<
                                                           ((\ (Service obj') ->
                                                               (obj' <!> getResource t))
                                                              =<< I'.readIORef ser))
               oldCapacity :: IORef' InfRat <- I'.lift
                                                 (I'.newIORef =<<
                                                    (get =<< I'.readIORef fOldCapacity))
               I'.lift
                 ((\ b' -> assert b' (I'.pure ())) =<<
                    ((not) <$!>
                       ((==) <$!> I'.readIORef oldCapacity <*> I'.pure InfRat)))
               newCapacity :: IORef' Rat <- I'.lift (I'.newIORef 0)
               case' <- I'.lift (I'.readIORef oldCapacity)
               case case' of
                   Fin v -> do I'.lift (I'.writeIORef newCapacity (v + r))
               req :: IORef' ResourceCapacities <- I'.lift
                                                     (I'.newIORef =<<
                                                        (I'.pure map <*>
                                                           ((:) <$!>
                                                              ((,) <$!> I'.pure t <*>
                                                                 I'.readIORef newCapacity)
                                                              <*> I'.pure [])))
               mendPoint :: IORef' (Maybe Int) <- I'.lift
                                                    ((\ this'' ->
                                                        I'.newIORef
                                                          (lookup
                                                             (serviceToEndPoints'PlatformServiceImpl
                                                                this'')
                                                             serviceId))
                                                       =<< I'.readIORef this')
               I'.lift
                 ((\ b' -> assert b' (I'.pure ())) =<<
                    ((not) <$!>
                       ((==) <$!> I'.readIORef mendPoint <*> I'.pure Nothing)))
               endPoint :: IORef' Int <- I'.lift
                                           (I'.newIORef =<<
                                              (I'.pure fromJust <*> I'.readIORef mendPoint))
               instances :: IORef' (List Int) <- I'.lift
                                                   (I'.newIORef =<<
                                                      (\ this'' ->
                                                         (I'.pure lookupDefault <*>
                                                            I'.pure
                                                              (endPoints'PlatformServiceImpl this'')
                                                            <*>
                                                            (I'.fromIntegral <$!>
                                                               I'.readIORef endPoint)
                                                            <*> I'.pure []))
                                                        =<< I'.readIORef this')
               fc :: IORef' (Fut Customer) <- I'.lift
                                                (I'.newIORef =<<
                                                   ((\ (Service obj') -> (obj' <!> getCustomer)) =<<
                                                      I'.readIORef ser))
               customer :: IORef' Customer <- I'.lift
                                                (I'.newIORef =<< (get =<< I'.readIORef fc))
               fs :: IORef' (Fut ServiceType) <- I'.lift
                                                   (I'.newIORef =<<
                                                      ((\ (Service obj') ->
                                                          (obj' <!> getServiceType))
                                                         =<< I'.readIORef ser))
               st :: IORef' ServiceType <- I'.lift
                                             (I'.newIORef =<< (get =<< I'.readIORef fs))
               newSer :: IORef' Service <- (I'.lift . I'.newIORef) =<<
                                             ((this <..>) =<<
                                                I'.lift
                                                  (I'.pure
                                                     createServiceInstance''PlatformServiceImpl
                                                     <*> I'.readIORef customer
                                                     <*> I'.readIORef st
                                                     <*> I'.readIORef req))
               idf :: IORef' (Fut Int) <- I'.lift
                                            (I'.newIORef =<<
                                               ((\ (Service obj') -> (obj' <!> getServiceId)) =<<
                                                  I'.readIORef newSer))
               newId :: IORef' Int <- I'.lift
                                        (I'.newIORef =<< (get =<< I'.readIORef idf))
               fb :: IORef' (Fut Bool) <- I'.lift
                                            ((\ this'' ->
                                                I'.newIORef =<<
                                                  ((\ (LoadBalancerService obj') ->
                                                      (obj' <!>) =<<
                                                        I'.pure increase <*>
                                                          (I'.fromIntegral <$!>
                                                             I'.readIORef endPoint)
                                                          <*>
                                                          (I'.pure list <*>
                                                             ((:) <$!>
                                                                (up' <$!> I'.readIORef newSer)
                                                                <*> I'.pure [])))
                                                     (ls'PlatformServiceImpl this'')))
                                               =<< I'.readIORef this')
               increased :: IORef' Bool <- I'.lift
                                             (I'.newIORef =<< (get =<< I'.readIORef fb))
               I'.lift
                 ((\ b' -> assert b' (I'.pure ())) =<< I'.readIORef increased)
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{serviceToEndPoints'PlatformServiceImpl = v'}) <$!>
                          (I'.pure InsertAssoc <*>
                             ((,) <$!> (I'.fromIntegral <$!> I'.readIORef newId) <*>
                                (I'.fromIntegral <$!> I'.readIORef endPoint))
                             <*> I'.pure (serviceToEndPoints'PlatformServiceImpl this'')))
                       =<< I'.readIORef this'))
               allInstances :: IORef' (List Int) <- I'.lift
                                                      (I'.newIORef =<<
                                                         (\ this'' ->
                                                            ((:) <$!>
                                                               (I'.fromIntegral <$!>
                                                                  I'.readIORef newId)
                                                               <*>
                                                               (I'.pure lookupDefault <*>
                                                                  I'.pure
                                                                    (endPoints'PlatformServiceImpl
                                                                       this'')
                                                                  <*>
                                                                  (I'.fromIntegral <$!>
                                                                     I'.readIORef endPoint)
                                                                  <*> I'.pure [])))
                                                           =<< I'.readIORef this')
               _ <- (this <..>) =<<
                      I'.lift
                        (I'.pure uninstallInstance''PlatformServiceImpl <*>
                           (I'.fromIntegral <$!> I'.readIORef endPoint)
                           <*> (up' <$!> I'.readIORef ser)
                           <*> I'.pure serviceId)
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        this''{serviceToEndPoints'PlatformServiceImpl =
                                 (removeKey (serviceToEndPoints'PlatformServiceImpl this'')
                                    serviceId)})
                       <$!> I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        this''{services'PlatformServiceImpl =
                                 (removeKey (services'PlatformServiceImpl this'') serviceId)})
                       <$!> I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{endPoints'PlatformServiceImpl = v'}) <$!>
                          (I'.pure put <*> I'.pure (endPoints'PlatformServiceImpl this'') <*>
                             (I'.fromIntegral <$!> I'.readIORef endPoint)
                             <*>
                             (I'.pure without <*> I'.readIORef allInstances <*>
                                I'.pure serviceId)))
                       =<< I'.readIORef this'))
        addEndPoint lb this@(Obj' this' _ thisDC)
          = do I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        this''{serviceId'PlatformServiceImpl =
                                 (incr (serviceId'PlatformServiceImpl this''))})
                       <$!> I'.readIORef this'))
               endPoint :: IORef' Int <- I'.lift
                                           ((\ this'' ->
                                               I'.newIORef (serviceId'PlatformServiceImpl this''))
                                              =<< I'.readIORef this')
               tmp948268398 :: IORef' (Fut (List Service)) <- I'.lift
                                                                (I'.newIORef =<<
                                                                   ((\ (LoadBalancerEndPoint obj')
                                                                       -> (obj' <!> getServices))
                                                                      lb))
               awaitFuture' this =<< I'.lift (I'.readIORef tmp948268398)
               connectedServices :: IORef' (List Service) <- I'.lift
                                                               (I'.newIORef =<<
                                                                  (get =<<
                                                                     I'.readIORef tmp948268398))
               remaining :: IORef' (List Service) <- I'.lift
                                                       (I'.newIORef =<<
                                                          I'.readIORef connectedServices)
               while
                 ((not) <$!> ((==) <$!> I'.readIORef remaining <*> I'.pure []))
                 (do s :: IORef' Service <- I'.lift
                                              (I'.newIORef =<<
                                                 (I'.pure head <*> I'.readIORef remaining))
                     tmp1792172929 :: IORef' (Fut Int) <- I'.lift
                                                            (I'.newIORef =<<
                                                               ((\ (Service obj') ->
                                                                   (obj' <!> getServiceId))
                                                                  =<< I'.readIORef s))
                     awaitFuture' this =<< I'.lift (I'.readIORef tmp1792172929)
                     serviceId :: IORef' Id <- I'.lift
                                                 (I'.newIORef =<<
                                                    (get =<< I'.readIORef tmp1792172929))
                     I'.lift
                       (I'.writeIORef this' =<<
                          ((\ this'' ->
                              (\ v' -> this''{serviceToEndPoints'PlatformServiceImpl = v'}) <$!>
                                (I'.pure put <*>
                                   I'.pure (serviceToEndPoints'PlatformServiceImpl this'')
                                   <*> I'.readIORef serviceId
                                   <*> (I'.fromIntegral <$!> I'.readIORef endPoint)))
                             =<< I'.readIORef this'))
                     connectedIds :: IORef' (List Id) <- I'.lift
                                                           (I'.newIORef =<<
                                                              (\ this'' ->
                                                                 (I'.pure lookupDefault <*>
                                                                    I'.pure
                                                                      (endPoints'PlatformServiceImpl
                                                                         this'')
                                                                    <*>
                                                                    (I'.fromIntegral <$!>
                                                                       I'.readIORef endPoint)
                                                                    <*> I'.pure []))
                                                                =<< I'.readIORef this')
                     I'.lift
                       (I'.writeIORef connectedIds =<<
                          ((:) <$!> I'.readIORef serviceId <*> I'.readIORef connectedIds))
                     I'.lift
                       (I'.writeIORef this' =<<
                          ((\ this'' ->
                              (\ v' -> this''{endPoints'PlatformServiceImpl = v'}) <$!>
                                (I'.pure put <*> I'.pure (endPoints'PlatformServiceImpl this'') <*>
                                   (I'.fromIntegral <$!> I'.readIORef endPoint)
                                   <*> I'.readIORef connectedIds))
                             =<< I'.readIORef this')))
               tmp636002577 :: IORef' (Fut Bool) <- I'.lift
                                                      ((\ this'' ->
                                                          I'.newIORef =<<
                                                            ((\ (LoadBalancerService obj') ->
                                                                (obj' <!>) =<<
                                                                  I'.pure addLBS <*>
                                                                    (I'.fromIntegral <$!>
                                                                       I'.readIORef endPoint)
                                                                    <*> I'.pure (up' lb))
                                                               (ls'PlatformServiceImpl this'')))
                                                         =<< I'.readIORef this')
               awaitFuture' this =<< I'.lift (I'.readIORef tmp636002577)
               _ <- I'.lift (get =<< I'.readIORef tmp636002577)
               tmp97281693 :: IORef' (Fut Bool) <- I'.lift
                                                     ((\ this'' ->
                                                         I'.newIORef =<<
                                                           ((\ (LoadBalancerService obj') ->
                                                               (obj' <!>) =<<
                                                                 I'.pure enable <*>
                                                                   (I'.fromIntegral <$!>
                                                                      I'.readIORef endPoint))
                                                              (ls'PlatformServiceImpl this'')))
                                                        =<< I'.readIORef this')
               awaitFuture' this =<< I'.lift (I'.readIORef tmp97281693)
               _ <- I'.lift (get =<< I'.readIORef tmp97281693)
               I'.lift ((I'.fromIntegral <$!> I'.readIORef endPoint))
        removeEndPoint lb this@(Obj' this' _ thisDC)
          = do I'.lift (I'.pure 0)
        addServiceInstance s this@(Obj' this' _ thisDC)
          = do I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        this''{serviceId'PlatformServiceImpl =
                                 (incr (serviceId'PlatformServiceImpl this''))})
                       <$!> I'.readIORef this'))
               id :: IORef' Id <- I'.lift
                                    ((\ this'' ->
                                        I'.newIORef (serviceId'PlatformServiceImpl this''))
                                       =<< I'.readIORef this')
               tmp305646302 :: IORef' (Fut Unit) <- I'.lift
                                                      (I'.newIORef =<<
                                                         ((\ (Service obj') ->
                                                             (obj' <!>) =<<
                                                               I'.pure setServiceId <*>
                                                                 I'.readIORef id)
                                                            s))
               awaitFuture' this =<< I'.lift (I'.readIORef tmp305646302)
               _ <- I'.lift (get =<< I'.readIORef tmp305646302)
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{services'PlatformServiceImpl = v'}) <$!>
                          (I'.pure put <*> I'.pure (services'PlatformServiceImpl this'') <*>
                             I'.readIORef id
                             <*> I'.pure (up' s)))
                       =<< I'.readIORef this'))
               tmp268620232 :: IORef' (Fut (List Int)) <- I'.lift
                                                            ((\ this'' ->
                                                                I'.newIORef =<<
                                                                  ((\ (LoadBalancerService obj') ->
                                                                      (obj' <!>
                                                                         getEndPointIdsByService
                                                                           (up' s)))
                                                                     (ls'PlatformServiceImpl
                                                                        this'')))
                                                               =<< I'.readIORef this')
               awaitFuture' this =<< I'.lift (I'.readIORef tmp268620232)
               onEndPoints :: IORef' (List Id) <- I'.lift
                                                    (I'.newIORef =<<
                                                       (get =<< I'.readIORef tmp268620232))
               endPointsToProcess :: IORef' (List Id) <- I'.lift
                                                           (I'.newIORef =<<
                                                              I'.readIORef onEndPoints)
               while
                 ((not) <$!>
                    ((==) <$!> I'.readIORef endPointsToProcess <*> I'.pure []))
                 (do endPointId :: IORef' Id <- I'.lift
                                                  (I'.newIORef =<<
                                                     (I'.pure head <*>
                                                        I'.readIORef endPointsToProcess))
                     I'.lift
                       (I'.writeIORef this' =<<
                          ((\ this'' ->
                              (\ v' -> this''{serviceToEndPoints'PlatformServiceImpl = v'}) <$!>
                                (I'.pure InsertAssoc <*>
                                   ((,) <$!> I'.readIORef id <*> I'.readIORef endPointId)
                                   <*> I'.pure (serviceToEndPoints'PlatformServiceImpl this'')))
                             =<< I'.readIORef this'))
                     mSerEndpoints :: IORef' (Maybe (List Id)) <- I'.lift
                                                                    (I'.newIORef =<<
                                                                       (\ this'' ->
                                                                          (I'.pure lookup <*>
                                                                             I'.pure
                                                                               (endPoints'PlatformServiceImpl
                                                                                  this'')
                                                                             <*>
                                                                             I'.readIORef
                                                                               endPointId))
                                                                         =<< I'.readIORef this')
                     case' <- I'.lift (I'.readIORef mSerEndpoints)
                     case case' of
                         Nothing -> do I'.lift
                                         (I'.writeIORef this' =<<
                                            ((\ this'' ->
                                                (\ v' -> this''{endPoints'PlatformServiceImpl = v'})
                                                  <$!>
                                                  (I'.pure InsertAssoc <*>
                                                     ((,) <$!> I'.readIORef endPointId <*>
                                                        (I'.pure list <*>
                                                           ((:) <$!> I'.readIORef id <*>
                                                              I'.pure [])))
                                                     <*>
                                                     I'.pure
                                                       (endPoints'PlatformServiceImpl this'')))
                                               =<< I'.readIORef this'))
                         Just l -> do I'.lift
                                        (I'.writeIORef this' =<<
                                           ((\ this'' ->
                                               (\ v' -> this''{endPoints'PlatformServiceImpl = v'})
                                                 <$!>
                                                 (I'.pure InsertAssoc <*>
                                                    ((,) <$!> I'.readIORef endPointId <*>
                                                       ((:) <$!> I'.readIORef id <*> I'.pure l))
                                                    <*>
                                                    I'.pure (endPoints'PlatformServiceImpl this'')))
                                              =<< I'.readIORef this'))
                     I'.lift
                       (I'.writeIORef endPointsToProcess =<<
                          (I'.pure tail <*> I'.readIORef endPointsToProcess)))
               I'.lift
                 ((\ this'' -> I'.pure (serviceId'PlatformServiceImpl this'')) =<<
                    I'.readIORef this')

instance PlatformService' PlatformServiceImpl where
        createService config customer this@(Obj' this' _ thisDC)
          = do st :: IORef' ServiceType <- I'.lift
                                             (I'.newIORef (serviceType config))
               I'.lift
                 ((\ b' -> assert b' (I'.pure ())) =<<
                    (\ this'' ->
                       ((==) <$!>
                          (I'.pure lookupCustomerService <*>
                             I'.pure (customers'PlatformServiceImpl this'')
                             <*> I'.pure customer
                             <*> I'.readIORef st)
                          <*> I'.pure Nothing))
                      =<< I'.readIORef this')
               instances :: IORef' (List ResourceCapacities) <- I'.lift
                                                                  (I'.newIORef (instances_ config))
               I'.lift
                 ((\ b' -> assert b' (I'.pure ())) =<<
                    ((not) <$!> ((==) <$!> I'.readIORef instances <*> I'.pure [])))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        this''{serviceId'PlatformServiceImpl =
                                 (incr (serviceId'PlatformServiceImpl this''))})
                       <$!> I'.readIORef this'))
               endPoint :: IORef' Int <- I'.lift
                                           ((\ this'' ->
                                               I'.newIORef (serviceId'PlatformServiceImpl this''))
                                              =<< I'.readIORef this')
               currentServices :: IORef' (List Service) <- I'.lift
                                                             (I'.newIORef [])
               ids :: IORef' (List Id) <- I'.lift (I'.newIORef [])
               while
                 ((not) <$!> ((==) <$!> I'.readIORef instances <*> I'.pure []))
                 (do res :: IORef' ResourceCapacities <- I'.lift
                                                           (I'.newIORef =<<
                                                              (I'.pure head <*>
                                                                 I'.readIORef instances))
                     service :: IORef' Service <- (I'.lift . I'.newIORef) =<<
                                                    ((this <..>) =<<
                                                       I'.lift
                                                         (I'.pure
                                                            createServiceInstance''PlatformServiceImpl
                                                            <*> I'.pure customer
                                                            <*> I'.readIORef st
                                                            <*> I'.readIORef res))
                     idf :: IORef' (Fut Id) <- I'.lift
                                                 (I'.newIORef =<<
                                                    ((\ (Service obj') -> (obj' <!> getServiceId))
                                                       =<< I'.readIORef service))
                     id :: IORef' Id <- I'.lift
                                          (I'.newIORef =<< (get =<< I'.readIORef idf))
                     I'.lift
                       (I'.writeIORef ids =<<
                          ((:) <$!> I'.readIORef id <*> I'.readIORef ids))
                     I'.lift
                       (I'.writeIORef this' =<<
                          ((\ this'' ->
                              (\ v' -> this''{serviceToEndPoints'PlatformServiceImpl = v'}) <$!>
                                (I'.pure InsertAssoc <*>
                                   ((,) <$!> I'.readIORef id <*>
                                      (I'.fromIntegral <$!> I'.readIORef endPoint))
                                   <*> I'.pure (serviceToEndPoints'PlatformServiceImpl this'')))
                             =<< I'.readIORef this'))
                     I'.lift
                       (I'.writeIORef currentServices =<<
                          ((:) <$!> (up' <$!> I'.readIORef service) <*>
                             I'.readIORef currentServices))
                     I'.lift
                       (I'.writeIORef instances =<<
                          (I'.pure tail <*> I'.readIORef instances)))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{endPoints'PlatformServiceImpl = v'}) <$!>
                          (I'.pure InsertAssoc <*>
                             ((,) <$!> (I'.fromIntegral <$!> I'.readIORef endPoint) <*>
                                I'.readIORef ids)
                             <*> I'.pure (endPoints'PlatformServiceImpl this'')))
                       =<< I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{customers'PlatformServiceImpl = v'}) <$!>
                          (I'.pure put <*> I'.pure (customers'PlatformServiceImpl this'') <*>
                             I'.pure customer
                             <*>
                             (I'.pure put <*>
                                (I'.pure lookupDefault <*>
                                   I'.pure (customers'PlatformServiceImpl this'')
                                   <*> I'.pure customer
                                   <*> I'.pure EmptyMap)
                                <*> I'.pure config
                                <*> (I'.fromIntegral <$!> I'.readIORef endPoint))))
                       =<< I'.readIORef this'))
               tmp1171196949 :: IORef' (Fut Bool) <- I'.lift
                                                       ((\ this'' ->
                                                           I'.newIORef =<<
                                                             ((\ (LoadBalancerService obj') ->
                                                                 (obj' <!>) =<<
                                                                   I'.pure create <*>
                                                                     I'.readIORef currentServices
                                                                     <*>
                                                                     (I'.fromIntegral <$!>
                                                                        I'.readIORef endPoint))
                                                                (ls'PlatformServiceImpl this'')))
                                                          =<< I'.readIORef this')
               awaitFuture' this =<< I'.lift (I'.readIORef tmp1171196949)
               _ <- I'.lift (get =<< I'.readIORef tmp1171196949)
               tmp1403088342 :: IORef' (Fut Bool) <- I'.lift
                                                       ((\ this'' ->
                                                           I'.newIORef =<<
                                                             ((\ (LoadBalancerService obj') ->
                                                                 (obj' <!>) =<<
                                                                   I'.pure enable <*>
                                                                     (I'.fromIntegral <$!>
                                                                        I'.readIORef endPoint))
                                                                (ls'PlatformServiceImpl this'')))
                                                          =<< I'.readIORef this')
               awaitFuture' this =<< I'.lift (I'.readIORef tmp1403088342)
               _ <- I'.lift (get =<< I'.readIORef tmp1403088342)
               I'.lift ((I'.fromIntegral <$!> I'.readIORef endPoint))
        removeService endPoint this@(Obj' this' _ thisDC)
          = do tmp1409497701 :: IORef' (Fut Bool) <- I'.lift
                                                       ((\ this'' ->
                                                           I'.newIORef =<<
                                                             ((\ (LoadBalancerService obj') ->
                                                                 (obj' <!>
                                                                    disable
                                                                      (I'.fromIntegral endPoint)))
                                                                (ls'PlatformServiceImpl this'')))
                                                          =<< I'.readIORef this')
               awaitFuture' this =<< I'.lift (I'.readIORef tmp1409497701)
               b :: IORef' Bool <- I'.lift
                                     (I'.newIORef =<< (get =<< I'.readIORef tmp1409497701))
               I'.lift ((\ b' -> assert b' (I'.pure ())) =<< I'.readIORef b)
               tmp1576354454 :: IORef' (Fut Bool) <- I'.lift
                                                       ((\ this'' ->
                                                           I'.newIORef =<<
                                                             ((\ (LoadBalancerService obj') ->
                                                                 (obj' <!>
                                                                    removeLBS
                                                                      (serviceId'PlatformServiceImpl
                                                                         this'')))
                                                                (ls'PlatformServiceImpl this'')))
                                                          =<< I'.readIORef this')
               awaitFuture' this =<< I'.lift (I'.readIORef tmp1576354454)
               I'.lift (I'.writeIORef b =<< (get =<< I'.readIORef tmp1576354454))
               I'.lift ((\ b' -> assert b' (I'.pure ())) =<< I'.readIORef b)
               sids :: IORef' (List Id) <- I'.lift
                                             ((\ this'' ->
                                                 I'.newIORef
                                                   (lookupDefault
                                                      (endPoints'PlatformServiceImpl this'')
                                                      (I'.fromIntegral endPoint)
                                                      []))
                                                =<< I'.readIORef this')
               while ((not) <$!> ((==) <$!> I'.readIORef sids <*> I'.pure []))
                 (do id :: IORef' Id <- I'.lift
                                          (I'.newIORef =<< (I'.pure head <*> I'.readIORef sids))
                     I'.lift
                       (I'.writeIORef this' =<<
                          ((\ this'' ->
                              (\ v' -> this''{services'PlatformServiceImpl = v'}) <$!>
                                (I'.pure removeKey <*>
                                   I'.pure (services'PlatformServiceImpl this'')
                                   <*> I'.readIORef id))
                             =<< I'.readIORef this'))
                     I'.lift
                       (I'.writeIORef this' =<<
                          ((\ this'' ->
                              (\ v' -> this''{serviceToEndPoints'PlatformServiceImpl = v'}) <$!>
                                (I'.pure removeKey <*>
                                   I'.pure (serviceToEndPoints'PlatformServiceImpl this'')
                                   <*> I'.readIORef id))
                             =<< I'.readIORef this'))
                     tmp1688710402 :: IORef' (Fut Unit) <- I'.lift
                                                             ((\ this'' ->
                                                                 I'.newIORef =<<
                                                                   ((\ (DeploymentService obj') ->
                                                                       (obj' <!>) =<<
                                                                         I'.pure stopDS <*>
                                                                           I'.readIORef id)
                                                                      (ds'PlatformServiceImpl
                                                                         this'')))
                                                                =<< I'.readIORef this')
                     awaitFuture' this =<< I'.lift (I'.readIORef tmp1688710402)
                     _ <- I'.lift (get =<< I'.readIORef tmp1688710402)
                     tmp1061115215 :: IORef' (Fut Unit) <- I'.lift
                                                             ((\ this'' ->
                                                                 I'.newIORef =<<
                                                                   ((\ (DeploymentService obj') ->
                                                                       (obj' <!>) =<<
                                                                         I'.pure uninstallDS <*>
                                                                           I'.readIORef id)
                                                                      (ds'PlatformServiceImpl
                                                                         this'')))
                                                                =<< I'.readIORef this')
                     awaitFuture' this =<< I'.lift (I'.readIORef tmp1061115215)
                     _ <- I'.lift (get =<< I'.readIORef tmp1061115215)
                     I'.lift
                       (I'.writeIORef sids =<< (I'.pure tail <*> I'.readIORef sids)))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        this''{endPoints'PlatformServiceImpl =
                                 (removeKey (endPoints'PlatformServiceImpl this'')
                                    (I'.fromIntegral endPoint))})
                       <$!> I'.readIORef this'))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        this''{customers'PlatformServiceImpl =
                                 (removeGlobalEndPoint (customers'PlatformServiceImpl this'')
                                    (I'.fromIntegral endPoint))})
                       <$!> I'.readIORef this'))

createServiceInstance''PlatformServiceImpl ::
                                           Customer ->
                                             ServiceType ->
                                               ResourceCapacities ->
                                                 Obj' PlatformServiceImpl -> ABS' Service
createServiceInstance''PlatformServiceImpl customer st resource
  this@(Obj' this' _ thisDC)
  = do I'.lift
         (I'.writeIORef this' =<<
            ((\ this'' ->
                this''{serviceId'PlatformServiceImpl =
                         (incr (serviceId'PlatformServiceImpl this''))})
               <$!> I'.readIORef this'))
       id :: IORef' Id <- I'.lift
                            ((\ this'' -> I'.newIORef (serviceId'PlatformServiceImpl this''))
                               =<< I'.readIORef this')
       vms :: IORef' (Set VMType) <- (I'.lift . I'.newIORef) =<<
                                       (this <..> findVM''PlatformServiceImpl resource)
       I'.lift
         ((\ b' -> assert b' (I'.pure ())) =<<
            ((not) <$!> (I'.pure emptySet <*> I'.readIORef vms)))
       v :: IORef' VMType <- I'.lift
                               (I'.newIORef =<< (I'.pure takeFromSet <*> I'.readIORef vms))
       sf :: IORef' (Fut Service) <- I'.lift
                                       ((\ this'' ->
                                           I'.newIORef =<<
                                             ((\ (DeploymentService obj') ->
                                                 (obj' <!>) =<<
                                                   I'.pure installDS <*> I'.pure customer <*>
                                                     I'.pure st
                                                     <*> I'.readIORef id
                                                     <*> I'.readIORef v)
                                                (ds'PlatformServiceImpl this'')))
                                          =<< I'.readIORef this')
       service :: IORef' Service <- I'.lift
                                      (I'.newIORef =<< (get =<< I'.readIORef sf))
       uf :: IORef' (Fut Unit) <- I'.lift
                                    ((\ this'' ->
                                        I'.newIORef =<<
                                          ((\ (DeploymentService obj') ->
                                              (obj' <!>) =<< I'.pure startDS <*> I'.readIORef id)
                                             (ds'PlatformServiceImpl this'')))
                                       =<< I'.readIORef this')
       _ <- I'.lift (get =<< I'.readIORef sf)
       I'.lift
         (I'.writeIORef this' =<<
            ((\ this'' ->
                (\ v' -> this''{services'PlatformServiceImpl = v'}) <$!>
                  (I'.pure put <*> I'.pure (services'PlatformServiceImpl this'') <*>
                     I'.readIORef id
                     <*> (up' <$!> I'.readIORef service)))
               =<< I'.readIORef this'))
       I'.lift ((up' <$!> I'.readIORef service))

findVM''PlatformServiceImpl ::
                            ResourceCapacities -> Obj' PlatformServiceImpl -> ABS' (Set VMType)
findVM''PlatformServiceImpl rc2 this@(Obj' this' _ thisDC)
  = do remaining :: IORef' (Set VMType) <- I'.lift
                                             (I'.newIORef (vmTypesCollection))
       res :: IORef' (Set VMType) <- I'.lift (I'.newIORef EmptySet)
       while ((not) <$!> (I'.pure emptySet <*> I'.readIORef remaining))
         (do vm :: IORef' VMType <- I'.lift
                                      (I'.newIORef =<<
                                         (I'.pure takeFromSet <*> I'.readIORef remaining))
             I'.lift
               (I'.writeIORef remaining =<<
                  (I'.pure remove <*> I'.readIORef remaining <*> I'.readIORef vm))
             match :: IORef' Bool <- (I'.lift . I'.newIORef) =<<
                                       ((this <..>) =<<
                                          I'.lift
                                            (I'.pure matchResources''PlatformServiceImpl <*>
                                               (I'.pure vmResources <*> I'.readIORef vm)
                                               <*> I'.pure rc2))
             when' <- I'.lift (I'.readIORef match)
             I'.when when'
               (do I'.lift
                     (I'.writeIORef res =<<
                        (I'.pure insertElement <*> I'.readIORef res <*> I'.readIORef vm))))
       I'.lift (I'.readIORef res)

matchResources''PlatformServiceImpl ::
                                    ResourceCapacities ->
                                      ResourceCapacities -> Obj' PlatformServiceImpl -> ABS' Bool
matchResources''PlatformServiceImpl rc1 rc2
  this@(Obj' this' _ thisDC)
  = do result :: IORef' Bool <- I'.lift (I'.newIORef True)
       resources1 :: IORef' (Set Resourcetype) <- I'.lift
                                                    (I'.newIORef (keys rc1))
       while
         ((&&) <$!> ((==) <$!> I'.readIORef result <*> I'.pure True) <*>
            ((not) <$!> (I'.pure emptySet <*> I'.readIORef resources1)))
         (do key :: IORef' Resourcetype <- I'.lift
                                             (I'.newIORef =<<
                                                (I'.pure takeFromSet <*> I'.readIORef resources1))
             I'.lift
               (I'.writeIORef resources1 =<<
                  (I'.pure remove <*> I'.readIORef resources1 <*> I'.readIORef key))
             value1 :: IORef' Rat <- I'.lift
                                       (I'.newIORef =<<
                                          (I'.pure lookupUnsafe <*> I'.pure rc1 <*>
                                             I'.readIORef key))
             value2 :: IORef' (Maybe Rat) <- I'.lift
                                               (I'.newIORef =<<
                                                  (I'.pure lookup <*> I'.pure rc2 <*>
                                                     I'.readIORef key))
             case' <- I'.lift (I'.readIORef value2)
             case case' of
                 Nothing -> do I'.lift (I'.writeIORef result True)
                 Just v2 -> do when' <- I'.lift
                                          (((<) <$!> I'.readIORef value1 <*> I'.pure v2))
                               I'.when when' (do I'.lift (I'.writeIORef result False)))
       I'.lift (I'.readIORef result)

uninstallInstance''PlatformServiceImpl ::
                                       Id -> Service -> Id -> Obj' PlatformServiceImpl -> ABS' Unit
uninstallInstance''PlatformServiceImpl endPoint s serviceId
  this@(Obj' this' _ thisDC)
  = do bf :: IORef' (Fut Bool) <- I'.lift
                                    ((\ this'' ->
                                        I'.newIORef =<<
                                          ((\ (LoadBalancerService obj') ->
                                              (obj' <!> decrease endPoint (list ((up' s) : []))))
                                             (ls'PlatformServiceImpl this'')))
                                       =<< I'.readIORef this')
       _ <- I'.lift (get =<< I'.readIORef bf)
       uf :: IORef' (Fut Unit) <- I'.lift
                                    ((\ this'' ->
                                        I'.newIORef =<<
                                          ((\ (DeploymentService obj') ->
                                              (obj' <!> stopDS serviceId))
                                             (ds'PlatformServiceImpl this'')))
                                       =<< I'.readIORef this')
       _ <- I'.lift (get =<< I'.readIORef uf)
       I'.lift
         ((\ this'' ->
             I'.writeIORef uf =<<
               ((\ (DeploymentService obj') -> (obj' <!> uninstallDS serviceId))
                  (ds'PlatformServiceImpl this'')))
            =<< I'.readIORef this')
       _ <- I'.lift (get =<< I'.readIORef uf)
       I'.pure ()

{-# LINE 1480 "initial\FRH.abs" #-}
data MonitoringServiceImpl = MonitoringServiceImpl{log'MonitoringServiceImpl
                                                   :: Int,
                                                   monitorMap'MonitoringServiceImpl ::
                                                   Map Int (Map Int (List Monitor))}

smart'MonitoringServiceImpl :: MonitoringServiceImpl
smart'MonitoringServiceImpl
  = (\ log'this ->
       (\ monitorMap'this ->
          (MonitoringServiceImpl (I'.fromIntegral log'this) monitorMap'this))
         (EmptyMap :: Map Int (Map Int (List Monitor))))
      (0 :: Int)

init'MonitoringServiceImpl ::
                           Obj' MonitoringServiceImpl -> I'.IO ()
{-# LINE 1480 "initial\FRH.abs" #-}
init'MonitoringServiceImpl this@(Obj' this' _ thisDC)
  = this <!!> run''MonitoringServiceImpl

instance MonitoringService' MonitoringServiceImpl where
        addMS rule this@(Obj' this' _ thisDC)
          = do interval :: IORef' Int <- I'.lift
                                           (I'.newIORef (interval rule))
               I'.lift
                 ((\ b' -> assert b' (I'.pure ())) =<<
                    ((>) <$!> (I'.fromIntegral <$!> I'.readIORef interval) <*>
                       I'.pure 0))
               monitor :: IORef' Monitor <- I'.lift (I'.newIORef (monitor_ rule))
               thisInterval :: IORef' (Map Int (List Monitor)) <- I'.lift
                                                                    (I'.newIORef =<<
                                                                       (\ this'' ->
                                                                          (I'.pure lookupDefault <*>
                                                                             I'.pure
                                                                               (monitorMap'MonitoringServiceImpl
                                                                                  this'')
                                                                             <*>
                                                                             (I'.fromIntegral <$!>
                                                                                I'.readIORef
                                                                                  interval)
                                                                             <*> I'.pure EmptyMap))
                                                                         =<< I'.readIORef this')
               monitors :: IORef' (List Monitor) <- I'.lift
                                                      (I'.newIORef =<<
                                                         (I'.pure lookupDefault <*>
                                                            I'.readIORef thisInterval
                                                            <*>
                                                            (I'.fromIntegral <$!>
                                                               I'.readIORef interval)
                                                            <*> I'.pure []))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{monitorMap'MonitoringServiceImpl = v'}) <$!>
                          (I'.pure put <*> I'.pure (monitorMap'MonitoringServiceImpl this'')
                             <*> (I'.fromIntegral <$!> I'.readIORef interval)
                             <*>
                             (I'.pure put <*> I'.readIORef thisInterval <*>
                                (I'.fromIntegral <$!> I'.readIORef interval)
                                <*>
                                ((:) <$!> (up' <$!> I'.readIORef monitor) <*>
                                   I'.readIORef monitors))))
                       =<< I'.readIORef this'))
        removeMS rule this@(Obj' this' _ thisDC)
          = do interval :: IORef' Int <- I'.lift
                                           (I'.newIORef (interval rule))
               monitor :: IORef' Monitor <- I'.lift (I'.newIORef (monitor_ rule))
               thisInterval :: IORef' (Map Int (List Monitor)) <- I'.lift
                                                                    (I'.newIORef =<<
                                                                       (\ this'' ->
                                                                          (I'.pure lookupDefault <*>
                                                                             I'.pure
                                                                               (monitorMap'MonitoringServiceImpl
                                                                                  this'')
                                                                             <*>
                                                                             (I'.fromIntegral <$!>
                                                                                I'.readIORef
                                                                                  interval)
                                                                             <*> I'.pure EmptyMap))
                                                                         =<< I'.readIORef this')
               keys :: IORef' (Set Int) <- I'.lift
                                             (I'.newIORef =<<
                                                (I'.pure keys <*> I'.readIORef thisInterval))
               done :: IORef' Bool <- I'.lift (I'.newIORef False)
               while
                 ((&&) <$!>
                    ((not) <$!> ((==) <$!> I'.readIORef keys <*> I'.pure EmptySet))
                    <*> ((not) <$!> I'.readIORef done))
                 (do nt :: IORef' (Pair (Set Int) Int) <- I'.lift
                                                            (I'.newIORef =<<
                                                               (I'.pure next <*> I'.readIORef keys))
                     monitors :: IORef' (List Monitor) <- I'.lift
                                                            (I'.newIORef =<<
                                                               (I'.pure lookupDefault <*>
                                                                  I'.readIORef thisInterval
                                                                  <*>
                                                                  (I'.pure snd <*> I'.readIORef nt)
                                                                  <*> I'.pure []))
                     if' <- I'.lift
                              ((I'.pure inList <*> I'.readIORef monitors <*>
                                  (up' <$!> I'.readIORef monitor)))
                     if if' then
                       do I'.lift
                            (I'.writeIORef this' =<<
                               ((\ this'' ->
                                   (\ v' -> this''{monitorMap'MonitoringServiceImpl = v'}) <$!>
                                     (I'.pure put <*>
                                        I'.pure (monitorMap'MonitoringServiceImpl this'')
                                        <*> (I'.fromIntegral <$!> I'.readIORef interval)
                                        <*>
                                        (I'.pure put <*> I'.readIORef thisInterval <*>
                                           (I'.pure snd <*> I'.readIORef nt)
                                           <*>
                                           (I'.pure without <*> I'.readIORef monitors <*>
                                              (up' <$!> I'.readIORef monitor)))))
                                  =<< I'.readIORef this'))
                          I'.lift (I'.writeIORef done True)
                       else
                       do I'.lift
                            (I'.writeIORef keys =<< (I'.pure fst <*> I'.readIORef nt)))

execute''MonitoringServiceImpl ::
                               Monitor -> Obj' MonitoringServiceImpl -> ABS' Unit
execute''MonitoringServiceImpl m this@(Obj' this' _ thisDC)
  = do f :: IORef' (Fut Unit) <- I'.lift
                                   (I'.newIORef =<< ((\ (Monitor obj') -> (obj' <!> monitor)) m))
       awaitFuture' this =<< I'.lift (I'.readIORef f)
       _ <- I'.lift (get =<< I'.readIORef f)
       I'.pure ()

run''MonitoringServiceImpl ::
                           Obj' MonitoringServiceImpl -> ABS' Unit
run''MonitoringServiceImpl this@(Obj' this' _ thisDC)
  = do while (I'.pure True)
         (do awaitDuration' this (maximum [1]) (minimum [1])
             I'.lift
               (I'.writeIORef this' =<<
                  ((\ this'' ->
                      this''{log'MonitoringServiceImpl =
                               ((I'.fromIntegral (log'MonitoringServiceImpl this'')) + 1)})
                     <$!> I'.readIORef this'))
             I'.lift
               (I'.writeIORef this' =<<
                  ((\ this'' ->
                      this''{monitorMap'MonitoringServiceImpl =
                               (decr (monitorMap'MonitoringServiceImpl this''))})
                     <$!> I'.readIORef this'))
             toBeRun :: IORef' (List Monitor) <- I'.lift
                                                   ((\ this'' ->
                                                       I'.newIORef
                                                         (lookupAllSecond
                                                            (monitorMap'MonitoringServiceImpl
                                                               this'')
                                                            0))
                                                      =<< I'.readIORef this')
             I'.lift
               (I'.writeIORef this' =<<
                  ((\ this'' ->
                      this''{monitorMap'MonitoringServiceImpl =
                               (reset (monitorMap'MonitoringServiceImpl this'') 0)})
                     <$!> I'.readIORef this'))
             futs :: IORef' (List (Fut Unit)) <- I'.lift (I'.newIORef [])
             while ((not) <$!> ((==) <$!> I'.readIORef toBeRun <*> I'.pure []))
               (do mon :: IORef' Monitor <- I'.lift
                                              (I'.newIORef =<<
                                                 (I'.pure head <*> I'.readIORef toBeRun))
                   f :: IORef' (Fut Unit) <- I'.lift
                                               (I'.newIORef =<<
                                                  ((this <!>) =<<
                                                     I'.pure execute''MonitoringServiceImpl <*>
                                                       (up' <$!> I'.readIORef mon)))
                   I'.lift
                     (I'.writeIORef futs =<<
                        ((:) <$!> I'.readIORef f <*> I'.readIORef futs))
                   I'.lift
                     (I'.writeIORef toBeRun =<<
                        (I'.pure tail <*> I'.readIORef toBeRun)))
             while ((not) <$!> ((==) <$!> I'.readIORef futs <*> I'.pure []))
               (do f :: IORef' (Fut Unit) <- I'.lift
                                               (I'.newIORef =<<
                                                  (I'.pure head <*> I'.readIORef futs))
                   awaitFuture' this =<< I'.lift (I'.readIORef f)
                   _ <- I'.lift (get =<< I'.readIORef f)
                   I'.lift
                     (I'.writeIORef futs =<< (I'.pure tail <*> I'.readIORef futs))))

{-# LINE 1538 "initial\FRH.abs" #-}
data LatencyMonitor = LatencyMonitor{log'LatencyMonitor :: Int,
                                     ps'LatencyMonitor :: MonitorPlatformService,
                                     upper'LatencyMonitor :: Int}

smart'LatencyMonitor ::
                     Int -> MonitorPlatformService -> LatencyMonitor
smart'LatencyMonitor upper'this ps'this
  = (\ log'this ->
       (LatencyMonitor (I'.fromIntegral log'this) (up' ps'this)
          (I'.fromIntegral upper'this)))
      (0 :: Int)

init'LatencyMonitor :: Obj' LatencyMonitor -> I'.IO ()
{-# LINE 1538 "initial\FRH.abs" #-}
init'LatencyMonitor this@(Obj' this' _ thisDC) = I'.pure ()

instance Monitor' LatencyMonitor where
        monitor this@(Obj' this' _ thisDC)
          = do I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        this''{log'LatencyMonitor =
                                 ((I'.fromIntegral (log'LatencyMonitor this'')) + 1)})
                       <$!> I'.readIORef this'))
               scaling_ :: IORef' (List (Triple Id Resourcetype Rat)) <- I'.lift
                                                                           (I'.newIORef [])
               ef :: IORef' (Fut (List Int)) <- I'.lift
                                                  ((\ this'' ->
                                                      I'.newIORef =<<
                                                        ((\ (MonitorPlatformService obj') ->
                                                            (obj' <!> getEndPoints))
                                                           (ps'LatencyMonitor this'')))
                                                     =<< I'.readIORef this')
               endPoints :: IORef' (List Int) <- I'.lift
                                                   (I'.newIORef =<< (get =<< I'.readIORef ef))
               while
                 ((not) <$!> ((==) <$!> I'.readIORef endPoints <*> I'.pure []))
                 (do endPoint :: IORef' Int <- I'.lift
                                                 (I'.newIORef =<<
                                                    (I'.pure head <*> I'.readIORef endPoints))
                     setting :: IORef' (List (Pair Id Int)) <- (I'.lift . I'.newIORef)
                                                                 =<<
                                                                 ((this <..>) =<<
                                                                    I'.lift
                                                                      (I'.pure
                                                                         getLatencies''LatencyMonitor
                                                                         <*>
                                                                         (I'.fromIntegral <$!>
                                                                            I'.readIORef endPoint)))
                     while ((not) <$!> ((==) <$!> I'.readIORef setting <*> I'.pure []))
                       (do lat :: IORef' (Pair Id Int) <- I'.lift
                                                            (I'.newIORef =<<
                                                               (I'.pure head <*>
                                                                  I'.readIORef setting))
                           I'.lift
                             (I'.writeIORef setting =<< (I'.pure tail <*> I'.readIORef setting))
                           h :: IORef' (Triple Id Resourcetype Rat) <- I'.lift
                                                                         (I'.newIORef =<<
                                                                            ((,,) <$!>
                                                                               (I'.pure fst <*>
                                                                                  I'.readIORef lat)
                                                                               <*> I'.pure Speed
                                                                               <*>
                                                                               (I'.pure (I'.fromIntegral . snd) <*>
                                                                                  I'.readIORef
                                                                                    lat)))
                           I'.lift
                             (I'.writeIORef scaling_ =<<
                                ((:) <$!> I'.readIORef h <*> I'.readIORef scaling_)))
                     I'.lift
                       (I'.writeIORef endPoints =<<
                          (I'.pure tail <*> I'.readIORef endPoints)))
               while ((not) <$!> ((==) <$!> I'.readIORef scaling_ <*> I'.pure []))
                 (do sc :: IORef' (Triple Id Resourcetype Rat) <- I'.lift
                                                                    (I'.newIORef =<<
                                                                       (I'.pure head <*>
                                                                          I'.readIORef scaling_))
                     instance_ :: IORef' Id <- I'.lift
                                                 (I'.newIORef =<<
                                                    (I'.pure fstT <*> I'.readIORef sc))
                     rt :: IORef' Resourcetype <- I'.lift
                                                    (I'.newIORef =<<
                                                       (I'.pure sndT <*> I'.readIORef sc))
                     v :: IORef' Rat <- I'.lift
                                          (I'.newIORef =<< (I'.pure trd <*> I'.readIORef sc))
                     fu :: IORef' (Fut Unit) <- I'.lift
                                                  ((\ this'' ->
                                                      I'.newIORef =<<
                                                        ((\ (MonitorPlatformService obj') ->
                                                            (obj' <!>) =<<
                                                              I'.pure alterResource <*>
                                                                I'.readIORef instance_
                                                                <*> I'.readIORef rt
                                                                <*> I'.readIORef v)
                                                           (ps'LatencyMonitor this'')))
                                                     =<< I'.readIORef this')
                     _ <- I'.lift (get =<< I'.readIORef fu)
                     I'.lift
                       (I'.writeIORef scaling_ =<<
                          (I'.pure tail <*> I'.readIORef scaling_)))
        metricHistory this@(Obj' this' _ thisDC) = do I'.lift (I'.pure [])

getLatencies''LatencyMonitor ::
                             Int -> Obj' LatencyMonitor -> ABS' (List (Pair Id Int))
getLatencies''LatencyMonitor endPoint this@(Obj' this' _ thisDC)
  = do scaling_ :: IORef' (List (Pair Id Int)) <- I'.lift
                                                    (I'.newIORef [])
       fservices :: IORef' (Fut (List Id)) <- I'.lift
                                                ((\ this'' ->
                                                    I'.newIORef =<<
                                                      ((\ (MonitorPlatformService obj') ->
                                                          (obj' <!>
                                                             getServiceIds
                                                               (I'.fromIntegral endPoint)))
                                                         (ps'LatencyMonitor this'')))
                                                   =<< I'.readIORef this')
       services :: IORef' (List Id) <- I'.lift
                                         (I'.newIORef =<< (get =<< I'.readIORef fservices))
       while ((not) <$!> ((==) <$!> I'.readIORef services <*> I'.pure []))
         (do serviceId :: IORef' Id <- I'.lift
                                         (I'.newIORef =<< (I'.pure head <*> I'.readIORef services))
             lf :: IORef' (Fut (Maybe Service)) <- I'.lift
                                                     ((\ this'' ->
                                                         I'.newIORef =<<
                                                           ((\ (MonitorPlatformService obj') ->
                                                               (obj' <!>) =<<
                                                                 I'.pure getServiceMPS <*>
                                                                   I'.readIORef serviceId)
                                                              (ps'LatencyMonitor this'')))
                                                        =<< I'.readIORef this')
             ml :: IORef' (Maybe Service) <- I'.lift
                                               (I'.newIORef =<< (get =<< I'.readIORef lf))
             I'.lift
               ((\ b' -> assert b' (I'.pure ())) =<<
                  ((not) <$!> ((==) <$!> I'.readIORef ml <*> I'.pure Nothing)))
             logger :: IORef' Service <- I'.lift
                                           (I'.newIORef =<< (I'.pure fromJust <*> I'.readIORef ml))
             lif :: IORef' (Fut Int) <- I'.lift
                                          (I'.newIORef =<<
                                             ((\ (Service obj') -> (obj' <!> getLatency)) =<<
                                                I'.readIORef logger))
             latency :: IORef' Int <- I'.lift
                                        (I'.newIORef =<< (get =<< I'.readIORef lif))
             when' <- I'.lift
                        ((\ this'' ->
                            ((>) <$!> (I'.fromIntegral <$!> I'.readIORef latency) <*>
                               I'.pure (I'.fromIntegral (upper'LatencyMonitor this''))))
                           =<< I'.readIORef this')
             I'.when when'
               (do res :: IORef' Int <- (I'.lift . I'.newIORef) =<<
                                          ((this <..>) =<<
                                             I'.lift
                                               (I'.pure scaling''LatencyMonitor <*>
                                                  (up' <$!> I'.readIORef logger)
                                                  <*> (I'.fromIntegral <$!> I'.readIORef latency)))
                   I'.lift
                     (I'.writeIORef scaling_ =<<
                        ((:) <$!>
                           ((,) <$!> I'.readIORef serviceId <*>
                              (I'.fromIntegral <$!> I'.readIORef res))
                           <*> I'.readIORef scaling_)))
             I'.lift
               (I'.writeIORef services =<<
                  (I'.pure tail <*> I'.readIORef services)))
       I'.lift (I'.readIORef scaling_)

scaling''LatencyMonitor ::
                        Service -> Int -> Obj' LatencyMonitor -> ABS' Int
scaling''LatencyMonitor logger latency this@(Obj' this' _ thisDC)
  = do fcpu :: IORef' (Fut Int) <- I'.lift
                                     (I'.newIORef =<<
                                        ((\ (Service obj') -> (obj' <!> getCPU)) logger))
       cpu :: IORef' Int <- I'.lift
                              (I'.newIORef =<< (get =<< I'.readIORef fcpu))
       amount :: IORef' Rat <- I'.lift
                                 (I'.newIORef =<<
                                    (\ this'' ->
                                       ((*) <$!> (I'.fromIntegral <$!> I'.readIORef cpu) <*>
                                          ((-) <$!> I'.pure (I'.fromIntegral latency) <*>
                                             ((/) <$!>
                                                I'.pure
                                                  (I'.fromIntegral (upper'LatencyMonitor this''))
                                                <*> I'.pure (I'.fromIntegral latency)))))
                                      =<< I'.readIORef this')
       I'.lift
         ((I'.pure max <*> I'.pure 1 <*>
             (I'.pure truncate <*> I'.readIORef amount)))

{-# LINE 1599 "initial\FRH.abs" #-}
data ServiceProviderImpl = ServiceProviderImpl{customers'ServiceProviderImpl
                                               :: Map Customer (Map Config Int),
                                               ls'ServiceProviderImpl :: LoadBalancerService,
                                               ps'ServiceProviderImpl :: PlatformService}

smart'ServiceProviderImpl ::
                          PlatformService -> LoadBalancerService -> ServiceProviderImpl
smart'ServiceProviderImpl ps'this ls'this
  = (\ customers'this ->
       (ServiceProviderImpl customers'this (up' ls'this) (up' ps'this)))
      (EmptyMap :: Map Customer (Map Config Int))

init'ServiceProviderImpl :: Obj' ServiceProviderImpl -> I'.IO ()
{-# LINE 1599 "initial\FRH.abs" #-}
init'ServiceProviderImpl this@(Obj' this' _ thisDC) = I'.pure ()

instance ServiceProvider' ServiceProviderImpl where
        addCustomer sc c this@(Obj' this' _ thisDC)
          = do tmp325183616 :: IORef' (Fut Int) <- I'.lift
                                                     ((\ this'' ->
                                                         I'.newIORef =<<
                                                           ((\ (PlatformService obj') ->
                                                               (obj' <!> createService sc c))
                                                              (ps'ServiceProviderImpl this'')))
                                                        =<< I'.readIORef this')
               awaitFuture' this =<< I'.lift (I'.readIORef tmp325183616)
               id :: IORef' Int <- I'.lift
                                     (I'.newIORef =<< (get =<< I'.readIORef tmp325183616))
               map :: IORef' (Map Config Int) <- I'.lift
                                                   ((\ this'' ->
                                                       I'.newIORef
                                                         (lookupDefault
                                                            (customers'ServiceProviderImpl this'')
                                                            c
                                                            EmptyMap))
                                                      =<< I'.readIORef this')
               I'.lift
                 ((\ b' -> assert b' (I'.pure ())) =<<
                    ((==) <$!> (I'.pure lookup <*> I'.readIORef map <*> I'.pure sc) <*>
                       I'.pure Nothing))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{customers'ServiceProviderImpl = v'}) <$!>
                          (I'.pure put <*> I'.pure (customers'ServiceProviderImpl this'') <*>
                             I'.pure c
                             <*>
                             (I'.pure InsertAssoc <*>
                                ((,) <$!> I'.pure sc <*> (I'.fromIntegral <$!> I'.readIORef id))
                                <*> I'.readIORef map)))
                       =<< I'.readIORef this'))
               ef :: IORef' (Fut (Maybe LoadBalancerEndPoint)) <- I'.lift
                                                                    ((\ this'' ->
                                                                        I'.newIORef =<<
                                                                          ((\ (LoadBalancerService
                                                                                 obj')
                                                                              ->
                                                                              (obj' <!>) =<<
                                                                                I'.pure
                                                                                  getEndPointById
                                                                                  <*>
                                                                                  (I'.fromIntegral
                                                                                     <$!>
                                                                                     I'.readIORef
                                                                                       id))
                                                                             (ls'ServiceProviderImpl
                                                                                this'')))
                                                                       =<< I'.readIORef this')
               eps :: IORef' (Maybe LoadBalancerEndPoint) <- I'.lift
                                                               (I'.newIORef =<<
                                                                  (get =<< I'.readIORef ef))
               I'.lift
                 ((\ b' -> assert b' (I'.pure ())) =<<
                    ((not) <$!> ((==) <$!> I'.readIORef eps <*> I'.pure Nothing)))
               I'.lift ((I'.pure (up' . fromJust) <*> I'.readIORef eps))
        removeCustomer sc c this@(Obj' this' _ thisDC)
          = do map :: IORef' (Map Config Int) <- I'.lift
                                                   ((\ this'' ->
                                                       I'.newIORef
                                                         (lookupDefault
                                                            (customers'ServiceProviderImpl this'')
                                                            c
                                                            EmptyMap))
                                                      =<< I'.readIORef this')
               mid :: IORef' (Maybe Int) <- I'.lift
                                              (I'.newIORef =<<
                                                 (I'.pure lookup <*> I'.readIORef map <*>
                                                    I'.pure sc))
               I'.lift
                 ((\ b' -> assert b' (I'.pure ())) =<<
                    ((not) <$!> ((==) <$!> I'.readIORef mid <*> I'.pure Nothing)))
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{customers'ServiceProviderImpl = v'}) <$!>
                          (I'.pure put <*> I'.pure (customers'ServiceProviderImpl this'') <*>
                             I'.pure c
                             <*> (I'.pure removeKey <*> I'.readIORef map <*> I'.pure sc)))
                       =<< I'.readIORef this'))
               tmp28692953 :: IORef' (Fut Unit) <- I'.lift
                                                     ((\ this'' ->
                                                         I'.newIORef =<<
                                                           ((\ (PlatformService obj') ->
                                                               (obj' <!>) =<<
                                                                 I'.pure removeService <*>
                                                                   (I'.pure fromJust <*>
                                                                      I'.readIORef mid))
                                                              (ps'ServiceProviderImpl this'')))
                                                        =<< I'.readIORef this')
               awaitFuture' this =<< I'.lift (I'.readIORef tmp28692953)
               _ <- I'.lift (get =<< I'.readIORef tmp28692953)
               I'.pure ()

{-# LINE 1627 "initial\FRH.abs" #-}
data QueryServiceImpl = QueryServiceImpl{c'QueryServiceImpl ::
                                         Customer,
                                         currentState'QueryServiceImpl :: State,
                                         da'QueryServiceImpl :: DeploymentAgent,
                                         reqCount'QueryServiceImpl :: Int,
                                         serviceId'QueryServiceImpl :: Id,
                                         staging'QueryServiceImpl :: Bool}

smart'QueryServiceImpl ::
                       DeploymentAgent -> Customer -> Bool -> QueryServiceImpl
smart'QueryServiceImpl da'this c'this staging'this
  = (\ reqCount'this ->
       (\ currentState'this ->
          (\ serviceId'this ->
             (QueryServiceImpl c'this currentState'this (up' da'this)
                (I'.fromIntegral reqCount'this)
                serviceId'this
                staging'this))
            (0 :: Id))
         (STOP :: State))
      (0 :: Int)

init'QueryServiceImpl :: Obj' QueryServiceImpl -> I'.IO ()
{-# LINE 1627 "initial\FRH.abs" #-}
init'QueryServiceImpl this@(Obj' this' _ thisDC) = I'.pure ()

instance IQueryService' QueryServiceImpl where
        doQuery q this@(Obj' this' _ thisDC) = do I'.lift (I'.pure [])

instance Service' QueryServiceImpl where
        getServiceId this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' -> I'.pure (serviceId'QueryServiceImpl this'')) =<<
                    I'.readIORef this')
        setServiceId id this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' ->
                     assert ((serviceId'QueryServiceImpl this'') == 0) (I'.pure ()))
                    =<< I'.readIORef this')
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' -> this''{serviceId'QueryServiceImpl = id}) <$!>
                       I'.readIORef this'))
        getServiceType this@(Obj' this' _ thisDC)
          = do I'.lift (I'.pure FAS)
        getCustomer this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' -> I'.pure (c'QueryServiceImpl this'')) =<<
                    I'.readIORef this')
        getLatency this@(Obj' this' _ thisDC) = do I'.lift (I'.pure 0)
        getRequestCount this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' ->
                     I'.pure (I'.fromIntegral (reqCount'QueryServiceImpl this'')))
                    =<< I'.readIORef this')
        getCPU this@(Obj' this' _ thisDC)
          = do dc :: IORef' DeploymentComponent <- I'.lift
                                                     (I'.newIORef thisDC)
               fdt :: IORef' (Fut InfRat) <- I'.lift
                                               (I'.newIORef =<<
                                                  ((\ (DeploymentComponent obj') ->
                                                      (obj' <!> total Speed))
                                                     =<< I'.readIORef dc))
               dt :: IORef' InfRat <- I'.lift
                                        (I'.newIORef =<< (get =<< I'.readIORef fdt))
               I'.lift
                 ((I'.pure truncate <*> (I'.pure finvalue <*> I'.readIORef dt)))
        getBandwidth this@(Obj' this' _ thisDC)
          = do dc :: IORef' DeploymentComponent <- I'.lift
                                                     (I'.newIORef thisDC)
               fdt :: IORef' (Fut InfRat) <- I'.lift
                                               (I'.newIORef =<<
                                                  ((\ (DeploymentComponent obj') ->
                                                      (obj' <!> total Bandwidth))
                                                     =<< I'.readIORef dc))
               dt :: IORef' InfRat <- I'.lift
                                        (I'.newIORef =<< (get =<< I'.readIORef fdt))
               I'.lift
                 ((I'.pure truncate <*> (I'.pure finvalue <*> I'.readIORef dt)))
        getMemory this@(Obj' this' _ thisDC)
          = do dc :: IORef' DeploymentComponent <- I'.lift
                                                     (I'.newIORef thisDC)
               fdt :: IORef' (Fut InfRat) <- I'.lift
                                               (I'.newIORef =<<
                                                  ((\ (DeploymentComponent obj') ->
                                                      (obj' <!> total Memory))
                                                     =<< I'.readIORef dc))
               dt :: IORef' InfRat <- I'.lift
                                        (I'.newIORef =<< (get =<< I'.readIORef fdt))
               I'.lift
                 ((I'.pure truncate <*> (I'.pure finvalue <*> I'.readIORef dt)))
        getResource t this@(Obj' this' _ thisDC)
          = do dc :: IORef' DeploymentComponent <- I'.lift
                                                     (I'.newIORef thisDC)
               fdt :: IORef' (Fut InfRat) <- I'.lift
                                               (I'.newIORef =<<
                                                  ((\ (DeploymentComponent obj') ->
                                                      (obj' <!> total t))
                                                     =<< I'.readIORef dc))
               dt :: IORef' InfRat <- I'.lift
                                        (I'.newIORef =<< (get =<< I'.readIORef fdt))
               I'.lift (I'.readIORef dt)

instance EndPoint' QueryServiceImpl where
        invoke request this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' ->
                     assert ((currentState'QueryServiceImpl this'') == RUNNING)
                       (I'.pure ()))
                    =<< I'.readIORef this')
               t :: IORef' Time <- I'.lift (I'.newIORef =<< now)
               I'.lift
                 (println =<<
                    ((+) <$!>
                       ((+) <$!> (I'.pure toString <*> I'.readIORef t) <*>
                          I'.pure ": invoking query with cost ")
                       <*> (I'.pure toString <*> I'.pure request)))
               (\ (DeploymentComponent obj') ->
                  awaitSugar'' this obj' (request__ (cost request)))
                 thisDC
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        this''{reqCount'QueryServiceImpl =
                                 ((I'.fromIntegral (reqCount'QueryServiceImpl this'')) + 1)})
                       <$!> I'.readIORef this'))
               I'.lift (I'.pure True)
        setStatus status this@(Obj' this' _ thisDC)
          = do I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' -> this''{currentState'QueryServiceImpl = status}) <$!>
                       I'.readIORef this'))
        getStatus this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' -> I'.pure (currentState'QueryServiceImpl this'')) =<<
                    I'.readIORef this')

{-# LINE 1694 "initial\FRH.abs" #-}
data MonitoringQueryEndpointImpl = MonitoringQueryEndpointImpl{allEndpoints'MonitoringQueryEndpointImpl
                                                               :: List EndPoint,
                                                               current'MonitoringQueryEndpointImpl
                                                               :: List EndPoint,
                                                               monitor'MonitoringQueryEndpointImpl
                                                               :: DegradationMonitorIf,
                                                               state'MonitoringQueryEndpointImpl ::
                                                               State}

smart'MonitoringQueryEndpointImpl ::
                                  List EndPoint ->
                                    DegradationMonitorIf -> MonitoringQueryEndpointImpl
smart'MonitoringQueryEndpointImpl allEndpoints'this monitor'this
  = (\ current'this ->
       (\ state'this ->
          (MonitoringQueryEndpointImpl allEndpoints'this current'this
             (up' monitor'this)
             state'this))
         (RUNNING :: State))
      (allEndpoints'this :: List EndPoint)

init'MonitoringQueryEndpointImpl ::
                                 Obj' MonitoringQueryEndpointImpl -> I'.IO ()
{-# LINE 1694 "initial\FRH.abs" #-}
init'MonitoringQueryEndpointImpl this@(Obj' this' _ thisDC)
  = I'.pure ()

instance MonitoringQueryEndpoint' MonitoringQueryEndpointImpl where
        invokeWithDelay proctime customer amazonECU delay
          this@(Obj' this' _ thisDC)
          = do req :: IORef' Request <- I'.lift
                                          (I'.newIORef
                                             ((I'.fromIntegral proctime) *
                                                (I'.fromIntegral amazonECU)))
               start :: IORef' Time <- I'.lift (I'.newIORef =<< now)
               _ <- (this <..>) =<< I'.lift (I'.pure invoke <*> I'.readIORef req)
               end :: IORef' Time <- I'.lift (I'.newIORef =<< now)
               newProcTime :: IORef' Int <- I'.lift
                                              (I'.newIORef =<<
                                                 (I'.pure truncate <*>
                                                    ((-) <$!>
                                                       (I'.pure timeValue <*> I'.readIORef end)
                                                       <*>
                                                       (I'.pure timeValue <*> I'.readIORef start))))
               _ <- (\ this'' ->
                       (\ (DegradationMonitorIf obj') ->
                          awaitSugar'' this obj' =<<
                            I'.lift
                              (I'.pure notify_query_Mon <*> I'.readIORef start <*>
                                 I'.pure customer
                                 <*> (I'.fromIntegral <$!> I'.readIORef newProcTime)))
                         (monitor'MonitoringQueryEndpointImpl this''))
                      =<< I'.lift (I'.readIORef this')
               I'.pure ()

instance EndPoint' MonitoringQueryEndpointImpl where
        invoke request this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' ->
                     assert (not ((state'MonitoringQueryEndpointImpl this'') == STOP))
                       (I'.pure ()))
                    =<< I'.readIORef this')
               (\ this'' ->
                  I'.when ((current'MonitoringQueryEndpointImpl this'') == [])
                    (do I'.lift
                          (I'.writeIORef this' =<<
                             ((\ this'' ->
                                 this''{current'MonitoringQueryEndpointImpl =
                                          (allEndpoints'MonitoringQueryEndpointImpl this'')})
                                <$!> I'.readIORef this'))))
                 =<< I'.lift (I'.readIORef this')
               p :: IORef' EndPoint <- I'.lift
                                         ((\ this'' ->
                                             I'.newIORef
                                               (head (current'MonitoringQueryEndpointImpl this'')))
                                            =<< I'.readIORef this')
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        this''{current'MonitoringQueryEndpointImpl =
                                 (tail (current'MonitoringQueryEndpointImpl this''))})
                       <$!> I'.readIORef this'))
               tmp2024985246 :: IORef' (Fut Bool) <- I'.lift
                                                       (I'.newIORef =<<
                                                          ((\ (EndPoint obj') ->
                                                              (obj' <!> invoke request))
                                                             =<< I'.readIORef p))
               awaitFuture' this =<< I'.lift (I'.readIORef tmp2024985246)
               I'.lift (get =<< I'.readIORef tmp2024985246)
        setStatus status this@(Obj' this' _ thisDC)
          = do I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' -> this''{state'MonitoringQueryEndpointImpl = status})
                       <$!> I'.readIORef this'))
        getStatus this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' -> I'.pure (state'MonitoringQueryEndpointImpl this''))
                    =<< I'.readIORef this')

{-# LINE 1737 "initial\FRH.abs" #-}
data UtilityFunctionsImpl = UtilityFunctionsImpl{}

smart'UtilityFunctionsImpl :: UtilityFunctionsImpl
smart'UtilityFunctionsImpl = (UtilityFunctionsImpl)

init'UtilityFunctionsImpl :: Obj' UtilityFunctionsImpl -> I'.IO ()
{-# LINE 1737 "initial\FRH.abs" #-}
init'UtilityFunctionsImpl this@(Obj' this' _ thisDC) = I'.pure ()

instance UtilityFunctions' UtilityFunctionsImpl where
        getdc_instance_names ls this@(Obj' this' _ thisDC)
          = do ls_ :: IORef' (List DeploymentComponent) <- I'.lift
                                                             (I'.newIORef ls)
               rs :: IORef' (List String) <- I'.lift (I'.newIORef [])
               while ((not) <$!> ((==) <$!> I'.readIORef ls_ <*> I'.pure []))
                 (do dc :: IORef' DeploymentComponent <- I'.lift
                                                           (I'.newIORef =<<
                                                              (I'.pure head <*> I'.readIORef ls_))
                     s :: IORef' String <- I'.lift (I'.newIORef I'.undefined)
                     (\ (DeploymentComponent obj') ->
                        awaitSugar' this (I'.writeIORef s) obj' (getName))
                       =<< I'.lift (I'.readIORef dc)
                     I'.lift
                       (I'.writeIORef s =<< (I'.pure getInstanceName <*> I'.readIORef s))
                     I'.lift
                       (I'.writeIORef rs =<<
                          ((:) <$!> I'.readIORef s <*> I'.readIORef rs))
                     I'.lift
                       (I'.writeIORef ls_ =<< (I'.pure tail <*> I'.readIORef ls_)))
               I'.lift ((I'.pure reverse <*> I'.readIORef rs))

{-# LINE 1753 "initial\FRH.abs" #-}
data AddQueryServicesDeployerImpl = AddQueryServicesDeployerImpl{cloudProvider'AddQueryServicesDeployerImpl
                                                                 :: CloudProvider,
                                                                 depl_list'AddQueryServicesDeployerImpl
                                                                 :: List SmartDeployInterface,
                                                                 deploymentServiceObjEu'AddQueryServicesDeployerImpl
                                                                 :: DeploymentService,
                                                                 deploymentServiceObjUs'AddQueryServicesDeployerImpl
                                                                 :: DeploymentService,
                                                                 loadBalancerEndPointObjEu1'AddQueryServicesDeployerImpl
                                                                 :: LoadBalancerEndPoint,
                                                                 loadBalancerEndPointObjUs1'AddQueryServicesDeployerImpl
                                                                 :: LoadBalancerEndPoint,
                                                                 loadBalancerEndPointObjUs2'AddQueryServicesDeployerImpl
                                                                 :: LoadBalancerEndPoint,
                                                                 platformServiceObjEu'AddQueryServicesDeployerImpl
                                                                 :: MonitorPlatformService,
                                                                 platformServiceObjUs'AddQueryServicesDeployerImpl
                                                                 :: MonitorPlatformService}

smart'AddQueryServicesDeployerImpl ::
                                   CloudProvider ->
                                     MonitorPlatformService ->
                                       MonitorPlatformService ->
                                         DeploymentService ->
                                           DeploymentService ->
                                             LoadBalancerEndPoint ->
                                               LoadBalancerEndPoint ->
                                                 LoadBalancerEndPoint ->
                                                   AddQueryServicesDeployerImpl
smart'AddQueryServicesDeployerImpl cloudProvider'this
  platformServiceObjEu'this platformServiceObjUs'this
  deploymentServiceObjEu'this deploymentServiceObjUs'this
  loadBalancerEndPointObjEu1'this loadBalancerEndPointObjUs1'this
  loadBalancerEndPointObjUs2'this
  = (\ depl_list'this ->
       (AddQueryServicesDeployerImpl cloudProvider'this depl_list'this
          (up' deploymentServiceObjEu'this)
          (up' deploymentServiceObjUs'this)
          (up' loadBalancerEndPointObjEu1'this)
          (up' loadBalancerEndPointObjUs1'this)
          (up' loadBalancerEndPointObjUs2'this)
          (up' platformServiceObjEu'this)
          (up' platformServiceObjUs'this)))
      ([] :: List SmartDeployInterface)

init'AddQueryServicesDeployerImpl ::
                                  Obj' AddQueryServicesDeployerImpl -> I'.IO ()
{-# LINE 1753 "initial\FRH.abs" #-}
init'AddQueryServicesDeployerImpl this@(Obj' this' _ thisDC)
  = I'.pure ()

instance DeployerIF' AddQueryServicesDeployerImpl where
        scaleUp this@(Obj' this' _ thisDC)
          = do d :: IORef' SmartDeployInterface <- I'.lift
                                                     ((\ this'' ->
                                                         ((I'.newIORef . SmartDeployInterface) =<<
                                                            new thisDC init'AddQueryDeployer
                                                              (smart'AddQueryDeployer
                                                                 (cloudProvider'AddQueryServicesDeployerImpl
                                                                    this'')
                                                                 (up'
                                                                    (platformServiceObjEu'AddQueryServicesDeployerImpl
                                                                       this''))
                                                                 (up'
                                                                    (platformServiceObjUs'AddQueryServicesDeployerImpl
                                                                       this''))
                                                                 (up'
                                                                    (deploymentServiceObjEu'AddQueryServicesDeployerImpl
                                                                       this''))
                                                                 (up'
                                                                    (deploymentServiceObjUs'AddQueryServicesDeployerImpl
                                                                       this''))
                                                                 (up'
                                                                    (loadBalancerEndPointObjEu1'AddQueryServicesDeployerImpl
                                                                       this''))
                                                                 (up'
                                                                    (loadBalancerEndPointObjUs1'AddQueryServicesDeployerImpl
                                                                       this''))
                                                                 (up'
                                                                    (loadBalancerEndPointObjUs2'AddQueryServicesDeployerImpl
                                                                       this'')))))
                                                        =<< I'.readIORef this')
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{depl_list'AddQueryServicesDeployerImpl = v'}) <$!>
                          ((:) <$!> (up' <$!> I'.readIORef d) <*>
                             I'.pure (depl_list'AddQueryServicesDeployerImpl this'')))
                       =<< I'.readIORef this'))
               _ <- (\ (SmartDeployInterface obj') ->
                       awaitSugar'' this obj' deploy)
                      =<< I'.lift (I'.readIORef d)
               I'.pure ()
        scaleDown this@(Obj' this' _ thisDC)
          = do (\ this'' ->
                  I'.when
                    (not ((depl_list'AddQueryServicesDeployerImpl this'') == []))
                    (do d :: IORef' SmartDeployInterface <- I'.lift
                                                              ((\ this'' ->
                                                                  I'.newIORef
                                                                    (head
                                                                       (depl_list'AddQueryServicesDeployerImpl
                                                                          this'')))
                                                                 =<< I'.readIORef this')
                        I'.lift
                          (I'.writeIORef this' =<<
                             ((\ this'' ->
                                 this''{depl_list'AddQueryServicesDeployerImpl =
                                          (tail (depl_list'AddQueryServicesDeployerImpl this''))})
                                <$!> I'.readIORef this'))
                        _ <- (\ (SmartDeployInterface obj') ->
                                awaitSugar'' this obj' undeploy)
                               =<< I'.lift (I'.readIORef d)
                        I'.pure ()))
                 =<< I'.lift (I'.readIORef this')
main
  = main_is'
      (\ this@(Obj' _ _ thisDC) ->
         do cp :: IORef' CloudProvider <- I'.lift
                                            (((I'.newIORef . CloudProvider) =<<
                                                newlocal' this init'SimCloudProvider
                                                  (smart'SimCloudProvider "CloudProvider")))
            _ <- (\ (CloudProvider obj') ->
                    sync' this obj' addSmartDeployInstances)
                   =<< I'.lift (I'.readIORef cp)
            c1 :: IORef' SmartDeployInterface <- I'.lift
                                                   ((I'.newIORef . SmartDeployInterface) =<<
                                                      (newlocal' this init'MainSmartDeployer =<<
                                                         I'.pure smart'MainSmartDeployer <*>
                                                           I'.readIORef cp))
            _ <- (\ (SmartDeployInterface obj') -> sync' this obj' deploy) =<<
                   I'.lift (I'.readIORef c1)
            I'.lift (println "Initial Deployment Configuration set up")
            monitorPlatformServices_aux ::
              IORef'
                (List
                   (Pair MonitorPlatformService DeploymentComponent)) <- (I'.lift .
                                                                            I'.newIORef)
                                                                           =<<
                                                                           ((\ (SmartDeployInterface
                                                                                  obj')
                                                                               ->
                                                                               sync' this obj'
                                                                                 getMonitorPlatformService)
                                                                              =<<
                                                                              I'.lift
                                                                                (I'.readIORef c1))
            deploymentServices_aux ::
              IORef'
                (List (Pair DeploymentService DeploymentComponent)) <- (I'.lift .
                                                                          I'.newIORef)
                                                                         =<<
                                                                         ((\ (SmartDeployInterface
                                                                                obj')
                                                                             ->
                                                                             sync' this obj'
                                                                               getDeploymentService)
                                                                            =<<
                                                                            I'.lift
                                                                              (I'.readIORef c1))
            loadBalancerEndPoints_aux ::
              IORef'
                (List (Pair LoadBalancerEndPoint DeploymentComponent)) <- (I'.lift
                                                                             . I'.newIORef)
                                                                            =<<
                                                                            ((\ (SmartDeployInterface
                                                                                   obj')
                                                                                ->
                                                                                sync' this obj'
                                                                                  getLoadBalancerEndPoint)
                                                                               =<<
                                                                               I'.lift
                                                                                 (I'.readIORef c1))
            utility_obj :: IORef' UtilityFunctions <- I'.lift
                                                        (((I'.newIORef . UtilityFunctions) =<<
                                                            newlocal' this init'UtilityFunctionsImpl
                                                              smart'UtilityFunctionsImpl))
            string_list :: IORef' (List String) <- I'.lift (I'.newIORef [])
            dc_list :: IORef' (List DeploymentComponent) <- I'.lift
                                                              (I'.newIORef [])
            monitorPlatformServices ::
              IORef' (List MonitorPlatformService) <- I'.lift
                                                        (I'.newIORef =<<
                                                           (I'.pure fst_list <*>
                                                              I'.readIORef
                                                                monitorPlatformServices_aux))
            I'.lift
              (I'.writeIORef dc_list =<<
                 (I'.pure snd_list <*> I'.readIORef monitorPlatformServices_aux))
            (I'.lift . I'.writeIORef string_list) =<<
              ((\ (UtilityFunctions obj') ->
                  sync' this obj' =<<
                    I'.lift (I'.pure getdc_instance_names <*> I'.readIORef dc_list))
                 =<< I'.lift (I'.readIORef utility_obj))
            monitorPlatformServicesUs ::
              IORef' (List MonitorPlatformService) <- I'.lift
                                                        (I'.newIORef =<<
                                                           (I'.pure filter_lists_by_substr <*>
                                                              I'.readIORef monitorPlatformServices
                                                              <*> I'.readIORef string_list
                                                              <*> I'.pure "_us"))
            monitorPlatformServicesEu ::
              IORef' (List MonitorPlatformService) <- I'.lift
                                                        (I'.newIORef =<<
                                                           (I'.pure filter_lists_by_substr <*>
                                                              I'.readIORef monitorPlatformServices
                                                              <*> I'.readIORef string_list
                                                              <*> I'.pure "_eu"))
            deploymentServices :: IORef' (List DeploymentService) <- I'.lift
                                                                       (I'.newIORef =<<
                                                                          (I'.pure fst_list <*>
                                                                             I'.readIORef
                                                                               deploymentServices_aux))
            I'.lift
              (I'.writeIORef dc_list =<<
                 (I'.pure snd_list <*> I'.readIORef deploymentServices_aux))
            (I'.lift . I'.writeIORef string_list) =<<
              ((\ (UtilityFunctions obj') ->
                  sync' this obj' =<<
                    I'.lift (I'.pure getdc_instance_names <*> I'.readIORef dc_list))
                 =<< I'.lift (I'.readIORef utility_obj))
            deploymentServicesUs :: IORef' (List DeploymentService) <- I'.lift
                                                                         (I'.newIORef =<<
                                                                            (I'.pure
                                                                               filter_lists_by_substr
                                                                               <*>
                                                                               I'.readIORef
                                                                                 deploymentServices
                                                                               <*>
                                                                               I'.readIORef
                                                                                 string_list
                                                                               <*> I'.pure "_us"))
            deploymentServicesEu :: IORef' (List DeploymentService) <- I'.lift
                                                                         (I'.newIORef =<<
                                                                            (I'.pure
                                                                               filter_lists_by_substr
                                                                               <*>
                                                                               I'.readIORef
                                                                                 deploymentServices
                                                                               <*>
                                                                               I'.readIORef
                                                                                 string_list
                                                                               <*> I'.pure "_eu"))
            loadBalancerEndPoints ::
              IORef' (List LoadBalancerEndPoint) <- I'.lift
                                                      (I'.newIORef =<<
                                                         (I'.pure fst_list <*>
                                                            I'.readIORef loadBalancerEndPoints_aux))
            I'.lift
              (I'.writeIORef dc_list =<<
                 (I'.pure snd_list <*> I'.readIORef loadBalancerEndPoints_aux))
            (I'.lift . I'.writeIORef string_list) =<<
              ((\ (UtilityFunctions obj') ->
                  sync' this obj' =<<
                    I'.lift (I'.pure getdc_instance_names <*> I'.readIORef dc_list))
                 =<< I'.lift (I'.readIORef utility_obj))
            loadBalancerEndPointsUs ::
              IORef' (List LoadBalancerEndPoint) <- I'.lift
                                                      (I'.newIORef =<<
                                                         (I'.pure filter_lists_by_substr <*>
                                                            I'.readIORef loadBalancerEndPoints
                                                            <*> I'.readIORef string_list
                                                            <*> I'.pure "_us"))
            loadBalancerEndPointsEu ::
              IORef' (List LoadBalancerEndPoint) <- I'.lift
                                                      (I'.newIORef =<<
                                                         (I'.pure filter_lists_by_substr <*>
                                                            I'.readIORef loadBalancerEndPoints
                                                            <*> I'.readIORef string_list
                                                            <*> I'.pure "_eu"))
            deployerif :: IORef' DeployerIF <- I'.lift
                                                 ((I'.newIORef . DeployerIF) =<<
                                                    (new thisDC init'AddQueryServicesDeployerImpl
                                                       =<<
                                                       I'.pure smart'AddQueryServicesDeployerImpl
                                                         <*> I'.readIORef cp
                                                         <*>
                                                         (I'.pure head <*>
                                                            I'.readIORef monitorPlatformServicesEu)
                                                         <*>
                                                         (I'.pure head <*>
                                                            I'.readIORef monitorPlatformServicesUs)
                                                         <*>
                                                         (I'.pure head <*>
                                                            I'.readIORef deploymentServicesUs)
                                                         <*>
                                                         (I'.pure head <*>
                                                            I'.readIORef deploymentServicesEu)
                                                         <*>
                                                         (I'.pure head <*>
                                                            I'.readIORef loadBalancerEndPointsEu)
                                                         <*>
                                                         (I'.pure head <*>
                                                            I'.readIORef loadBalancerEndPointsUs)
                                                         <*>
                                                         (I'.pure head <*>
                                                            (I'.pure tail <*>
                                                               I'.readIORef
                                                                 loadBalancerEndPointsUs))))
            ms :: IORef' MonitoringService <- I'.lift
                                                (((I'.newIORef . MonitoringService) =<<
                                                    new thisDC init'MonitoringServiceImpl
                                                      smart'MonitoringServiceImpl))
            degradationMonitor :: IORef' DegradationMonitorIf <- I'.lift
                                                                   ((I'.newIORef .
                                                                       DegradationMonitorIf)
                                                                      =<<
                                                                      (new thisDC
                                                                         init'DegradationMonitorImpl
                                                                         =<<
                                                                         I'.pure
                                                                           smart'DegradationMonitorImpl
                                                                           <*>
                                                                           (up' <$!>
                                                                              I'.readIORef
                                                                                deployerif)))
            I'.lift
              ((\ v' ->
                  I'.atomicModifyIORef' apiStore'
                    (\ m' -> (I'.put m' "monitor" (I'.toDyn v'), ())))
                 =<< I'.readIORef degradationMonitor)
            df :: IORef' (Fut Unit) <- I'.lift
                                         (I'.newIORef =<<
                                            ((\ (MonitoringService obj') ->
                                                (obj' <!>) =<<
                                                  I'.pure addMS <*>
                                                    (I'.pure Rule_ <*> I'.pure 500 <*>
                                                       (up' <$!> I'.readIORef degradationMonitor)))
                                               =<< I'.readIORef ms))
            _ <- I'.lift (get =<< I'.readIORef df)
            mqep :: IORef' MonitoringQueryEndpoint <- I'.lift
                                                        ((I'.newIORef . MonitoringQueryEndpoint) =<<
                                                           (new thisDC
                                                              init'MonitoringQueryEndpointImpl
                                                              =<<
                                                              I'.pure
                                                                smart'MonitoringQueryEndpointImpl
                                                                <*>
                                                                 (I'.fmap up' <$!> I'.readIORef loadBalancerEndPoints)
                                                                <*>
                                                                (up' <$!>
                                                                   I'.readIORef
                                                                     degradationMonitor)))
            I'.lift
              ((\ v' ->
                  I'.atomicModifyIORef' apiStore'
                    (\ m' -> (I'.put m' "queryService" (I'.toDyn v'), ())))
                 =<< I'.readIORef mqep)
            I'.lift (println "Endpoints set up. Waiting for requests..."))
      (do I'.get "/call/:httpName'/invokeWithDelay"
            (do objs' <- I'.lift (I'.readIORef apiStore')
                httpName' <- I'.param "httpName'"
                case I'.lookup httpName' objs' of
                    Just obj' -> I'.json =<<
                                   case I'.fromDynamic obj' of
                                       Just
                                         (MonitoringQueryEndpoint obj'') -> do mapplied' <- I'.pure
                                                                                              invokeWithDelay
                                                                                              <*>
                                                                                              I'.param
                                                                                                "proctime"
                                                                                              <*>
                                                                                              I'.param
                                                                                                "customer"
                                                                                              <*>
                                                                                              I'.param
                                                                                                "amazonECU"
                                                                                              <*>
                                                                                              I'.param
                                                                                                "delay"
                                                                               I'.lift
                                                                                 (get =<<
                                                                                    obj'' <!>
                                                                                      mapplied')
                                       Nothing -> I'.raise "wrong interface"
                    Nothing -> I'.raise "no such object name")
          I'.get "/call/:httpName'/metricHistory"
            (do objs' <- I'.lift (I'.readIORef apiStore')
                httpName' <- I'.param "httpName'"
                case I'.lookup httpName' objs' of
                    Just obj' -> I'.json =<<
                                   case I'.fromDynamic obj' of
                                       Just (DegradationMonitorIf obj'') -> do mapplied' <- I'.pure
                                                                                              metricHistory
                                                                               I'.lift
                                                                                 (get =<<
                                                                                    obj'' <!>
                                                                                      mapplied')
                                       Nothing -> case I'.fromDynamic obj' of
                                                      Just
                                                        (Monitor obj'') -> do mapplied' <- I'.pure
                                                                                             metricHistory
                                                                              I'.lift
                                                                                (get =<<
                                                                                   obj'' <!>
                                                                                     mapplied')
                                                      Nothing -> I'.raise "wrong interface"
                    Nothing -> I'.raise "no such object name"))
