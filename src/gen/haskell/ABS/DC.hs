{-# LINE 1 "frh\single_module\DC.abs" #-}
{-# LANGUAGE NoImplicitPrelude, ExistentialQuantification,
  MultiParamTypeClasses, ScopedTypeVariables, FlexibleContexts,
  PartialTypeSignatures, LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC
  -w -Werror -fforce-recomp -fwarn-missing-methods -fno-ignore-asserts#-}
module ABS.DC
       (SimDeploymentComponent(..), smart'SimDeploymentComponent,
        init'SimDeploymentComponent, CloudProvider'(..), CloudProvider(..),
        SimCloudProvider(..), smart'SimCloudProvider,
        init'SimCloudProvider)
       where
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
{-# LINE 5 "frh\single_module\DC.abs" #-}
import ABS.SetsMaps hiding (main)

default (Int, Rat)

{-# LINE 7 "frh\single_module\DC.abs" #-}
data SimDeploymentComponent = SimDeploymentComponent{description'SimDeploymentComponent
                                                     :: String,
                                                     initconfig'SimDeploymentComponent ::
                                                     Map Resourcetype Rat,
                                                     instrPS'SimDeploymentComponent :: Int}

smart'SimDeploymentComponent ::
                             String -> Map Resourcetype Rat -> SimDeploymentComponent
smart'SimDeploymentComponent description'this initconfig'this
  = (\ instrPS'this ->
       (SimDeploymentComponent description'this initconfig'this
          (I'.fromIntegral instrPS'this)))
      ((truncate (lookupDefault initconfig'this Speed 0)) :: Int)

init'SimDeploymentComponent ::
                            Obj' SimDeploymentComponent -> I'.IO ()
{-# LINE 7 "frh\single_module\DC.abs" #-}
init'SimDeploymentComponent this@(Obj' this' _ thisDC) = I'.pure ()

instance DeploymentComponent' SimDeploymentComponent where
        load rtype periods this@(Obj' this' _ thisDC)
          = do I'.lift (I'.pure 0)
        total rtype this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' ->
                     I'.pure
                       (case (lookup (initconfig'SimDeploymentComponent this'') rtype) of
                            Nothing -> InfRat
                            Just v -> (Fin v)))
                    =<< I'.readIORef this')
        transfer target amount rtype this@(Obj' this' _ thisDC)
          = I'.pure ()
        decrementResources amount rtype this@(Obj' this' _ thisDC)
          = I'.pure ()
        incrementResources amount rtype this@(Obj' this' _ thisDC)
          = I'.pure ()
        getName this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' -> I'.pure (description'SimDeploymentComponent this''))
                    =<< I'.readIORef this')
        getCreationTime this@(Obj' this' _ thisDC) = do I'.lift (now)
        getStartupDuration this@(Obj' this' _ thisDC)
          = do I'.lift (I'.pure 0)
        getShutdownDuration this@(Obj' this' _ thisDC)
          = do I'.lift (I'.pure 0)
        getPaymentInterval this@(Obj' this' _ thisDC)
          = do I'.lift (I'.pure 0)
        getCostPerInterval this@(Obj' this' _ thisDC)
          = do I'.lift (I'.pure 0)
        getNumberOfCores this@(Obj' this' _ thisDC)
          = do I'.lift (I'.pure 0)
        acquire this@(Obj' this' _ thisDC) = do I'.lift (I'.pure True)
        release this@(Obj' this' _ thisDC) = do I'.lift (I'.pure True)
        shutdown this@(Obj' this' _ thisDC) = do I'.lift (I'.pure True)
        request__ nrInstr this@(Obj' this' _ thisDC)
          = do (\ this'' ->
                  if
                    ((I'.fromIntegral nrInstr) >
                       (I'.fromIntegral (instrPS'SimDeploymentComponent this'')))
                    then
                    do I'.lift (duration 1 1)
                       suspend this
                       (\ this'' ->
                          this <..>
                            request__
                              ((I'.fromIntegral nrInstr) -
                                 (I'.fromIntegral (instrPS'SimDeploymentComponent this''))))
                         =<< I'.lift (I'.readIORef this')
                    else
                    do remaining :: IORef' Rat <- I'.lift
                                                    ((\ this'' ->
                                                        I'.newIORef
                                                          ((I'.fromIntegral nrInstr) /
                                                             (I'.fromIntegral
                                                                (instrPS'SimDeploymentComponent
                                                                   this''))))
                                                       =<< I'.readIORef this')
                       I'.lift
                         ((\ e1' -> duration e1' =<< I'.readIORef remaining) =<<
                            I'.readIORef remaining))
                 =<< I'.lift (I'.readIORef this')

{-# LINE 87 "frh\single_module\DC.abs" #-}
class CloudProvider' a where
        {-# LINE 91 "frh\single_module\DC.abs" #-}
        prelaunchInstance ::
                          Map Resourcetype Rat -> Obj' a -> ABS' DeploymentComponent
        
        {-# LINE 92 "frh\single_module\DC.abs" #-}
        launchInstance ::
                       Map Resourcetype Rat -> Obj' a -> ABS' DeploymentComponent
        
        {-# LINE 95 "frh\single_module\DC.abs" #-}
        acquireInstance :: DeploymentComponent -> Obj' a -> ABS' Bool
        
        {-# LINE 97 "frh\single_module\DC.abs" #-}
        shutdownInstance :: DeploymentComponent -> Obj' a -> ABS' Bool
        
        {-# LINE 107 "frh\single_module\DC.abs" #-}
        setInstanceDescriptions ::
                                Map String (Map Resourcetype Rat) -> Obj' a -> ABS' Unit
        
        {-# LINE 108 "frh\single_module\DC.abs" #-}
        addInstanceDescription ::
                               Pair String (Map Resourcetype Rat) -> Obj' a -> ABS' Unit
        
        {-# LINE 109 "frh\single_module\DC.abs" #-}
        removeInstanceDescription :: String -> Obj' a -> ABS' Unit
        
        {-# LINE 110 "frh\single_module\DC.abs" #-}
        getInstanceDescriptions ::
                                Obj' a -> ABS' (Map String (Map Resourcetype Rat))
        
        {-# LINE 111 "frh\single_module\DC.abs" #-}
        prelaunchInstanceNamed ::
                               String -> Obj' a -> ABS' DeploymentComponent
        
        {-# LINE 112 "frh\single_module\DC.abs" #-}
        launchInstanceNamed :: String -> Obj' a -> ABS' DeploymentComponent
        
        {-# LINE 114 "frh\single_module\DC.abs" #-}
        addSmartDeployInstances :: Obj' a -> ABS' Unit

data CloudProvider = forall a . CloudProvider' a =>
                       CloudProvider (Obj' a)

instance I'.Show CloudProvider where
        show _ = "CloudProvider"

instance I'.Eq CloudProvider where
        CloudProvider (Obj' ref1' _ _) == CloudProvider (Obj' ref2' _ _)
          = ref1' == I'.unsafeCoerce ref2'

instance CloudProvider' Null' where
        prelaunchInstance = I'.undefined
        launchInstance = I'.undefined
        acquireInstance = I'.undefined
        shutdownInstance = I'.undefined
        setInstanceDescriptions = I'.undefined
        addInstanceDescription = I'.undefined
        removeInstanceDescription = I'.undefined
        getInstanceDescriptions = I'.undefined
        prelaunchInstanceNamed = I'.undefined
        launchInstanceNamed = I'.undefined
        addSmartDeployInstances = I'.undefined

instance CloudProvider' a => Sub' (Obj' a) CloudProvider where
        up' = CloudProvider

{-# LINE 122 "frh\single_module\DC.abs" #-}
data SimCloudProvider = SimCloudProvider{acquiredInstances'SimCloudProvider
                                         :: List DeploymentComponent,
                                         instanceDescriptions'SimCloudProvider ::
                                         Map String (Map Resourcetype Rat),
                                         keeprunning'SimCloudProvider :: Bool,
                                         killedInstances'SimCloudProvider ::
                                         List DeploymentComponent,
                                         launchedInstances'SimCloudProvider ::
                                         List DeploymentComponent,
                                         name'SimCloudProvider :: String,
                                         nextInstanceId'SimCloudProvider :: Int}

smart'SimCloudProvider :: String -> SimCloudProvider
smart'SimCloudProvider name'this
  = (\ instanceDescriptions'this ->
       (\ launchedInstances'this ->
          (\ acquiredInstances'this ->
             (\ killedInstances'this ->
                (\ nextInstanceId'this ->
                   (\ keeprunning'this ->
                      (SimCloudProvider acquiredInstances'this instanceDescriptions'this
                         keeprunning'this
                         killedInstances'this
                         launchedInstances'this
                         name'this
                         (I'.fromIntegral nextInstanceId'this)))
                     (True :: Bool))
                  (0 :: Int))
               ((list []) :: List DeploymentComponent))
            ((list []) :: List DeploymentComponent))
         ((list []) :: List DeploymentComponent))
      ((map []) :: Map String (Map Resourcetype Rat))

init'SimCloudProvider :: Obj' SimCloudProvider -> I'.IO ()
{-# LINE 122 "frh\single_module\DC.abs" #-}
init'SimCloudProvider this@(Obj' this' _ thisDC) = I'.pure ()

instance CloudProvider' SimCloudProvider where
        prelaunchInstance d this@(Obj' this' _ thisDC)
          = do result :: IORef' DeploymentComponent <- (I'.lift .
                                                          I'.newIORef)
                                                         =<<
                                                         (\ this'' ->
                                                            (this <..>
                                                               createInstance''SimCloudProvider
                                                                 (name'SimCloudProvider this'')
                                                                 d))
                                                           =<< I'.lift (I'.readIORef this')
               startup_duration :: IORef' Rat <- I'.lift
                                                   (I'.newIORef I'.undefined)
               (\ (DeploymentComponent obj') ->
                  awaitSugar' this (I'.writeIORef startup_duration) obj'
                    (getStartupDuration))
                 =<< I'.lift (I'.readIORef result)
               (\ e1' ->
                  awaitDuration' this e1' =<<
                    I'.lift
                      ((minimum <$!> (I'.sequence [I'.readIORef startup_duration]))))
                 =<<
                 I'.lift
                   ((maximum <$!> (I'.sequence [I'.readIORef startup_duration])))
               I'.lift ((up' <$!> I'.readIORef result))
        launchInstance d this@(Obj' this' _ thisDC)
          = do result :: IORef' DeploymentComponent <- (I'.lift .
                                                          I'.newIORef)
                                                         =<< (this <..> prelaunchInstance d)
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{acquiredInstances'SimCloudProvider = v'}) <$!>
                          ((:) <$!> (up' <$!> I'.readIORef result) <*>
                             I'.pure (acquiredInstances'SimCloudProvider this'')))
                       =<< I'.readIORef this'))
               I'.lift ((up' <$!> I'.readIORef result))
        acquireInstance instance_ this@(Obj' this' _ thisDC)
          = do result :: IORef' Bool <- I'.lift (I'.newIORef True)
               (\ this'' ->
                  if
                    ((contains_ (acquiredInstances'SimCloudProvider this'')
                        (up' instance_))
                       ||
                       (contains_ (killedInstances'SimCloudProvider this'')
                          (up' instance_)))
                    then do I'.lift (I'.writeIORef result False) else
                    do I'.lift
                         (I'.writeIORef this' =<<
                            ((\ this'' ->
                                this''{acquiredInstances'SimCloudProvider =
                                         ((up' instance_) :
                                            (acquiredInstances'SimCloudProvider this''))})
                               <$!> I'.readIORef this')))
                 =<< I'.lift (I'.readIORef this')
               I'.lift (I'.readIORef result)
        shutdownInstance instance_ this@(Obj' this' _ thisDC)
          = do _ <- I'.lift
                      ((\ (DeploymentComponent obj') -> (obj' <!!> shutdown)) instance_)
               I'.lift (I'.pure True)
        setInstanceDescriptions instanceDescriptions
          this@(Obj' this' _ thisDC)
          = do I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        this''{instanceDescriptions'SimCloudProvider =
                                 instanceDescriptions})
                       <$!> I'.readIORef this'))
        addInstanceDescription instanceDescription
          this@(Obj' this' _ thisDC)
          = do I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        this''{instanceDescriptions'SimCloudProvider =
                                 (insert (instanceDescriptions'SimCloudProvider this'')
                                    instanceDescription)})
                       <$!> I'.readIORef this'))
        removeInstanceDescription instanceDescriptionName
          this@(Obj' this' _ thisDC)
          = do I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        this''{instanceDescriptions'SimCloudProvider =
                                 (removeKey (instanceDescriptions'SimCloudProvider this'')
                                    instanceDescriptionName)})
                       <$!> I'.readIORef this'))
        getInstanceDescriptions this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' ->
                     I'.pure (instanceDescriptions'SimCloudProvider this''))
                    =<< I'.readIORef this')
        prelaunchInstanceNamed instancename this@(Obj' this' _ thisDC)
          = do mconfig :: IORef' (Maybe (Map Resourcetype Rat)) <- I'.lift
                                                                     ((\ this'' ->
                                                                         I'.newIORef
                                                                           (lookup
                                                                              (instanceDescriptions'SimCloudProvider
                                                                                 this'')
                                                                              instancename))
                                                                        =<< I'.readIORef this')
               dc :: IORef' DeploymentComponent <- I'.lift
                                                     (I'.newIORef (up' null))
               when' <- I'.lift ((I'.pure isJust <*> I'.readIORef mconfig))
               I'.when when'
                 (do config :: IORef' (Map Resourcetype Rat) <- I'.lift
                                                                  (I'.newIORef =<<
                                                                     (I'.pure fromJust <*>
                                                                        I'.readIORef mconfig))
                     (I'.lift . I'.writeIORef dc) =<<
                       ((this <..>) =<<
                          I'.lift
                            ((\ this'' ->
                                I'.pure createInstance''SimCloudProvider <*>
                                  ((+) <$!>
                                     ((+) <$!> I'.pure (name'SimCloudProvider this'') <*>
                                        I'.pure "-")
                                     <*> I'.pure instancename)
                                  <*> I'.readIORef config)
                               =<< I'.readIORef this')))
               startup_duration :: IORef' Rat <- I'.lift
                                                   (I'.newIORef I'.undefined)
               (\ (DeploymentComponent obj') ->
                  awaitSugar' this (I'.writeIORef startup_duration) obj'
                    (getStartupDuration))
                 =<< I'.lift (I'.readIORef dc)
               (\ e1' ->
                  awaitDuration' this e1' =<<
                    I'.lift
                      ((minimum <$!> (I'.sequence [I'.readIORef startup_duration]))))
                 =<<
                 I'.lift
                   ((maximum <$!> (I'.sequence [I'.readIORef startup_duration])))
               I'.lift ((up' <$!> I'.readIORef dc))
        launchInstanceNamed instancename this@(Obj' this' _ thisDC)
          = do result :: IORef' DeploymentComponent <- (I'.lift .
                                                          I'.newIORef)
                                                         =<<
                                                         (this <..>
                                                            prelaunchInstanceNamed instancename)
               when' <- I'.lift
                          (((not) <$!>
                              ((==) (DeploymentComponent null) <$!>
                                 (up' <$!> I'.readIORef result))))
               I'.when when'
                 (do I'.lift
                       (I'.writeIORef this' =<<
                          ((\ this'' ->
                              (\ v' -> this''{acquiredInstances'SimCloudProvider = v'}) <$!>
                                ((:) <$!> (up' <$!> I'.readIORef result) <*>
                                   I'.pure (acquiredInstances'SimCloudProvider this'')))
                             =<< I'.readIORef this')))
               I'.lift ((up' <$!> I'.readIORef result))
        addSmartDeployInstances this@(Obj' this' _ thisDC)
          = do _ <- this <..>
                      addInstanceDescription
                        (("c4_2xlarge_eu",
                          (map
                             (((CostPerInterval, 419)) :
                                (((Cores, 8)) : (((Memory, 1500)) : (((Speed, 31)) : [])))))))
               _ <- this <..>
                      addInstanceDescription
                        (("m4_large_eu",
                          (map
                             (((CostPerInterval, 120)) :
                                (((Cores, 2)) : (((Memory, 800)) : (((Speed, 6)) : [])))))))
               _ <- this <..>
                      addInstanceDescription
                        (("m4_xlarge_eu",
                          (map
                             (((CostPerInterval, 239)) :
                                (((Cores, 4)) : (((Memory, 1600)) : (((Speed, 13)) : [])))))))
               _ <- this <..>
                      addInstanceDescription
                        (("m4_large_us2",
                          (map
                             (((CostPerInterval, 120)) :
                                (((Cores, 2)) : (((Memory, 800)) : (((Speed, 6)) : [])))))))
               _ <- this <..>
                      addInstanceDescription
                        (("c4_2xlarge_us1",
                          (map
                             (((CostPerInterval, 419)) :
                                (((Cores, 8)) : (((Memory, 1500)) : (((Speed, 31)) : [])))))))
               _ <- this <..>
                      addInstanceDescription
                        (("c4_2xlarge_us2",
                          (map
                             (((CostPerInterval, 419)) :
                                (((Cores, 8)) : (((Memory, 1500)) : (((Speed, 31)) : [])))))))
               _ <- this <..>
                      addInstanceDescription
                        (("m4_large_us1",
                          (map
                             (((CostPerInterval, 120)) :
                                (((Cores, 2)) : (((Memory, 800)) : (((Speed, 6)) : [])))))))
               _ <- this <..>
                      addInstanceDescription
                        (("c4_xlarge_eu",
                          (map
                             (((CostPerInterval, 209)) :
                                (((Cores, 4)) : (((Memory, 750)) : (((Speed, 16)) : [])))))))
               _ <- this <..>
                      addInstanceDescription
                        (("c4_xlarge_us1",
                          (map
                             (((CostPerInterval, 209)) :
                                (((Cores, 4)) : (((Memory, 750)) : (((Speed, 16)) : [])))))))
               _ <- this <..>
                      addInstanceDescription
                        (("c4_xlarge_us2",
                          (map
                             (((CostPerInterval, 209)) :
                                (((Cores, 4)) : (((Memory, 750)) : (((Speed, 16)) : [])))))))
               _ <- this <..>
                      addInstanceDescription
                        (("m4_xlarge_us2",
                          (map
                             (((CostPerInterval, 239)) :
                                (((Cores, 4)) : (((Memory, 1600)) : (((Speed, 13)) : [])))))))
               _ <- this <..>
                      addInstanceDescription
                        (("m4_xlarge_us1",
                          (map
                             (((CostPerInterval, 239)) :
                                (((Cores, 4)) : (((Memory, 1600)) : (((Speed, 13)) : [])))))))
               I'.pure ()

createInstance''SimCloudProvider ::
                                 String ->
                                   Map Resourcetype Rat ->
                                     Obj' SimCloudProvider -> ABS' DeploymentComponent
createInstance''SimCloudProvider instancename d
  this@(Obj' this' _ thisDC)
  = do result :: IORef' DeploymentComponent <- I'.lift
                                                 ((\ this'' ->
                                                     ((I'.newIORef . DeploymentComponent) =<<
                                                        new thisDC init'SimDeploymentComponent
                                                          (smart'SimDeploymentComponent
                                                             ((instancename + "-") +
                                                                (toString
                                                                   (I'.fromIntegral
                                                                      (nextInstanceId'SimCloudProvider
                                                                         this''))))
                                                             d)))
                                                    =<< I'.readIORef this')
       I'.lift
         (I'.writeIORef this' =<<
            ((\ this'' ->
                this''{nextInstanceId'SimCloudProvider =
                         ((I'.fromIntegral (nextInstanceId'SimCloudProvider this'')) + 1)})
               <$!> I'.readIORef this'))
       stupidTypeSystem :: IORef' DeploymentComponent <- I'.lift
                                                           (I'.newIORef =<<
                                                              (up' <$!> I'.readIORef result))
       I'.lift
         (I'.writeIORef this' =<<
            ((\ this'' ->
                (\ v' -> this''{launchedInstances'SimCloudProvider = v'}) <$!>
                  ((:) <$!> (up' <$!> I'.readIORef stupidTypeSystem) <*>
                     I'.pure (launchedInstances'SimCloudProvider this'')))
               =<< I'.readIORef this'))
       I'.lift ((up' <$!> I'.readIORef result))

contains_ :: forall a . _ => List a -> a -> Bool
{-# LINE 320 "frh\single_module\DC.abs" #-}
contains_ xs y
  = case xs of
        [] -> False
        (x_ : xs_) -> ((x_ == y) || (contains_ xs_ y))
