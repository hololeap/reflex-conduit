{-# LANGUAGE FlexibleContexts #-}

module Reflex.Conduit (
      ResetConduitEvent (..)
    , ClearInputEvent (..)
    , runConduitReflex
    ) where    

import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, writeTChan, readTChan, tryReadTChan)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVarIO, putTMVar, takeTMVar, tryTakeTMVar)
import Control.Monad (forever, (<=<))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.STM (STM, atomically)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Conduit (ConduitT, runConduit, awaitForever, yield, (.|))
import Data.Functor (void)
import Reflex.Class (Event)
import Reflex.PerformEvent.Class (PerformEvent, Performable, performEvent_)
import Reflex.TriggerEvent.Class (TriggerEvent, newEventWithLazyTriggerWithOnComplete)

newtype ResetConduitEvent t = 
    ResetConduitEvent { getResetConduitEvent :: Event t () }
newtype ClearInputEvent t =
    ClearInputEvent { getClearInputEvent :: Event t () }


-- | Takes three Events and a Conduit.
-- | Feeds input from the third Event to the Conduit. Fires the created Event
-- |   when the Conduit produces output.
-- |
-- | - The 1st event resets the current `runConduit`, causing it to start again.
-- | - The 2nd event clears the input buffer.
-- | - Values from the 3rd event are fed into the Conduit when it fires.
runConduitReflex ::
    (TriggerEvent t m, PerformEvent t m, MonadIO (Performable m), MonadIO m)
    => ResetConduitEvent t -> ClearInputEvent t -> Event t i 
    -> ConduitT i o IO () -> m (Event t o)
runConduitReflex resetEvent clearEvent inEvent conduit = do
    inChan         <- liftIO newTChanIO
    innerAsyncMVar <- liftIO newEmptyTMVarIO

    writeEventToChan inEvent inChan
    cancelOnResetEvent resetEvent innerAsyncMVar
    clearOnClearEvent  clearEvent inChan

    asyncRunConduit inChan innerAsyncMVar conduit



-- | Calls `runConduit` asynchronously. Input coming in from the TChan will
-- |   be passed to the Conduit. Whenever the Conduit presents output, the
-- |   resulting Event will be fired.
-- | 
-- | Internally: The inner Async is pushed to the TMVar whenever it is created.
-- |   (This allows for it to be cancelled by another thread.)
-- |
-- | When the Event is no longer needed, the outer Async will be cancelled as
-- |   well as the inner Async (if it exits)
asyncRunConduit :: (TriggerEvent t m)
    => TChan i -> TMVar (Async ()) -> ConduitT i o IO () -> m (Event t o)
asyncRunConduit inChan innerAsyncMVar conduit =
    newEventWithLazyTriggerWithOnComplete $ \trigger -> do
        outerAsync <- (async . forever) $ do
            innerAsync <- (async . runConduit) $
                   (forever $ (liftIO . atomically . readTChan) inChan >>= yield)
                .| conduit
                .| awaitForever (\o -> liftIO $ trigger o $ pure ())
            atomically $ putTMVar innerAsyncMVar innerAsync
        pure $ do
            maybeInnerAsync <- atomically $ tryTakeTMVar innerAsyncMVar
            maybe (pure ()) cancel maybeInnerAsync
            cancel outerAsync
    
-- | Whenever the passed Event is fired, the TChan will be cleared.
clearOnClearEvent :: (PerformEvent t m, MonadIO (Performable m))
    => ClearInputEvent t -> TChan i -> m ()
clearOnClearEvent (ClearInputEvent clearE) inChan =
    performEvent_ $ (liftIO . atomically . clearTChan) inChan <$ clearE

-- | Whenver the passed Event is fired, the Async will be taken from the passed
-- | TMVar; `cancel` will then be called on the Async.
cancelOnResetEvent :: (PerformEvent t m, MonadIO (Performable m))
    => ResetConduitEvent t -> TMVar (Async ()) -> m ()
cancelOnResetEvent (ResetConduitEvent resetE) tmvar =
    performEvent_ $ (liftIO . (cancel <=< atomically . takeTMVar)) tmvar <$ resetE

-- | Whenver the Event fires, its value will be pushed onto the TChan
writeEventToChan :: (PerformEvent t m, MonadIO (Performable m))
    => Event t a -> TChan a -> m ()
writeEventToChan inEvent tchan = 
    performEvent_ $ liftIO . atomically . writeTChan tchan <$> inEvent

-- | Clear a TChan (read all elements)
clearTChan :: TChan a -> STM ()
clearTChan = void . runMaybeT . mapM_ (MaybeT . tryReadTChan) . repeat
