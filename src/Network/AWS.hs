{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Network.AWS
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- |
module Network.AWS
    (
    -- * AWS Context
      AWS
    , runAWS

    -- * Credentials
    , Credentials      (..)

    -- * Regions
    , Region           (..)
    , within
    , getRegion

    -- * Debugging
    , getDebug
    , whenDebug

    -- * Synchronous Requests
    , send
    , send_
    , sendCatch

    -- * Asynchronous Actions
    , async
    , wait
    , wait_

    -- * Asynchronous Requests
    , sendAsync
    , waitAsync
    , waitAsync_

    -- -- * Paginated Requests
    -- , paginate
    -- , paginateCatch

    -- * File Bodies
    , requestBodyFile

    -- * Errors
    , ToError          (..)
    , AWSError         (..)
    , hoistError

    -- * Types
    , AvailabilityZone (..)
    , InstanceType     (..)
    , Items            (..)
    , Members          (..)
    ) where

import qualified Control.Concurrent.Async              as A
import           Control.Error
import           Control.Exception
import qualified Control.Exception.Lifted              as Lifted
import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.Error
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.Resource.Internal
import           Data.Conduit
import qualified Data.Conduit.Binary                   as Conduit
import           Network.AWS.Auth
import           Network.AWS.Internal
import           Network.HTTP.Conduit
import           System.IO

runAWS :: (MonadIO m, MonadBaseControl IO m)
       => Credentials
       -> Bool
       -> AWS m a
       -> m (Either AWSError a)
runAWS cred dbg aws = runResourceT . withInternalState $ \s -> do
    m <- liftIO $ newManager conduitManagerSettings
    a <- runEitherT $ credentials cred
    either (return . Left)
           (runEnv aws . Env defaultRegion dbg s m)
           a

runEnv :: MonadIO m => AWS m a -> Env -> m (Either AWSError a)
runEnv aws = runEitherT . runReaderT (unwrap aws)

-- | Run an 'AWS' operation inside a specific 'Region'.
within :: Monad m => Region -> AWS m a -> AWS m a
within reg = AWS . local (\e -> e { awsRegion = reg }) . unwrap

hoistError :: (MonadError e m, Error e) => Either e a -> m a
hoistError = either throwError return

-- | Send a request and return the associated response type.
send :: (MonadIO m, MonadUnsafeIO m, MonadThrow m, Rq a, ToError (Er a))
     => a
     -> AWS m (Rs a)
send = (hoistError . fmapL toError =<<) . sendCatch

send_ :: (MonadIO m, MonadUnsafeIO m, MonadThrow m, Rq a, ToError (Er a))
      => a
      -> AWS m ()
send_ = void . send

sendCatch :: (MonadIO m, MonadUnsafeIO m, MonadThrow m, Rq a)
          => a
          -> AWS m (Either (Er a) (Rs a))
sendCatch rq = do
    s  <- sign $ request rq
    whenDebug . liftIO $ print s
    m  <- getManager
    h  <- http s m
    whenDebug . liftIO $ print h
    rs <- response rq h
    hoistError rs

async :: (MonadIO m, MonadUnsafeIO m, MonadThrow m)
      => AWS IO a
      -> AWS m (A.Async (Either AWSError a))
async aws = AWS ask >>= resourceAsync . lift . runEnv aws

wait :: MonadIO m => A.Async (Either AWSError a) -> AWS m a
wait a = liftIO (A.waitCatch a) >>= hoistError . join . fmapL toError

wait_ :: MonadIO m => A.Async (Either AWSError a) -> AWS m ()
wait_ = void . wait

sendAsync :: (MonadIO m, MonadUnsafeIO m, MonadThrow m, Rq a)
          => a
          -> AWS m (A.Async (Either AWSError (Either (Er a) (Rs a))))
sendAsync = async . sendCatch

waitAsync :: (MonadIO m, ToError e)
          => A.Async (Either AWSError (Either e a))
          -> AWS m a
waitAsync a = wait a >>= hoistError . fmapL toError

waitAsync_ :: (MonadIO m, ToError e)
           => A.Async (Either AWSError (Either e a))
           -> AWS m ()
waitAsync_ = void . waitAsync

-- -- | Create a 'Source' which yields the initial and subsequent repsonses
-- -- for requests that support pagination.
-- -- paginate :: (MonadIO m, Rq a, Pg a, ToError (Er a))
-- --          => a
-- --          -> Source (AWS m) (Rs a)
-- paginate = ($= go) . paginateCatch
--   where
--     go = do
--         x <- await
--         maybe (return ())
--               (either (lift . lift . left . toError) yield)
--               x

-- -- liftEitherT :: (Monad m, ToError e) => EitherT e m a -> AWS m a
-- -- liftEitherT = lift . lift . fmapLT toError

-- paginateCatch :: (MonadIO m, Rq a, Pg a, ToError (Er a))
--               => a
--               -> Source (AWS m) (Either (Er a) (Rs a))
-- paginateCatch = go . Just
--   where
--     go Nothing   = return ()
--     go (Just rq) = do
--         rs <- lift $ sendCatch rq
--         yield rs
--         either (const $ return ()) (go . next rq) rs

resourceAsync :: (MonadIO m, MonadUnsafeIO m, MonadThrow m)
              => ResourceT IO a
              -> AWS m (A.Async a)
resourceAsync (ResourceT f) = liftResourceT . ResourceT $ \g -> Lifted.mask $ \h ->
    bracket_
        (stateAlloc g)
        (return ())
        (A.async $ bracket_
            (return ())
            (stateCleanup g)
            (h $ f g))

requestBodyFile :: MonadIO m => FilePath -> m (Maybe RequestBody)
requestBodyFile f = runMaybeT $ do
    n <- join . hushT $ syncIO getFileSize
    return . requestBodySource n $ Conduit.sourceFile f
  where
    getFileSize = fmap hoistMaybe $
        bracket (openBinaryFile f ReadMode)
                hClose
                (fmap (Just . fromIntegral) . hFileSize)
