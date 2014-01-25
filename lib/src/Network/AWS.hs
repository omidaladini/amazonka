{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Network.AWS
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
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
    -- * Specifying credentials
      Credentials (..)
    , accessKey
    , secretKey

    -- * Running AWS actions
    , AWS
    , runAWS

    -- * Controlling the region
    , Region      (..)
    , within
    , getRegion

    -- * Debugging
    , whenDebug
    , getDebug

    -- * Sending requests
    , send
    , send_
    , sendCatch
    , sendAsync

    -- * Paginating requests
    , paginate
    , paginateCatch

    -- * Asynchronous actions
    , async
    , wait
    , wait_
    , waitCatch

    -- * Errors
    , AWSErrors   (..)
    , awsThrow
    , awsEitherT
    , awsEither

    -- * File bodies
    , requestBodyFile

    -- * Types
    , module Network.AWS.Internal.Types.Common

    -- * Type Classes
    , AWSRequest
    , AWSPager
    , AWSError
    ) where

import           Control.Applicative
import qualified Control.Concurrent.Async              as Async
import           Control.Error
import qualified Control.Exception                     as Ex
import qualified Control.Exception.Lifted              as Lifted
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.Resource.Internal
import           Data.Conduit
import qualified Data.Conduit.Binary                   as Conduit
import           Data.Default
import           Data.String
import           Network.AWS.Internal
import           Network.AWS.Internal.Credentials
import           Network.AWS.Internal.Types.Common
import           Network.HTTP.Conduit
import           System.IO

runAWS :: Credentials -> Bool -> AWS a -> IO (Either AWSErrors a)
runAWS cred dbg aws = runResourceT . withInternalState $ \s -> do
    m <- newManager conduitManagerSettings
    a <- runEitherT $ credentials cred
    either (return . Left)
           (runEnv aws . Env def dbg s m)
           (fromString `fmapL` a)

runEnv :: AWS a -> Env -> IO (Either AWSErrors a)
runEnv aws = runEitherT . runReaderT (unwrap aws)

within :: Region -> AWS a -> AWS a
within reg = AWS . local (\e -> e { awsRegion = reg }) . unwrap

send :: (AWSRequest a, AWSError (Er a)) => a -> AWS (Rs a)
send = awsEither <=< sendCatch

send_ :: (AWSRequest a, AWSError (Er a)) => a -> AWS ()
send_ = void . send

sendAsync :: AWSRequest a
          => a
          -> AWS (Async.Async (Either AWSErrors (Either (Er a) (Rs a))))
sendAsync = async . sendCatch

sendCatch :: AWSRequest a => a -> AWS (Either (Er a) (Rs a))
sendCatch rq = do
    s  <- sign $ request rq
    whenDebug . liftIO $ print s
    m  <- getManager
    rs <- http s m
    response rq rs

paginate :: (AWSRequest a, AWSPager a, AWSError (Er a))
         => a
         -> Source AWS (Rs a)
paginate = ($= go) . paginateCatch
  where
    go = do
        x <- await
        maybe (return ())
              (either (lift . awsThrow) yield)
              x

paginateCatch :: (AWSRequest a, AWSPager a, AWSError (Er a))
              => a
              -> Source AWS (Either (Er a) (Rs a))
paginateCatch = go . Just
  where
    go Nothing   = return ()
    go (Just rq) = do
        rs <- lift $ sendCatch rq
        yield rs
        either (const $ return ())
               (go . next rq)
               rs

async :: AWS a -> AWS (Async.Async (Either AWSErrors a))
async aws = AWS ask >>= resourceAsync . lift . runEnv aws

wait :: AWSError e => Async.Async (Either e a) -> AWS a
wait = awsEither <=< waitCatch

wait_ :: AWSError e => Async.Async (Either e a) -> AWS ()
wait_ = void . wait

waitCatch :: AWSError e => Async.Async (Either e a) -> AWS (Either e a)
waitCatch a = liftIO (Async.waitCatch a) >>= either awsThrow return

resourceAsync :: MonadResource m => ResourceT IO a -> m (Async.Async a)
resourceAsync (ResourceT f) =
    liftResourceT . ResourceT $ \g -> Lifted.mask $ \h ->
        Ex.bracket_ (stateAlloc g)
                    (return ())
                    (Async.async $ Ex.bracket_
                        (return ())
                        (stateCleanup g)
                        (h $ f g))

requestBodyFile :: MonadIO m => FilePath -> m (Maybe RequestBody)
requestBodyFile f = runMaybeT $ do
    n <- join . hushT $ syncIO getFileSize
    return . requestBodySource n $ Conduit.sourceFile f
  where
    getFileSize = hoistMaybe <$>
        Ex.bracket (openBinaryFile f ReadMode)
                   hClose
                   (fmap (Just . fromIntegral) . hFileSize)
