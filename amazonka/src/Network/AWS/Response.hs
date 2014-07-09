{-# LANGUAGE FlexibleContexts #-}

-- Module      : Network.AWS.Response
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Response where

import           Control.Applicative
import           Control.Error
import           Control.Exception            (Exception)
import           Control.Lens                 hiding (Action)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Aeson                   hiding (Error)
import qualified Data.Attoparsec.Text         as AText
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Base16       as Base16
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.ByteString.Lazy.Char8   as LBS8
import           Data.Char
import           Data.Conduit
import           Data.Conduit
import qualified Data.Conduit.Binary          as Conduit
import           Data.Default
import           Data.IORef
import           Data.Int
import           Data.Monoid
import           Data.String
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text
import           Data.Time
import           Data.Typeable
import           Network.AWS.Data
import           Network.AWS.Types
import           Network.HTTP.Client
import           Network.HTTP.Types

headerResponse :: (ClientError e, Monad m)
               => (ResponseHeaders -> Either e a)
               -> Either ClientException (ClientResponse m)
               -> m (Either e a)
headerResponse f = bodyResponse $ \hs bdy ->
    (bdy $$+- return ()) >> return (f hs)

xmlResponse :: (ClientError e, FromXML (ResponseHeaders -> Either String a), Monad m)
            => Either ClientException (ClientResponse m)
            -> m (Either e a)
xmlResponse = bodyResponse $ \hs bdy -> do
    lbs <- bdy $$+- Conduit.sinkLbs
    undefined

    -- return . fmapL clientError
    --        . join
    --        . fmap ($ hs)
    --        $ decodeXML lbs

bodyResponse :: (ClientError e, Monad m)
             => (ResponseHeaders -> b -> m (Either e a))
             -> Either ClientException (Response b)
             -> m (Either e a)
bodyResponse _ (Left  ex) = return . Left $ clientError ex
bodyResponse f (Right rs)
    | statusCode st >= 400 = return . Left $ clientError ex
    | otherwise            = f hs bdy
  where
    ex  = StatusCodeException st hs mempty
    st  = responseStatus rs
    hs  = responseHeaders rs
    bdy = responseBody rs