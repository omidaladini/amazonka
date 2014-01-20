{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Network.HTTP.QueryString.Instances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.HTTP.QueryString.Instances where

import           Control.Monad
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Char8           as BS
import           Data.List.NonEmpty              (NonEmpty(..))
import qualified Data.List.NonEmpty              as NonEmpty
import           Data.Text.Helpers
import           Data.Time
import           Data.Time.Formatters
import           Network.HTTP.QueryString.Pickle

primQuery :: (FromText a, ToText a) => QueryPU a
primQuery = QueryPU p u
  where
    p = pickle queryPickler . toText
    u = join . fmap fromText . unpickle queryPickler

instance IsQuery a => IsQuery [a] where
    queryPickler = qpOrdinalList queryPickler

-- FIXME: Unsafe
instance IsQuery a => IsQuery (NonEmpty a) where
    queryPickler = (NonEmpty.fromList, NonEmpty.toList) `qpWrap` queryPickler

instance IsQuery () where
    queryPickler = qpLift ()

instance IsQuery Bool where
    queryPickler = QueryPU (Value . lowerBool) u
      where
        u (Value s) = parseBool s
        u e         = Left $ "unable to encode Bool from: " ++ show e

instance IsQuery UTCTime where
    queryPickler = QueryPU (Value . formatISO8601) u
      where
        u (Value s) = parseISO8601 $ BS.unpack s
        u o         = Left $ "unable to parse ISO8601 time from: " ++ show o

instance IsQuery Double where
    queryPickler = qpPrim

lowerBool :: Bool -> ByteString
lowerBool True  = "true"
lowerBool False = "false"

parseBool :: ByteString -> Either String Bool
parseBool "true"  = Right True
parseBool "false" = Right False
parseBool e       = Left $ "unable to parse Bool from: " ++ show e
