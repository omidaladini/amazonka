{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.Service
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.S3.Service where

import Network.AWS.Core
import Network.AWS.Generics.XML

-- | Currently supported version (@2006-03-01@) of the @Amazon Simple Storage Service@ service.
service :: ByteString -> Service
service b = Service Global (s3 b) "s3" "2006-03-01"

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlInherit   = True
    , xmlNamespace = Just "http://s3.amazonaws.com/doc/2006-03-01/"
    }

data S3Error
    = BucketAlreadyExists
    | NoSuchBucket
    | NoSuchKey
    | NoSuchUpload
    | ObjectAlreadyInActiveTierError
    | ObjectNotInActiveTierError
      deriving (Eq, Show, Generic)

instance FromXML S3Error where
    fromXMLOptions = xmlOptions
