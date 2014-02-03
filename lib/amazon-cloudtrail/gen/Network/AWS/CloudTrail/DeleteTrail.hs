{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudTrail.DeleteTrail
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a trail.
module Network.AWS.CloudTrail.DeleteTrail where

import           Control.Monad
import           Data.Aeson
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.CloudTrail.Service
import Network.AWS.CloudTrail.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteTrail :: Text
            -> DeleteTrail
deleteTrail p1 = DeleteTrail
    { dtsName = p1
    }

data DeleteTrail = DeleteTrail
    { dtsName :: !Text
      -- ^ The name of a trail to be deleted.
    } deriving (Eq, Show, Generic)

instance ToJSON DeleteTrail where
    toJSON = genericToJSON jsonOptions

instance AWSRequest DeleteTrail where
    type Er DeleteTrail = CloudTrailError
    type Rs DeleteTrail = DeleteTrailResponse
    request  = getJSON service
    response = responseJSON

data DeleteTrailResponse = DeleteTrailResponse
    deriving (Eq, Show, Generic)

instance FromJSON DeleteTrailResponse where
    fromJSON = genericFromJSON jsonOptions

