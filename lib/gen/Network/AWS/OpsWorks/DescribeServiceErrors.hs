{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.DescribeServiceErrors
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes AWS OpsWorks service errors. Required Permissions: To use this
-- action, an IAM user must have a Show, Deploy, or Manage permissions level
-- for the stack, or an attached policy that explicitly grants permissions.
-- For more information on user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.DescribeServiceErrors where

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

import Network.AWS.OpsWorks.Service
import Network.AWS.OpsWorks.Types

-- | Convenience method utilising default fields where applicable.
describeServiceErrors :: AWS (Either OpsWorksError DescribeServiceErrorsResponse)
describeServiceErrors = undefined $ DescribeServiceErrors
    { dserInstanceId = Nothing
    , dserServiceErrorIds = []
    , dserStackId = Nothing
    }

data DescribeServiceErrors = DescribeServiceErrors
    { dserInstanceId :: Maybe Text
      -- ^ The instance ID. If you use this parameter, DescribeServiceErrors returns
      -- descriptions of the errors associated with the specified instance.
    , dserServiceErrorIds :: [Text]
      -- ^ An array of service error IDs. If you use this parameter,
      -- DescribeServiceErrors returns descriptions of the specified errors.
      -- Otherwise, it returns a description of every error.
    , dserStackId :: Maybe Text
      -- ^ The stack ID. If you use this parameter, DescribeServiceErrors returns
      -- descriptions of the errors associated with the specified stack.
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeServiceErrors

instance AWSRequest DescribeServiceErrors where
    type Er DescribeServiceErrors = OpsWorksError
    type Rs DescribeServiceErrors = DescribeServiceErrorsResponse
    request  = getJSON service
    response = responseJSON

data DescribeServiceErrorsResponse = DescribeServiceErrorsResponse
    { dserrsServiceErrors :: [ServiceError]
      -- ^ An array of ServiceError objects that describe the specified service
      -- errors.
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeServiceErrorsResponse
