{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudTrail.Service
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.CloudTrail.Service where

import Data.Aeson
import GHC.Generics               (Generic)
import Network.AWS.Internal
import Network.AWS.Internal.Types (Service(..))

-- | Currently supported version (@2013-11-01@) of the @AWS CloudTrail@ service.
service :: Service
service = Service Global v4 "cloudtrail" "2013-11-01"

data CloudTrailError
    = InsufficientS3BucketPolicyException
    | InsufficientSnsTopicPolicyException
    | InvalidS3BucketNameException
    | InvalidS3PrefixException
    | InvalidSnsTopicNameException
    | InvalidTrailNameException
    | MaximumNumberOfTrailsExceededException
    | S3BucketDoesNotExistException
    | TrailAlreadyExistsException
    | TrailNotFoundException
    | TrailNotProvidedException
      deriving (Eq, Show, Generic)

instance FromJSON CloudTrailError
