{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.UpdateLoginProfile
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Changes the password for the specified user. https://iam.amazonaws.com/
-- ?Action=UpdateLoginProfile &UserName=Bob &Password=NewPassword &AUTHPARAMS
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.UpdateLoginProfile where

import           Control.Monad
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import           Text.XML.Generic

import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
updateLoginProfile :: Text
                   -> Text
                   -> UpdateLoginProfile
updateLoginProfile p1 p2 = UpdateLoginProfile
    { ulprPassword = p1
    , ulprUserName = p2
    }

data UpdateLoginProfile = UpdateLoginProfile
    { ulprPassword :: !Text
      -- ^ The new password for the user name.
    , ulprUserName :: !Text
      -- ^ Name of the user whose password you want to update.
    } deriving (Eq, Show, Generic)

instance ToQuery UpdateLoginProfile

instance AWSRequest UpdateLoginProfile where
    type Er UpdateLoginProfile = IAMError
    type Rs UpdateLoginProfile = UpdateLoginProfileResponse
    request = getQuery service "UpdateLoginProfile"

data UpdateLoginProfileResponse = UpdateLoginProfileResponse
    deriving (Eq, Show, Generic)

instance FromXML UpdateLoginProfileResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot UpdateLoginProfileResponse
