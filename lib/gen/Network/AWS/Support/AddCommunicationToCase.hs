{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Support.AddCommunicationToCase
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds additional customer communication to an AWS Support case. You use the
-- CaseId value to identify the case to add communication to. You can list a
-- set of email addresses to copy on the communication using the
-- CcEmailAddresses value. The CommunicationBody value contains the text of
-- the communication. The response indicates the success or failure of the
-- request. This operation implements a subset of the behavior on the AWS
-- Support Your Support Cases web form.
module Network.AWS.Support.AddCommunicationToCase where

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

import Network.AWS.Support.Service
import Network.AWS.Support.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
addCommunicationToCase :: Text
                       -> AddCommunicationToCase
addCommunicationToCase p1 = undefined $ AddCommunicationToCase
    { actcrCommunicationBody = p1
    , actcrCaseId = Nothing
    , actcrCcEmailAddresses = []
    }

data AddCommunicationToCase = AddCommunicationToCase
    { actcrCaseId :: Maybe Text
      -- ^ The AWS Support case ID requested or returned in the call. The case ID is
      -- an alphanumeric string formatted as shown in this example:
      -- case-12345678910-2013-c4c1d2bf33c5cf47.
    , actcrCcEmailAddresses :: [Text]
      -- ^ The email addresses in the CC line of an email to be added to the support
      -- case.
    , actcrCommunicationBody :: !Text
      -- ^ The body of an email communication to add to the support case.
    } deriving (Eq, Show, Generic)

instance ToJSON AddCommunicationToCase

instance AWSRequest AddCommunicationToCase where
    type Er AddCommunicationToCase = SupportError
    type Rs AddCommunicationToCase = AddCommunicationToCaseResponse
    request  = getJSON service
    response = responseJSON

data AddCommunicationToCaseResponse = AddCommunicationToCaseResponse
    { actcrrsResult :: Maybe Bool
      -- ^ True if AddCommunicationToCase succeeds. Otherwise, returns an error.
    } deriving (Eq, Show, Generic)

instance FromJSON AddCommunicationToCaseResponse
