{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Support.V2013_04_15.AddCommunicationToCase
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
module Network.AWS.Support.V2013_04_15.AddCommunicationToCase
    (
    -- * Request
      AddCommunicationToCase
    -- ** Request constructor
    , addCommunicationToCase
    -- ** Request lenses
    , actcrCommunicationBody
    , actcrAttachmentSetId
    , actcrCaseId
    , actcrCcEmailAddresses

    -- * Response
    , AddCommunicationToCaseResponse
    -- ** Response lenses
    , actcsResult
    ) where

import           Network.AWS.Support.V2013_04_15.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'AddCommunicationToCase' request.
addCommunicationToCase :: Text -- ^ 'actcrCommunicationBody'
                       -> AddCommunicationToCase
addCommunicationToCase p1 = AddCommunicationToCase
    { _actcrCommunicationBody = p1
    , _actcrAttachmentSetId = Nothing
    , _actcrCaseId = Nothing
    , _actcrCcEmailAddresses = mempty
    }

data AddCommunicationToCase = AddCommunicationToCase
    { _actcrCommunicationBody :: Text
      -- ^ The body of an email communication to add to the support case.
    , _actcrAttachmentSetId :: Maybe Text
      -- ^ The ID of a set of one or more attachments for the communication
      -- to add to the case. Create the set by calling
      -- AddAttachmentsToSet.
    , _actcrCaseId :: Maybe Text
      -- ^ The AWS Support case ID requested or returned in the call. The
      -- case ID is an alphanumeric string formatted as shown in this
      -- example: case-12345678910-2013-c4c1d2bf33c5cf47.
    , _actcrCcEmailAddresses :: [Text]
      -- ^ The email addresses in the CC line of an email to be added to the
      -- support case.
    } deriving (Show, Generic)

-- | The body of an email communication to add to the support case.
actcrCommunicationBody
    :: Functor f
    => (Text
    -> f (Text))
    -> AddCommunicationToCase
    -> f AddCommunicationToCase
actcrCommunicationBody f x =
    (\y -> x { _actcrCommunicationBody = y })
       <$> f (_actcrCommunicationBody x)
{-# INLINE actcrCommunicationBody #-}

-- | The ID of a set of one or more attachments for the communication to add to
-- the case. Create the set by calling AddAttachmentsToSet.
actcrAttachmentSetId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AddCommunicationToCase
    -> f AddCommunicationToCase
actcrAttachmentSetId f x =
    (\y -> x { _actcrAttachmentSetId = y })
       <$> f (_actcrAttachmentSetId x)
{-# INLINE actcrAttachmentSetId #-}

-- | The AWS Support case ID requested or returned in the call. The case ID is
-- an alphanumeric string formatted as shown in this example:
-- case-12345678910-2013-c4c1d2bf33c5cf47.
actcrCaseId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AddCommunicationToCase
    -> f AddCommunicationToCase
actcrCaseId f x =
    (\y -> x { _actcrCaseId = y })
       <$> f (_actcrCaseId x)
{-# INLINE actcrCaseId #-}

-- | The email addresses in the CC line of an email to be added to the support
-- case.
actcrCcEmailAddresses
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> AddCommunicationToCase
    -> f AddCommunicationToCase
actcrCcEmailAddresses f x =
    (\y -> x { _actcrCcEmailAddresses = y })
       <$> f (_actcrCcEmailAddresses x)
{-# INLINE actcrCcEmailAddresses #-}

instance ToPath AddCommunicationToCase

instance ToQuery AddCommunicationToCase

instance ToHeaders AddCommunicationToCase

instance ToJSON AddCommunicationToCase

data AddCommunicationToCaseResponse = AddCommunicationToCaseResponse
    { _actcsResult :: Maybe Bool
      -- ^ True if AddCommunicationToCase succeeds. Otherwise, returns an
      -- error.
    } deriving (Show, Generic)

-- | True if AddCommunicationToCase succeeds. Otherwise, returns an error.
actcsResult
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> AddCommunicationToCaseResponse
    -> f AddCommunicationToCaseResponse
actcsResult f x =
    (\y -> x { _actcsResult = y })
       <$> f (_actcsResult x)
{-# INLINE actcsResult #-}

instance FromJSON AddCommunicationToCaseResponse

instance AWSRequest AddCommunicationToCase where
    type Sv AddCommunicationToCase = Support
    type Rs AddCommunicationToCase = AddCommunicationToCaseResponse

    request = get
    response _ = jsonResponse
