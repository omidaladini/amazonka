{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.ChangeResourceRecordSets
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Use this action to create or change your authoritative DNS information. To
-- use this action, send a POST request to the 2012-12-12/hostedzone/hosted
-- Zone ID/rrset resource. The request body must include an XML document with
-- a ChangeResourceRecordSetsRequest element. Changes are a list of change
-- items and are considered transactional. For more information on
-- transactional changes, also known as change batches, see Creating,
-- Changing, and Deleting Resource Record Sets Using the Route 53 API in the
-- Amazon Route 53 Developer Guide. Due to the nature of transactional
-- changes, you cannot delete the same resource record set more than once in a
-- single change batch. If you attempt to delete the same change batch more
-- than once, Route 53 returns an InvalidChangeBatch error. In response to a
-- ChangeResourceRecordSets request, your DNS data is changed on all Route 53
-- DNS servers. Initially, the status of a change is PENDING. This means the
-- change has not yet propagated to all the authoritative Route 53 DNS
-- servers. When the change is propagated to all hosts, the change returns a
-- status of INSYNC. Note the following limitations on a
-- ChangeResourceRecordSets request: - A request cannot contain more than 100
-- Change elements. - A request cannot contain more than 1000 ResourceRecord
-- elements. The sum of the number of characters (including spaces) in all
-- Value elements in a request cannot exceed 32,000 characters.
module Network.AWS.Route53.ChangeResourceRecordSets where

import qualified Data.Text        as Text
import           Network.AWS.Core
import           Network.AWS.Route53.Service
import           Network.AWS.Route53.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
changeResourceRecordSets :: ChangeBatch
                         -- ^ A complex type that contains an optional comment and the Changes element.
                         -> Text
                         -- ^ Alias resource record sets only: The value of the hosted zone ID for the
                         -- AWS resource. For more information, an example, and several ways to get the
                         -- hosted zone ID for the LoadBalancer, see Creating Alias Resource Record
                         -- Sets for Elastic Load Balancing in the Amazon Route 53 Developer Guide.
                         -> ChangeResourceRecordSets
changeResourceRecordSets p1 p2 = ChangeResourceRecordSets
    { crrsrChangeBatch = p1
    , crrsrHostedZoneId = p2
    }

data ChangeResourceRecordSets = ChangeResourceRecordSets
    { crrsrChangeBatch :: ChangeBatch
      -- ^ A complex type that contains an optional comment and the Changes element.
    , crrsrHostedZoneId :: !Text
      -- ^ Alias resource record sets only: The value of the hosted zone ID for the
      -- AWS resource. For more information, an example, and several ways to get the
      -- hosted zone ID for the LoadBalancer, see Creating Alias Resource Record
      -- Sets for Elastic Load Balancing in the Amazon Route 53 Developer Guide.
    } deriving (Eq, Show, Generic)

instance ToHeaders ChangeResourceRecordSets

instance ToPath ChangeResourceRecordSets where
    toPath ChangeResourceRecordSets{..} = Text.concat
        [ "/2012-12-12/hostedzone/"
        , toText crrsrHostedZoneId
        , "/rrset/"
        ]

instance ToQuery ChangeResourceRecordSets where
    toQuery = const mempty

instance ToXML ChangeResourceRecordSets where
    toXMLOptions = xmlOptions

instance AWSRequest ChangeResourceRecordSets where
    type Er ChangeResourceRecordSets = Route53Error
    type Rs ChangeResourceRecordSets = ChangeResourceRecordSetsResponse
    request = postRestXML service

data ChangeResourceRecordSetsResponse = ChangeResourceRecordSetsResponse
    { crrsrrsChangeInfo :: ChangeInfo
      -- ^ A complex type that contains information about changes made to your hosted
      -- zone. This element contains an ID that you use when performing a GetChange
      -- action to get detailed information about the change.
    } deriving (Eq, Show, Generic)

instance FromXML ChangeResourceRecordSetsResponse where
    fromXMLOptions = xmlOptions
