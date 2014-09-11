{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
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
-- use this action, send a POST request to the 2013-04-01/hostedzone/hosted
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
module Network.AWS.Route53.ChangeResourceRecordSets
    (
    -- * Request
      ChangeResourceRecordSets
    -- ** Request constructor
    , mkChangeResourceRecordSets
    -- ** Request lenses
    , crrsHostedZoneId
    , crrsChangeBatch

    -- * Response
    , ChangeResourceRecordSetsResponse
    -- ** Response constructor
    , mkChangeResourceRecordSetsResponse
    -- ** Response lenses
    , crrsrChangeInfo
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | A complex type that contains a change batch.
data ChangeResourceRecordSets = ChangeResourceRecordSets
    { _crrsHostedZoneId :: !Text
    , _crrsChangeBatch :: ChangeBatch
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ChangeResourceRecordSets' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @HostedZoneId ::@ @Text@
--
-- * @ChangeBatch ::@ @ChangeBatch@
--
mkChangeResourceRecordSets :: Text -- ^ 'crrsHostedZoneId'
                           -> ChangeBatch -- ^ 'crrsChangeBatch'
                           -> ChangeResourceRecordSets
mkChangeResourceRecordSets p1 p2 = ChangeResourceRecordSets
    { _crrsHostedZoneId = p1
    , _crrsChangeBatch = p2
    }

-- | The ID of the hosted zone that contains the resource record sets that you
-- want to change.
crrsHostedZoneId :: Lens' ChangeResourceRecordSets Text
crrsHostedZoneId =
    lens _crrsHostedZoneId (\s a -> s { _crrsHostedZoneId = a })

-- | A complex type that contains an optional comment and the Changes element.
crrsChangeBatch :: Lens' ChangeResourceRecordSets ChangeBatch
crrsChangeBatch = lens _crrsChangeBatch (\s a -> s { _crrsChangeBatch = a })

instance ToPath ChangeResourceRecordSets

instance ToQuery ChangeResourceRecordSets

instance ToHeaders ChangeResourceRecordSets

instance ToXML ChangeResourceRecordSets where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ChangeResourceRecordSetsRequest"

-- | A complex type containing the response for the request.
newtype ChangeResourceRecordSetsResponse = ChangeResourceRecordSetsResponse
    { _crrsrChangeInfo :: ChangeInfo
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ChangeResourceRecordSetsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ChangeInfo ::@ @ChangeInfo@
--
mkChangeResourceRecordSetsResponse :: ChangeInfo -- ^ 'crrsrChangeInfo'
                                   -> ChangeResourceRecordSetsResponse
mkChangeResourceRecordSetsResponse p1 = ChangeResourceRecordSetsResponse
    { _crrsrChangeInfo = p1
    }

-- | A complex type that contains information about changes made to your hosted
-- zone. This element contains an ID that you use when performing a GetChange
-- action to get detailed information about the change.
crrsrChangeInfo :: Lens' ChangeResourceRecordSetsResponse ChangeInfo
crrsrChangeInfo = lens _crrsrChangeInfo (\s a -> s { _crrsrChangeInfo = a })

instance FromXML ChangeResourceRecordSetsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ChangeResourceRecordSets where
    type Sv ChangeResourceRecordSets = Route53
    type Rs ChangeResourceRecordSets = ChangeResourceRecordSetsResponse

    request = get
    response _ = xmlResponse