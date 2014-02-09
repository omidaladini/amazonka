{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.ListResourceRecordSets
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Imagine all the resource record sets in a zone listed out in front of you.
-- Imagine them sorted lexicographically first by DNS name (with the labels
-- reversed, like "com.amazon.www" for example), and secondarily,
-- lexicographically by record type. This operation retrieves at most MaxItems
-- resource record sets from this list, in order, starting at a position
-- specified by the Name and Type arguments: If both Name and Type are
-- omitted, this means start the results at the first RRSET in the HostedZone.
-- If Name is specified but Type is omitted, this means start the results at
-- the first RRSET in the list whose name is greater than or equal to Name. If
-- both Name and Type are specified, this means start the results at the first
-- RRSET in the list whose name is greater than or equal to Name and whose
-- type is greater than or equal to Type. It is an error to specify the Type
-- but not the Name. Use ListResourceRecordSets to retrieve a single known
-- record set by specifying the record set's name and type, and setting
-- MaxItems = 1 To retrieve all the records in a HostedZone, first pause any
-- processes making calls to ChangeResourceRecordSets. Initially call
-- ListResourceRecordSets without a Name and Type to get the first page of
-- record sets. For subsequent calls, set Name and Type to the NextName and
-- NextType values returned by the previous response. In the presence of
-- concurrent ChangeResourceRecordSets calls, there is no consistency of
-- results across calls to ListResourceRecordSets. The only way to get a
-- consistent multi-page snapshot of all RRSETs in a zone is to stop making
-- changes while pagination is in progress. However, the results from
-- ListResourceRecordSets are consistent within a page. If MakeChange calls
-- are taking place concurrently, the result of each one will either be
-- completely visible in your results or not at all. You will not see partial
-- changes, or changes that do not ultimately succeed. (This follows from the
-- fact that MakeChange is atomic) The results from ListResourceRecordSets are
-- strongly consistent with ChangeResourceRecordSets. To be precise, if a
-- single process makes a call to ChangeResourceRecordSets and receives a
-- successful response, the effects of that change will be visible in a
-- subsequent call to ListResourceRecordSets by that process.
module Network.AWS.Route53.ListResourceRecordSets where

import qualified Data.Text        as Text
import           Network.AWS.Core
import           Network.AWS.Route53.Service
import           Network.AWS.Route53.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
listResourceRecordSets :: Text
                       -- ^ The ID of the hosted zone that contains the resource record sets that you
                       -- want to get.
                       -> ListResourceRecordSets
listResourceRecordSets p1 = ListResourceRecordSets
    { lrrsHostedZoneId = p1
    , lrrsMaxItems = Nothing
    , lrrsStartRecordIdentifier = Nothing
    , lrrsStartRecordName = Nothing
    , lrrsStartRecordType = Nothing
    }

data ListResourceRecordSets = ListResourceRecordSets
    { lrrsHostedZoneId :: !Text
      -- ^ The ID of the hosted zone that contains the resource record sets that you
      -- want to get.
    , lrrsMaxItems :: Maybe Text
      -- ^ The maximum number of records you want in the response body.
    , lrrsStartRecordIdentifier :: Maybe Text
      -- ^ Weighted resource record sets only: If results were truncated for a given
      -- DNS name and type, specify the value of
      -- ListResourceRecordSetsResponse$NextRecordIdentifier from the previous
      -- response to get the next resource record set that has the current DNS name
      -- and type.
    , lrrsStartRecordName :: Maybe Text
      -- ^ The first name in the lexicographic ordering of domain names that you want
      -- the ListResourceRecordSets request to list.
    , lrrsStartRecordType :: Maybe RRType
      -- ^ The DNS type at which to begin the listing of resource record sets. Valid
      -- values: A | AAAA | CNAME | MX | NS | PTR | SOA | SPF | SRV | TXT Values for
      -- Weighted Resource Record Sets: A | AAAA | CNAME | TXT Values for Regional
      -- Resource Record Sets: A | AAAA | CNAME | TXT Values for Alias Resource
      -- Record Sets: A | AAAA Constraint: Specifying type without specifying name
      -- returns an InvalidInput error.
    } deriving (Eq, Show, Generic)

instance ToHeaders ListResourceRecordSets

instance ToPath ListResourceRecordSets where
    toPath ListResourceRecordSets{..} = Text.concat
        [ "/2012-12-12/hostedzone/"
        , toText lrrsHostedZoneId
        , "/rrset"
        ]

instance ToQuery ListResourceRecordSets where
    toQuery ListResourceRecordSets{..} = queryFromList
        [ "identifier" =? lrrsStartRecordIdentifier
        , "maxitems" =? lrrsMaxItems
        , "name" =? lrrsStartRecordName
        , "type" =? lrrsStartRecordType
        ]

instance AWSRequest ListResourceRecordSets where
    type Er ListResourceRecordSets = Route53Error
    type Rs ListResourceRecordSets = ListResourceRecordSetsResponse
    request  = getRestXML service
    response = responseXML

data ListResourceRecordSetsResponse = ListResourceRecordSetsResponse
    { lrrsrIsTruncated :: !Bool
      -- ^ A flag that indicates whether there are more resource record sets to be
      -- listed. If your results were truncated, you can make a follow-up request
      -- for the next page of results by using the
      -- ListResourceRecordSetsResponse$NextRecordName element. Valid Values: true |
      -- false.
    , lrrsrMaxItems :: !Text
      -- ^ The maximum number of records you requested. The maximum value of MaxItems
      -- is 100.
    , lrrsrNextRecordIdentifier :: Maybe Text
      -- ^ Weighted resource record sets only: If results were truncated for a given
      -- DNS name and type, the value of SetIdentifier for the next resource record
      -- set that has the current DNS name and type.
    , lrrsrNextRecordName :: Maybe Text
      -- ^ If the results were truncated, the name of the next record in the list.
      -- This element is present only if ListResourceRecordSetsResponse$IsTruncated
      -- is true.
    , lrrsrNextRecordType :: Maybe RRType
      -- ^ If the results were truncated, the type of the next record in the list.
      -- This element is present only if ListResourceRecordSetsResponse$IsTruncated
      -- is true.
    , lrrsrResourceRecordSets :: [ResourceRecordSet]
      -- ^ A complex type that contains information about the resource record sets
      -- that are returned by the request.
    } deriving (Eq, Show, Generic)

instance FromXML ListResourceRecordSetsResponse where
    fromXMLOptions = xmlOptions
