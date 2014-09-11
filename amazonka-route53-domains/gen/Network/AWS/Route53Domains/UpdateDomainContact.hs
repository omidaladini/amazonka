{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53Domains.UpdateDomainContact
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation updates the contact information for a particular domain.
-- Information for at least one contact (registrant, administrator, or
-- technical) must be supplied for update. If the update is successful, this
-- method returns an operation ID that you can use to track the progress and
-- completion of the action. If the request is not completed successfully, the
-- domain registrant will be notified by email. UpdateDomainContact Example
-- POST / HTTP/1.1 host:route53domains.us-east-1.amazonaws.com
-- x-amz-date:20140711T205230Z authorization:AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20140711/us-east-1/route53domains/aws4_request,
-- 
-- SignedHeaders=content-length;content-type;host;user-agent;x-amz-date;x-amz-target,
-- Signature=[calculated-signature]
-- x-amz-target:Route53Domains_v20140515.UpdateDomainContact
-- user-agent:aws-sdk-java/1.8.3 Linux/2.6.18-164.el5PAE Java_HotSpot (TM
-- )_Server_VM/24.60-b09/1.7.0_60 content-type:application/x-amz-json-1.1
-- content-length:[number of characters in the JSON string] {
-- "DomainName":"example.com", "RegistrantContact":{ "FirstName":"John",
-- "MiddleName":"Richard", "LastName":"Doe", "ContactType":"PERSON",
-- "OrganizationName":"", "AddressLine1":"123 Any Street", "AddressLine2":"",
-- "City":"Any Town", "State":"WA", "CountryCode":"US", "ZipCode":"98101",
-- "PhoneNumber":"+2065550100", "Email":"john@example.com",
-- "Fax":"+2065550101" }, "AdminContact":{ "FirstName":"John",
-- "MiddleName":"Richard", "LastName":"Doe", "ContactType":"PERSON",
-- "OrganizationName":"", "AddressLine1":"123 Any Street", "AddressLine2":"",
-- "City":"Any Town", "State":"WA", "CountryCode":"US", "ZipCode":"98101",
-- "PhoneNumber":"+2065550100", "Email":"john@example.com",
-- "Fax":"+2065550101" }, "TechContact":{ "FirstName":"John",
-- "MiddleName":"Richard", "LastName":"Doe", "ContactType":"PERSON",
-- "OrganizationName":"", "AddressLine1":"123 Any Street", "AddressLine2":"",
-- "City":"Any Town", "State":"WA", "CountryCode":"US", "ZipCode":"98101",
-- "PhoneNumber":"+2065550100", "Email":"john@example.com",
-- "Fax":"+2065550101" }, } HTTP/1.1 200 Content-Length:[number of characters
-- in the JSON string] { "OperationId":"308c56712-faa4-40fe-94c8-b423069de3f6"
-- }.
module Network.AWS.Route53Domains.UpdateDomainContact
    (
    -- * Request
      UpdateDomainContact
    -- ** Request constructor
    , mkUpdateDomainContact
    -- ** Request lenses
    , udcDomainName
    , udcAdminContact
    , udcRegistrantContact
    , udcTechContact

    -- * Response
    , UpdateDomainContactResponse
    -- ** Response constructor
    , mkUpdateDomainContactResponse
    -- ** Response lenses
    , udcrOperationId
    ) where

import Network.AWS.Route53Domains.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | The UpdateDomainContact request includes the following elements.
data UpdateDomainContact = UpdateDomainContact
    { _udcDomainName :: !Text
    , _udcAdminContact :: Maybe ContactDetail
    , _udcRegistrantContact :: Maybe ContactDetail
    , _udcTechContact :: Maybe ContactDetail
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateDomainContact' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DomainName ::@ @Text@
--
-- * @AdminContact ::@ @Maybe ContactDetail@
--
-- * @RegistrantContact ::@ @Maybe ContactDetail@
--
-- * @TechContact ::@ @Maybe ContactDetail@
--
mkUpdateDomainContact :: Text -- ^ 'udcDomainName'
                      -> UpdateDomainContact
mkUpdateDomainContact p1 = UpdateDomainContact
    { _udcDomainName = p1
    , _udcAdminContact = Nothing
    , _udcRegistrantContact = Nothing
    , _udcTechContact = Nothing
    }

-- | The name of a domain. Type: String Default: None Constraints: The domain
-- name can contain only the letters a through z, the numbers 0 through 9, and
-- hyphen (-). Internationalized Domain Names are not supported. Required:
-- Yes.
udcDomainName :: Lens' UpdateDomainContact Text
udcDomainName = lens _udcDomainName (\s a -> s { _udcDomainName = a })

-- | Provides detailed contact information. Type: Complex Children: FirstName,
-- MiddleName, LastName, ContactType, OrganizationName, AddressLine1,
-- AddressLine2, City, State, CountryCode, ZipCode, PhoneNumber, Email, Fax,
-- ExtraParams Required: Yes.
udcAdminContact :: Lens' UpdateDomainContact (Maybe ContactDetail)
udcAdminContact = lens _udcAdminContact (\s a -> s { _udcAdminContact = a })

-- | Provides detailed contact information. Type: Complex Children: FirstName,
-- MiddleName, LastName, ContactType, OrganizationName, AddressLine1,
-- AddressLine2, City, State, CountryCode, ZipCode, PhoneNumber, Email, Fax,
-- ExtraParams Required: Yes.
udcRegistrantContact :: Lens' UpdateDomainContact (Maybe ContactDetail)
udcRegistrantContact =
    lens _udcRegistrantContact (\s a -> s { _udcRegistrantContact = a })

-- | Provides detailed contact information. Type: Complex Children: FirstName,
-- MiddleName, LastName, ContactType, OrganizationName, AddressLine1,
-- AddressLine2, City, State, CountryCode, ZipCode, PhoneNumber, Email, Fax,
-- ExtraParams Required: Yes.
udcTechContact :: Lens' UpdateDomainContact (Maybe ContactDetail)
udcTechContact = lens _udcTechContact (\s a -> s { _udcTechContact = a })

instance ToPath UpdateDomainContact

instance ToQuery UpdateDomainContact

instance ToHeaders UpdateDomainContact

instance ToJSON UpdateDomainContact

-- | The UpdateDomainContact response includes the following element.
newtype UpdateDomainContactResponse = UpdateDomainContactResponse
    { _udcrOperationId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateDomainContactResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @OperationId ::@ @Text@
--
mkUpdateDomainContactResponse :: Text -- ^ 'udcrOperationId'
                              -> UpdateDomainContactResponse
mkUpdateDomainContactResponse p1 = UpdateDomainContactResponse
    { _udcrOperationId = p1
    }

-- | Identifier for tracking the progress of the request. To use this ID to
-- query the operation status, use GetOperationDetail. Type: String Default:
-- None Constraints: Maximum 255 characters.
udcrOperationId :: Lens' UpdateDomainContactResponse Text
udcrOperationId = lens _udcrOperationId (\s a -> s { _udcrOperationId = a })

instance FromJSON UpdateDomainContactResponse

instance AWSRequest UpdateDomainContact where
    type Sv UpdateDomainContact = Route53Domains
    type Rs UpdateDomainContact = UpdateDomainContactResponse

    request = get
    response _ = jsonResponse