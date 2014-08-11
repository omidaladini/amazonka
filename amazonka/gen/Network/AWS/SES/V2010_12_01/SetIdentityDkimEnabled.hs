{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.SES.V2010_12_01.SetIdentityDkimEnabled
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Enables or disables Easy DKIM signing of email sent from an identity: If
-- Easy DKIM signing is enabled for a domain name identity (e.g.,
-- example.com), then Amazon SES will DKIM-sign all email sent by addresses
-- under that domain name (e.g., user@example.com). If Easy DKIM signing is
-- enabled for an email address, then Amazon SES will DKIM-sign all email sent
-- by that email address. For email addresses (e.g., user@example.com), you
-- can only enable Easy DKIM signing if the corresponding domain (e.g.,
-- example.com) has been set up for Easy DKIM using the AWS Console or the
-- VerifyDomainDkim action. This action is throttled at one request per
-- second. For more information about Easy DKIM signing, go to the Amazon SES
-- Developer Guide. POST / HTTP/1.1 Date: Fri, 29 Jun 2012 22:42:08 GMT Host:
-- email.us-east-1.amazonaws.com Content-Type:
-- application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=u/hDNhYm87AV7LAPzouTBz6HJxUEuE5k96sLzYHjR24=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 168
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=SetIdentityDkimEnabled
-- &DkimEnabled=true&Identity=user%40example.com
-- &Timestamp=2012-06-29T22%3A42%3A08.000Z &Version=2010-12-01
-- 7aa61362-c469-11e1-aee5-6bbb4608fbcc.
module Network.AWS.SES.V2010_12_01.SetIdentityDkimEnabled where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.SES.V2010_12_01.Types
import Network.AWS.Prelude

data SetIdentityDkimEnabled = SetIdentityDkimEnabled
    { _siderDkimEnabled :: Bool
      -- ^ Sets whether DKIM signing is enabled for an identity. Set to true
      -- to enable DKIM signing for this identity; false to disable it.
    , _siderIdentity :: Text
      -- ^ The identity for which DKIM signing should be enabled or
      -- disabled.
    } deriving (Show, Generic)

makeLenses ''SetIdentityDkimEnabled

instance ToQuery SetIdentityDkimEnabled where
    toQuery = genericToQuery def

data SetIdentityDkimEnabledResponse = SetIdentityDkimEnabledResponse
    deriving (Eq, Show, Generic)

makeLenses ''SetIdentityDkimEnabledResponse

instance AWSRequest SetIdentityDkimEnabled where
    type Sv SetIdentityDkimEnabled = SES
    type Rs SetIdentityDkimEnabled = SetIdentityDkimEnabledResponse

    request = post "SetIdentityDkimEnabled"
    response _ = nullaryResponse SetIdentityDkimEnabledResponse