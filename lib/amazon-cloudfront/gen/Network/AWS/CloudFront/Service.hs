{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.Service
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.CloudFront.Service where

import Data.Default
import Data.Tagged
import GHC.Generics               (Generic)
import Network.AWS.Internal
import Network.AWS.Internal.Types (Service(..))
import Text.XML.Generic

-- | Currently supported version (@2013-11-11@) of the @Amazon CloudFront@ service.
service :: Service
service = Service Global v4 "cloudfront" "2013-11-11"

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlInherit   = True
    , xmlNamespace = Just "http://cloudfront.amazonaws.com/doc/2013-11-11/"
    }

data CloudFrontError
    = AccessDenied
    | BatchTooLarge
    | CNAMEAlreadyExists
    | CloudFrontOriginAccessIdentityAlreadyExists
    | CloudFrontOriginAccessIdentityInUse
    | DistributionAlreadyExists
    | DistributionNotDisabled
    | IllegalUpdate
    | InconsistentQuantities
    | InvalidArgument
    | InvalidDefaultRootObject
    | InvalidErrorCode
    | InvalidForwardCookies
    | InvalidGeoRestrictionParameter
    | InvalidIfMatchVersion
    | InvalidLocationCode
    | InvalidOrigin
    | InvalidOriginAccessIdentity
    | InvalidRelativePath
    | InvalidRequiredProtocol
    | InvalidResponseCode
    | InvalidViewerCertificate
    | MissingBody
    | NoSuchCloudFrontOriginAccessIdentity
    | NoSuchDistribution
    | NoSuchInvalidation
    | NoSuchOrigin
    | NoSuchStreamingDistribution
    | PreconditionFailed
    | StreamingDistributionAlreadyExists
    | StreamingDistributionNotDisabled
    | TooManyCacheBehaviors
    | TooManyCertificates
    | TooManyCloudFrontOriginAccessIdentities
    | TooManyCookieNamesInWhiteList
    | TooManyDistributionCNAMEs
    | TooManyDistributions
    | TooManyInvalidationsInProgress
    | TooManyOrigins
    | TooManyStreamingDistributionCNAMEs
    | TooManyStreamingDistributions
    | TooManyTrustedSigners
    | TrustedSignerDoesNotExist
      deriving (Eq, Show, Generic)

instance FromXML CloudFrontError where
    fromXMLOptions = xmlOptions
