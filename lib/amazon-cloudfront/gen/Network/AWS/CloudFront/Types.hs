{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.CloudFront.Types where

import Data.ByteString      (ByteString)
import Data.Hashable
import Data.Monoid
import Data.List.NonEmpty
import Data.Text            (Text)
import Data.HashMap.Strict  (HashMap)
import Data.Time
import GHC.Generics         (Generic)
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.CloudFront.Service

-- | A complex type that contains information about viewer certificates for this
-- distribution.
data ViewerCertificate = ViewerCertificate
    { vcCloudFrontDefaultCertificate :: Maybe Bool
      -- ^ Set to true if you want to use the default *.cloudfront.net viewer
      -- certificate for this distribution. Omit this value if you are setting an
      -- IAMCertificateId.
    , vcIAMCertificateId :: Maybe Text
      -- ^ The IAM certificate identifier of the custom viewer certificate for this
      -- distribution.
    } deriving (Eq, Show, Generic)

instance ToQuery ViewerCertificate

instance FromXML ViewerCertificate where
    fromXMLOptions = xmlOptions

instance ToXML ViewerCertificate where
    toXMLOptions = xmlOptions

-- | A complex type that specifies the AWS accounts, if any, that you want to
-- allow to create signed URLs for private content. If you want to require
-- signed URLs in requests for objects in the target origin that match the
-- PathPattern for this cache behavior, specify true for Enabled, and specify
-- the applicable values for Quantity and Items. For more information, go to
-- Using a Signed URL to Serve Private Content in the Amazon CloudFront
-- Developer Guide. If you don't want to require signed URLs in requests for
-- objects that match PathPattern, specify false for Enabled and 0 for
-- Quantity. Omit Items. To add, change, or remove one or more trusted
-- signers, change Enabled to true (if it's currently false), change Quantity
-- as applicable, and specify all of the trusted signers that you want to
-- include in the updated distribution.
data TrustedSigners = TrustedSigners
    { tsEnabled :: !Bool
      -- ^ Specifies whether you want to require end users to use signed URLs to
      -- access the files specified by PathPattern and TargetOriginId.
    , tsItems :: [Text]
      -- ^ Optional: A complex type that contains trusted signers for this cache
      -- behavior. If Quantity is 0, you can omit Items.
    , tsQuantity :: !Int
      -- ^ The number of trusted signers for this cache behavior.
    } deriving (Eq, Show, Generic)

instance ToQuery TrustedSigners

instance FromXML TrustedSigners where
    fromXMLOptions = xmlOptions

instance ToXML TrustedSigners where
    toXMLOptions = xmlOptions

-- | A complex type that controls whether access logs are written for the
-- streaming distribution.
data StreamingLoggingConfig = StreamingLoggingConfig
    { slcBucket :: !Text
      -- ^ The Amazon S3 bucket to store the access logs in, for example,
      -- myawslogbucket.s3.amazonaws.com.
    , slcEnabled :: !Bool
      -- ^ Specifies whether you want CloudFront to save access logs to an Amazon S3
      -- bucket. If you do not want to enable logging when you create a streaming
      -- distribution or if you want to disable logging for an existing streaming
      -- distribution, specify false for Enabled, and specify empty Bucket and
      -- Prefix elements. If you specify false for Enabled but you specify values
      -- for Bucket and Prefix, the values are automatically deleted.
    , slcPrefix :: !Text
      -- ^ An optional string that you want CloudFront to prefix to the access log
      -- filenames for this streaming distribution, for example, myprefix/. If you
      -- want to enable logging, but you do not want to specify a prefix, you still
      -- must include an empty Prefix element in the Logging element.
    } deriving (Eq, Show, Generic)

instance ToQuery StreamingLoggingConfig

instance FromXML StreamingLoggingConfig where
    fromXMLOptions = xmlOptions

instance ToXML StreamingLoggingConfig where
    toXMLOptions = xmlOptions

-- | A summary of the information for an Amazon CloudFront streaming
-- distribution.
data StreamingDistributionSummary = StreamingDistributionSummary
    { sdsAliases :: Aliases
      -- ^ A complex type that contains information about CNAMEs (alternate domain
      -- names), if any, for this streaming distribution.
    , sdsComment :: !Text
      -- ^ The comment originally specified when this distribution was created.
    , sdsDomainName :: !Text
      -- ^ The domain name corresponding to the distribution. For example:
      -- d604721fxaaqy9.cloudfront.net.
    , sdsEnabled :: !Bool
      -- ^ Whether the distribution is enabled to accept end user requests for
      -- content.
    , sdsId :: !Text
      -- ^ The identifier for the distribution. For example: EDFDVBD632BHDS5.
    , sdsLastModifiedTime :: !UTCTime
      -- ^ The date and time the distribution was last modified.
    , sdsPriceClass :: !PriceClass
    , sdsS3Origin :: S3Origin
      -- ^ A complex type that contains information about the Amazon S3 bucket from
      -- which you want CloudFront to get your media files for distribution.
    , sdsStatus :: !Text
      -- ^ Indicates the current status of the distribution. When the status is
      -- Deployed, the distribution's information is fully propagated throughout the
      -- Amazon CloudFront system.
    , sdsTrustedSigners :: TrustedSigners
      -- ^ A complex type that specifies the AWS accounts, if any, that you want to
      -- allow to create signed URLs for private content. If you want to require
      -- signed URLs in requests for objects in the target origin that match the
      -- PathPattern for this cache behavior, specify true for Enabled, and specify
      -- the applicable values for Quantity and Items. For more information, go to
      -- Using a Signed URL to Serve Private Content in the Amazon CloudFront
      -- Developer Guide. If you don't want to require signed URLs in requests for
      -- objects that match PathPattern, specify false for Enabled and 0 for
      -- Quantity. Omit Items. To add, change, or remove one or more trusted
      -- signers, change Enabled to true (if it's currently false), change Quantity
      -- as applicable, and specify all of the trusted signers that you want to
      -- include in the updated distribution.
    } deriving (Eq, Show, Generic)

instance ToQuery StreamingDistributionSummary

instance FromXML StreamingDistributionSummary where
    fromXMLOptions = xmlOptions

instance ToXML StreamingDistributionSummary where
    toXMLOptions = xmlOptions

-- | The StreamingDistributionList type.
data StreamingDistributionList = StreamingDistributionList
    { sdlIsTruncated :: !Bool
      -- ^ A flag that indicates whether more streaming distributions remain to be
      -- listed. If your results were truncated, you can make a follow-up pagination
      -- request using the Marker request parameter to retrieve more distributions
      -- in the list.
    , sdlItems :: [StreamingDistributionSummary]
      -- ^ A complex type that contains one StreamingDistributionSummary element for
      -- each distribution that was created by the current AWS account.
    , sdlMarker :: !Text
      -- ^ The value you provided for the Marker request parameter.
    , sdlMaxItems :: !Int
      -- ^ The value you provided for the MaxItems request parameter.
    , sdlNextMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the value you
      -- can use for the Marker request parameter to continue listing your streaming
      -- distributions where they left off.
    , sdlQuantity :: !Int
      -- ^ The number of streaming distributions that were created by the current AWS
      -- account.
    } deriving (Eq, Show, Generic)

instance ToQuery StreamingDistributionList

instance FromXML StreamingDistributionList where
    fromXMLOptions = xmlOptions

instance ToXML StreamingDistributionList where
    toXMLOptions = xmlOptions

-- | The streaming distribution's configuration information.
data StreamingDistributionConfig = StreamingDistributionConfig
    { sdcAliases :: Aliases
      -- ^ A complex type that contains information about CNAMEs (alternate domain
      -- names), if any, for this streaming distribution.
    , sdcCallerReference :: !Text
      -- ^ A unique number that ensures the request can't be replayed. If the
      -- CallerReference is new (no matter the content of the
      -- StreamingDistributionConfig object), a new streaming distribution is
      -- created. If the CallerReference is a value you already sent in a previous
      -- request to create a streaming distribution, and the content of the
      -- StreamingDistributionConfig is identical to the original request (ignoring
      -- white space), the response includes the same information returned to the
      -- original request. If the CallerReference is a value you already sent in a
      -- previous request to create a streaming distribution but the content of the
      -- StreamingDistributionConfig is different from the original request,
      -- CloudFront returns a DistributionAlreadyExists error.
    , sdcComment :: !Text
      -- ^ Any comments you want to include about the streaming distribution.
    , sdcEnabled :: !Bool
      -- ^ Whether the streaming distribution is enabled to accept end user requests
      -- for content.
    , sdcLogging :: StreamingLoggingConfig
      -- ^ A complex type that controls whether access logs are written for the
      -- streaming distribution.
    , sdcPriceClass :: !PriceClass
      -- ^ A complex type that contains information about price class for this
      -- streaming distribution.
    , sdcS3Origin :: S3Origin
      -- ^ A complex type that contains information about the Amazon S3 bucket from
      -- which you want CloudFront to get your media files for distribution.
    , sdcTrustedSigners :: TrustedSigners
      -- ^ A complex type that specifies the AWS accounts, if any, that you want to
      -- allow to create signed URLs for private content. If you want to require
      -- signed URLs in requests for objects in the target origin that match the
      -- PathPattern for this cache behavior, specify true for Enabled, and specify
      -- the applicable values for Quantity and Items. For more information, go to
      -- Using a Signed URL to Serve Private Content in the Amazon CloudFront
      -- Developer Guide. If you don't want to require signed URLs in requests for
      -- objects that match PathPattern, specify false for Enabled and 0 for
      -- Quantity. Omit Items. To add, change, or remove one or more trusted
      -- signers, change Enabled to true (if it's currently false), change Quantity
      -- as applicable, and specify all of the trusted signers that you want to
      -- include in the updated distribution.
    } deriving (Eq, Show, Generic)

instance ToQuery StreamingDistributionConfig

instance FromXML StreamingDistributionConfig where
    fromXMLOptions = xmlOptions

instance ToXML StreamingDistributionConfig where
    toXMLOptions = xmlOptions

-- | The streaming distribution's information.
data StreamingDistribution = StreamingDistribution
    { sdActiveTrustedSigners :: ActiveTrustedSigners
      -- ^ CloudFront automatically adds this element to the response only if you've
      -- set up the distribution to serve private content with signed URLs. The
      -- element lists the key pair IDs that CloudFront is aware of for each trusted
      -- signer. The Signer child element lists the AWS account number of the
      -- trusted signer (or an empty Self element if the signer is you). The Signer
      -- element also includes the IDs of any active key pairs associated with the
      -- trusted signer's AWS account. If no KeyPairId element appears for a Signer,
      -- that signer can't create working signed URLs.
    , sdDomainName :: !Text
      -- ^ The domain name corresponding to the streaming distribution. For example:
      -- s5c39gqb8ow64r.cloudfront.net.
    , sdId :: !Text
      -- ^ The identifier for the streaming distribution. For example:
      -- EGTXBD79H29TRA8.
    , sdLastModifiedTime :: Maybe UTCTime
      -- ^ The date and time the distribution was last modified.
    , sdStatus :: !Text
      -- ^ The current status of the streaming distribution. When the status is
      -- Deployed, the distribution's information is fully propagated throughout the
      -- Amazon CloudFront system.
    , sdStreamingDistributionConfig :: StreamingDistributionConfig
      -- ^ The current configuration information for the streaming distribution.
    } deriving (Eq, Show, Generic)

instance ToQuery StreamingDistribution

instance FromXML StreamingDistribution where
    fromXMLOptions = xmlOptions

instance ToXML StreamingDistribution where
    toXMLOptions = xmlOptions

-- | A complex type that lists the AWS accounts that were included in the
-- TrustedSigners complex type, as well as their active CloudFront key pair
-- IDs, if any.
data Signer = Signer
    { sAwsAccountNumber :: Maybe Text
      -- ^ Specifies an AWS account that can create signed URLs. Values: self, which
      -- indicates that the AWS account that was used to create the distribution can
      -- created signed URLs, or an AWS account number. Omit the dashes in the
      -- account number.
    , sKeyPairIds :: Maybe KeyPairIds
      -- ^ A complex type that lists the active CloudFront key pairs, if any, that are
      -- associated with AwsAccountNumber.
    } deriving (Eq, Show, Generic)

instance ToQuery Signer

instance FromXML Signer where
    fromXMLOptions = xmlOptions

instance ToXML Signer where
    toXMLOptions = xmlOptions

-- | A complex type that contains information about the Amazon S3 origin. If the
-- origin is a custom origin, use the CustomOriginConfig element instead.
newtype S3OriginConfig = S3OriginConfig
    { s3ocOriginAccessIdentity :: Text
      -- ^ The CloudFront origin access identity to associate with the origin. Use an
      -- origin access identity to configure the origin so that end users can only
      -- access objects in an Amazon S3 bucket through CloudFront. If you want end
      -- users to be able to access objects using either the CloudFront URL or the
      -- Amazon S3 URL, specify an empty OriginAccessIdentity element. To delete the
      -- origin access identity from an existing distribution, update the
      -- distribution configuration and include an empty OriginAccessIdentity
      -- element. To replace the origin access identity, update the distribution
      -- configuration and specify the new origin access identity.
    } deriving (Eq, Show, Generic)

instance ToQuery S3OriginConfig

instance FromXML S3OriginConfig where
    fromXMLOptions = xmlOptions

instance ToXML S3OriginConfig where
    toXMLOptions = xmlOptions

-- | A complex type that contains information about the Amazon S3 bucket from
-- which you want CloudFront to get your media files for distribution.
data S3Origin = S3Origin
    { s3oDomainName :: !Text
      -- ^ The DNS name of the S3 origin.
    , s3oOriginAccessIdentity :: !Text
      -- ^ Your S3 origin's origin access identity.
    } deriving (Eq, Show, Generic)

instance ToQuery S3Origin

instance FromXML S3Origin where
    fromXMLOptions = xmlOptions

instance ToXML S3Origin where
    toXMLOptions = xmlOptions

-- | A complex type that identifies ways in which you want to restrict
-- distribution of your content.
newtype Restrictions = Restrictions
    { rGeoRestriction :: GeoRestriction
      -- ^ A complex type that controls the countries in which your content is
      -- distributed. For more information about geo restriction, go to Customizing
      -- Error Responses in the Amazon CloudFront Developer Guide. CloudFront
      -- determines the location of your users using MaxMind GeoIP databases. For
      -- information about the accuracy of these databases, see How accurate are
      -- your GeoIP databases? on the MaxMind website.
    } deriving (Eq, Show, Generic)

instance ToQuery Restrictions

instance FromXML Restrictions where
    fromXMLOptions = xmlOptions

instance ToXML Restrictions where
    toXMLOptions = xmlOptions

-- | The path of the object to invalidate. The path is relative to the
-- distribution and must begin with a slash (/). You must enclose each
-- invalidation object with the Path element tags. If the path includes
-- non-ASCII characters or unsafe characters as defined in RFC 1783
-- (http://www.ietf.org/rfc/rfc1738.txt), URL encode those characters. Do not
-- URL encode any other characters in the path, or CloudFront will not
-- invalidate the old version of the updated object.
data Paths = Paths
    { qItems :: [Text]
      -- ^ A complex type that contains a list of the objects that you want to
      -- invalidate.
    , qQuantity :: !Int
      -- ^ The number of objects that you want to invalidate.
    } deriving (Eq, Show, Generic)

instance ToQuery Paths

instance FromXML Paths where
    fromXMLOptions = xmlOptions

instance ToXML Paths where
    toXMLOptions = xmlOptions

-- | A complex type that contains information about origins for this
-- distribution.
data Origins = Origins
    { pItems :: [Origin]
      -- ^ A complex type that contains origins for this distribution.
    , pQuantity :: !Int
      -- ^ The number of origins for this distribution.
    } deriving (Eq, Show, Generic)

instance ToQuery Origins

instance FromXML Origins where
    fromXMLOptions = xmlOptions

instance ToXML Origins where
    toXMLOptions = xmlOptions

-- | A complex type that describes the Amazon S3 bucket or the HTTP server (for
-- example, a web server) from which CloudFront gets your files.You must
-- create at least one origin.
data Origin = Origin
    { oCustomOriginConfig :: Maybe CustomOriginConfig
      -- ^ A complex type that contains information about a custom origin. If the
      -- origin is an Amazon S3 bucket, use the S3OriginConfig element instead.
    , oDomainName :: !Text
      -- ^ Amazon S3 origins: The DNS name of the Amazon S3 bucket from which you want
      -- CloudFront to get objects for this origin, for example,
      -- myawsbucket.s3.amazonaws.com. Custom origins: The DNS domain name for the
      -- HTTP server from which you want CloudFront to get objects for this origin,
      -- for example, www.example.com.
    , oId :: !Text
      -- ^ A unique identifier for the origin. The value of Id must be unique within
      -- the distribution. You use the value of Id when you create a cache behavior.
      -- The Id identifies the origin that CloudFront routes a request to when the
      -- request matches the path pattern for that cache behavior.
    , oS3OriginConfig :: Maybe S3OriginConfig
      -- ^ A complex type that contains information about the Amazon S3 origin. If the
      -- origin is a custom origin, use the CustomOriginConfig element instead.
    } deriving (Eq, Show, Generic)

instance ToQuery Origin

instance FromXML Origin where
    fromXMLOptions = xmlOptions

instance ToXML Origin where
    toXMLOptions = xmlOptions

-- | A complex type that controls whether access logs are written for the
-- distribution.
data LoggingConfig = LoggingConfig
    { lcBucket :: !Text
      -- ^ The Amazon S3 bucket to store the access logs in, for example,
      -- myawslogbucket.s3.amazonaws.com.
    , lcEnabled :: !Bool
      -- ^ Specifies whether you want CloudFront to save access logs to an Amazon S3
      -- bucket. If you do not want to enable logging when you create a distribution
      -- or if you want to disable logging for an existing distribution, specify
      -- false for Enabled, and specify empty Bucket and Prefix elements. If you
      -- specify false for Enabled but you specify values for Bucket, prefix and
      -- IncludeCookies, the values are automatically deleted.
    , lcIncludeCookies :: !Bool
      -- ^ Specifies whether you want CloudFront to include cookies in access logs,
      -- specify true for IncludeCookies. If you choose to include cookies in logs,
      -- CloudFront logs all cookies regardless of how you configure the cache
      -- behaviors for this distribution. If you do not want to include cookies when
      -- you create a distribution or if you want to disable include cookies for an
      -- existing distribution, specify false for IncludeCookies.
    , lcPrefix :: !Text
      -- ^ An optional string that you want CloudFront to prefix to the access log
      -- filenames for this distribution, for example, myprefix/. If you want to
      -- enable logging, but you do not want to specify a prefix, you still must
      -- include an empty Prefix element in the Logging element.
    } deriving (Eq, Show, Generic)

instance ToQuery LoggingConfig

instance FromXML LoggingConfig where
    fromXMLOptions = xmlOptions

instance ToXML LoggingConfig where
    toXMLOptions = xmlOptions

-- | A complex type that lists the active CloudFront key pairs, if any, that are
-- associated with AwsAccountNumber.
data KeyPairIds = KeyPairIds
    { kpiItems :: [Text]
      -- ^ A complex type that lists the active CloudFront key pairs, if any, that are
      -- associated with AwsAccountNumber.
    , kpiQuantity :: !Int
      -- ^ The number of active CloudFront key pairs for AwsAccountNumber.
    } deriving (Eq, Show, Generic)

instance ToQuery KeyPairIds

instance FromXML KeyPairIds where
    fromXMLOptions = xmlOptions

instance ToXML KeyPairIds where
    toXMLOptions = xmlOptions

-- | Summary of an invalidation request.
data InvalidationSummary = InvalidationSummary
    { isCreateTime :: !UTCTime
    , isId :: !Text
      -- ^ The unique ID for an invalidation request.
    , isStatus :: !Text
      -- ^ The status of an invalidation request.
    } deriving (Eq, Show, Generic)

instance ToQuery InvalidationSummary

instance FromXML InvalidationSummary where
    fromXMLOptions = xmlOptions

instance ToXML InvalidationSummary where
    toXMLOptions = xmlOptions

-- | Information about invalidation batches.
data InvalidationList = InvalidationList
    { ilIsTruncated :: !Bool
      -- ^ A flag that indicates whether more invalidation batch requests remain to be
      -- listed. If your results were truncated, you can make a follow-up pagination
      -- request using the Marker request parameter to retrieve more invalidation
      -- batches in the list.
    , ilItems :: [InvalidationSummary]
      -- ^ A complex type that contains one InvalidationSummary element for each
      -- invalidation batch that was created by the current AWS account.
    , ilMarker :: !Text
      -- ^ The value you provided for the Marker request parameter.
    , ilMaxItems :: !Int
      -- ^ The value you provided for the MaxItems request parameter.
    , ilNextMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the value you
      -- can use for the Marker request parameter to continue listing your
      -- invalidation batches where they left off.
    , ilQuantity :: !Int
      -- ^ The number of invalidation batches that were created by the current AWS
      -- account.
    } deriving (Eq, Show, Generic)

instance ToQuery InvalidationList

instance FromXML InvalidationList where
    fromXMLOptions = xmlOptions

instance ToXML InvalidationList where
    toXMLOptions = xmlOptions

-- | The current invalidation information for the batch request.
data InvalidationBatch = InvalidationBatch
    { ibCallerReference :: !Text
      -- ^ A unique name that ensures the request can't be replayed. If the
      -- CallerReference is new (no matter the content of the Path object), a new
      -- distribution is created. If the CallerReference is a value you already sent
      -- in a previous request to create an invalidation batch, and the content of
      -- each Path element is identical to the original request, the response
      -- includes the same information returned to the original request. If the
      -- CallerReference is a value you already sent in a previous request to create
      -- a distribution but the content of any Path is different from the original
      -- request, CloudFront returns an InvalidationBatchAlreadyExists error.
    , ibPaths :: Paths
      -- ^ The path of the object to invalidate. The path is relative to the
      -- distribution and must begin with a slash (/). You must enclose each
      -- invalidation object with the Path element tags. If the path includes
      -- non-ASCII characters or unsafe characters as defined in RFC 1783
      -- (http://www.ietf.org/rfc/rfc1738.txt), URL encode those characters. Do not
      -- URL encode any other characters in the path, or CloudFront will not
      -- invalidate the old version of the updated object.
    } deriving (Eq, Show, Generic)

instance ToQuery InvalidationBatch

instance FromXML InvalidationBatch where
    fromXMLOptions = xmlOptions

instance ToXML InvalidationBatch where
    toXMLOptions = xmlOptions

-- | The invalidation's information.
data Invalidation = Invalidation
    { iCreateTime :: !UTCTime
      -- ^ The date and time the invalidation request was first made.
    , iId :: !Text
      -- ^ The identifier for the invalidation request. For example: IDFDVBD632BHDS5.
    , iInvalidationBatch :: InvalidationBatch
      -- ^ The current invalidation information for the batch request.
    , iStatus :: !Text
      -- ^ The status of the invalidation request. When the invalidation batch is
      -- finished, the status is Completed.
    } deriving (Eq, Show, Generic)

instance ToQuery Invalidation

instance FromXML Invalidation where
    fromXMLOptions = xmlOptions

instance ToXML Invalidation where
    toXMLOptions = xmlOptions

-- | A complex type that controls the countries in which your content is
-- distributed. For more information about geo restriction, go to Customizing
-- Error Responses in the Amazon CloudFront Developer Guide. CloudFront
-- determines the location of your users using MaxMind GeoIP databases. For
-- information about the accuracy of these databases, see How accurate are
-- your GeoIP databases? on the MaxMind website.
data GeoRestriction = GeoRestriction
    { grItems :: [Text]
      -- ^ A complex type that contains a Location element for each country in which
      -- you want CloudFront either to distribute your content (whitelist) or not
      -- distribute your content (blacklist). The Location element is a two-letter,
      -- uppercase country code for a country that you want to include in your
      -- blacklist or whitelist. Include one Location element for each country.
      -- CloudFront and MaxMind both use ISO 3166 country codes. For the current
      -- list of countries and the corresponding codes, see ISO 3166-1-alpha-2 code
      -- on the International Organization for Standardization website. You can also
      -- refer to the country list in the CloudFront console, which includes both
      -- country names and codes.
    , grQuantity :: !Int
      -- ^ When geo restriction is enabled, this is the number of countries in your
      -- whitelist or blacklist. Otherwise, when it is not enabled, Quantity is 0,
      -- and you can omit Items.
    , grRestrictionType :: !GeoRestrictionType
      -- ^ The method that you want to use to restrict distribution of your content by
      -- country: - none: No geo restriction is enabled, meaning access to content
      -- is not restricted by client geo location. - blacklist: The Location
      -- elements specify the countries in which you do not want CloudFront to
      -- distribute your content. - whitelist: The Location elements specify the
      -- countries in which you want CloudFront to distribute your content.
    } deriving (Eq, Show, Generic)

instance ToQuery GeoRestriction

instance FromXML GeoRestriction where
    fromXMLOptions = xmlOptions

instance ToXML GeoRestriction where
    toXMLOptions = xmlOptions

-- | A complex type that specifies how CloudFront handles query strings and
-- cookies.
data ForwardedValues = ForwardedValues
    { fvCookies :: CookiePreference
      -- ^ A complex type that specifies how CloudFront handles cookies.
    , fvQueryString :: !Bool
      -- ^ Indicates whether you want CloudFront to forward query strings to the
      -- origin that is associated with this cache behavior. If so, specify true; if
      -- not, specify false.
    } deriving (Eq, Show, Generic)

instance ToQuery ForwardedValues

instance FromXML ForwardedValues where
    fromXMLOptions = xmlOptions

instance ToXML ForwardedValues where
    toXMLOptions = xmlOptions

-- | A summary of the information for an Amazon CloudFront distribution.
data DistributionSummary = DistributionSummary
    { dsAliases :: Aliases
      -- ^ A complex type that contains information about CNAMEs (alternate domain
      -- names), if any, for this distribution.
    , dsCacheBehaviors :: CacheBehaviors
      -- ^ A complex type that contains zero or more CacheBehavior elements.
    , dsComment :: !Text
      -- ^ The comment originally specified when this distribution was created.
    , dsCustomErrorResponses :: CustomErrorResponses
      -- ^ A complex type that contains zero or more CustomErrorResponses elements.
    , dsDefaultCacheBehavior :: DefaultCacheBehavior
      -- ^ A complex type that describes the default cache behavior if you do not
      -- specify a CacheBehavior element or if files don't match any of the values
      -- of PathPattern in CacheBehavior elements.You must create exactly one
      -- default cache behavior.
    , dsDomainName :: !Text
      -- ^ The domain name corresponding to the distribution. For example:
      -- d604721fxaaqy9.cloudfront.net.
    , dsEnabled :: !Bool
      -- ^ Whether the distribution is enabled to accept end user requests for
      -- content.
    , dsId :: !Text
      -- ^ The identifier for the distribution. For example: EDFDVBD632BHDS5.
    , dsLastModifiedTime :: !UTCTime
      -- ^ The date and time the distribution was last modified.
    , dsOrigins :: Origins
      -- ^ A complex type that contains information about origins for this
      -- distribution.
    , dsPriceClass :: !PriceClass
    , dsRestrictions :: Restrictions
      -- ^ A complex type that identifies ways in which you want to restrict
      -- distribution of your content.
    , dsStatus :: !Text
      -- ^ This response element indicates the current status of the distribution.
      -- When the status is Deployed, the distribution's information is fully
      -- propagated throughout the Amazon CloudFront system.
    , dsViewerCertificate :: ViewerCertificate
      -- ^ A complex type that contains information about viewer certificates for this
      -- distribution.
    } deriving (Eq, Show, Generic)

instance ToQuery DistributionSummary

instance FromXML DistributionSummary where
    fromXMLOptions = xmlOptions

instance ToXML DistributionSummary where
    toXMLOptions = xmlOptions

-- | The DistributionList type.
data DistributionList = DistributionList
    { dlIsTruncated :: !Bool
      -- ^ A flag that indicates whether more distributions remain to be listed. If
      -- your results were truncated, you can make a follow-up pagination request
      -- using the Marker request parameter to retrieve more distributions in the
      -- list.
    , dlItems :: [DistributionSummary]
      -- ^ A complex type that contains one DistributionSummary element for each
      -- distribution that was created by the current AWS account.
    , dlMarker :: !Text
      -- ^ The value you provided for the Marker request parameter.
    , dlMaxItems :: !Int
      -- ^ The value you provided for the MaxItems request parameter.
    , dlNextMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the value you
      -- can use for the Marker request parameter to continue listing your
      -- distributions where they left off.
    , dlQuantity :: !Int
      -- ^ The number of distributions that were created by the current AWS account.
    } deriving (Eq, Show, Generic)

instance ToQuery DistributionList

instance FromXML DistributionList where
    fromXMLOptions = xmlOptions

instance ToXML DistributionList where
    toXMLOptions = xmlOptions

-- | The distribution's configuration information.
data DistributionConfig = DistributionConfig
    { dcAliases :: Aliases
      -- ^ A complex type that contains information about CNAMEs (alternate domain
      -- names), if any, for this distribution.
    , dcCacheBehaviors :: CacheBehaviors
      -- ^ A complex type that contains zero or more CacheBehavior elements.
    , dcCallerReference :: !Text
      -- ^ A unique number that ensures the request can't be replayed. If the
      -- CallerReference is new (no matter the content of the DistributionConfig
      -- object), a new distribution is created. If the CallerReference is a value
      -- you already sent in a previous request to create a distribution, and the
      -- content of the DistributionConfig is identical to the original request
      -- (ignoring white space), the response includes the same information returned
      -- to the original request. If the CallerReference is a value you already sent
      -- in a previous request to create a distribution but the content of the
      -- DistributionConfig is different from the original request, CloudFront
      -- returns a DistributionAlreadyExists error.
    , dcComment :: !Text
      -- ^ Any comments you want to include about the distribution.
    , dcCustomErrorResponses :: Maybe CustomErrorResponses
      -- ^ A complex type that contains zero or more CustomErrorResponse elements.
    , dcDefaultCacheBehavior :: DefaultCacheBehavior
      -- ^ A complex type that describes the default cache behavior if you do not
      -- specify a CacheBehavior element or if files don't match any of the values
      -- of PathPattern in CacheBehavior elements.You must create exactly one
      -- default cache behavior.
    , dcDefaultRootObject :: !Text
      -- ^ The object that you want CloudFront to return (for example, index.html)
      -- when an end user requests the root URL for your distribution
      -- (http://www.example.com) instead of an object in your distribution
      -- (http://www.example.com/index.html). Specifying a default root object
      -- avoids exposing the contents of your distribution. If you don't want to
      -- specify a default root object when you create a distribution, include an
      -- empty DefaultRootObject element. To delete the default root object from an
      -- existing distribution, update the distribution configuration and include an
      -- empty DefaultRootObject element. To replace the default root object, update
      -- the distribution configuration and specify the new object.
    , dcEnabled :: !Bool
      -- ^ Whether the distribution is enabled to accept end user requests for
      -- content.
    , dcLogging :: LoggingConfig
      -- ^ A complex type that controls whether access logs are written for the
      -- distribution.
    , dcOrigins :: Origins
      -- ^ A complex type that contains information about origins for this
      -- distribution.
    , dcPriceClass :: !PriceClass
      -- ^ A complex type that contains information about price class for this
      -- distribution.
    , dcRestrictions :: Maybe Restrictions
      -- ^ A complex type that identifies ways in which you want to restrict
      -- distribution of your content.
    , dcViewerCertificate :: Maybe ViewerCertificate
      -- ^ A complex type that contains information about viewer certificates for this
      -- distribution.
    } deriving (Eq, Show, Generic)

instance ToQuery DistributionConfig

instance FromXML DistributionConfig where
    fromXMLOptions = xmlOptions

instance ToXML DistributionConfig where
    toXMLOptions = xmlOptions

-- | The distribution's information.
data Distribution = Distribution
    { dActiveTrustedSigners :: ActiveTrustedSigners
      -- ^ CloudFront automatically adds this element to the response only if you've
      -- set up the distribution to serve private content with signed URLs. The
      -- element lists the key pair IDs that CloudFront is aware of for each trusted
      -- signer. The Signer child element lists the AWS account number of the
      -- trusted signer (or an empty Self element if the signer is you). The Signer
      -- element also includes the IDs of any active key pairs associated with the
      -- trusted signer's AWS account. If no KeyPairId element appears for a Signer,
      -- that signer can't create working signed URLs.
    , dDistributionConfig :: DistributionConfig
      -- ^ The current configuration information for the distribution.
    , dDomainName :: !Text
      -- ^ The domain name corresponding to the distribution. For example:
      -- d604721fxaaqy9.cloudfront.net.
    , dId :: !Text
      -- ^ The identifier for the distribution. For example: EDFDVBD632BHDS5.
    , dInProgressInvalidationBatches :: !Int
      -- ^ The number of invalidation batches currently in progress.
    , dLastModifiedTime :: !UTCTime
      -- ^ The date and time the distribution was last modified.
    , dStatus :: !Text
      -- ^ This response element indicates the current status of the distribution.
      -- When the status is Deployed, the distribution's information is fully
      -- propagated throughout the Amazon CloudFront system.
    } deriving (Eq, Show, Generic)

instance ToQuery Distribution

instance FromXML Distribution where
    fromXMLOptions = xmlOptions

instance ToXML Distribution where
    toXMLOptions = xmlOptions

-- | A complex type that describes the default cache behavior if you do not
-- specify a CacheBehavior element or if files don't match any of the values
-- of PathPattern in CacheBehavior elements.You must create exactly one
-- default cache behavior.
data DefaultCacheBehavior = DefaultCacheBehavior
    { dcbAllowedMethods :: Maybe AllowedMethods
      -- ^ A complex type that controls which HTTP methods CloudFront processes and
      -- forwards to your Amazon S3 bucket or your custom origin. There are two
      -- options: - CloudFront forwards only GET and HEAD requests. - CloudFront
      -- forwards DELETE, GET, HEAD, OPTIONS, PATCH, POST, and PUT requests. If you
      -- choose the second option, you may need to restrict access to your Amazon S3
      -- bucket or to your custom origin so users can't perform operations that you
      -- don't want them to. For example, you may not want users to have permission
      -- to delete objects from your origin.
    , dcbForwardedValues :: ForwardedValues
      -- ^ A complex type that specifies how CloudFront handles query strings and
      -- cookies.
    , dcbMinTTL :: !Integer
      -- ^ The minimum amount of time that you want objects to stay in CloudFront
      -- caches before CloudFront queries your origin to see whether the object has
      -- been updated.You can specify a value from 0 to 3,153,600,000 seconds (100
      -- years).
    , dcbTargetOriginId :: !Text
      -- ^ The value of ID for the origin that you want CloudFront to route requests
      -- to when a request matches the path pattern either for a cache behavior or
      -- for the default cache behavior.
    , dcbTrustedSigners :: TrustedSigners
      -- ^ A complex type that specifies the AWS accounts, if any, that you want to
      -- allow to create signed URLs for private content. If you want to require
      -- signed URLs in requests for objects in the target origin that match the
      -- PathPattern for this cache behavior, specify true for Enabled, and specify
      -- the applicable values for Quantity and Items. For more information, go to
      -- Using a Signed URL to Serve Private Content in the Amazon CloudFront
      -- Developer Guide. If you don't want to require signed URLs in requests for
      -- objects that match PathPattern, specify false for Enabled and 0 for
      -- Quantity. Omit Items. To add, change, or remove one or more trusted
      -- signers, change Enabled to true (if it's currently false), change Quantity
      -- as applicable, and specify all of the trusted signers that you want to
      -- include in the updated distribution.
    , dcbViewerProtocolPolicy :: !ViewerProtocolPolicy
      -- ^ Use this element to specify the protocol that users can use to access the
      -- files in the origin specified by TargetOriginId when a request matches the
      -- path pattern in PathPattern. If you want CloudFront to allow end users to
      -- use any available protocol, specify allow-all. If you want CloudFront to
      -- require HTTPS, specify https.
    } deriving (Eq, Show, Generic)

instance ToQuery DefaultCacheBehavior

instance FromXML DefaultCacheBehavior where
    fromXMLOptions = xmlOptions

instance ToXML DefaultCacheBehavior where
    toXMLOptions = xmlOptions

-- | A complex type that contains information about a custom origin. If the
-- origin is an Amazon S3 bucket, use the S3OriginConfig element instead.
data CustomOriginConfig = CustomOriginConfig
    { cocHTTPPort :: !Int
      -- ^ The HTTP port the custom origin listens on.
    , cocHTTPSPort :: !Int
      -- ^ The HTTPS port the custom origin listens on.
    , cocOriginProtocolPolicy :: !OriginProtocolPolicy
      -- ^ The origin protocol policy to apply to your origin.
    } deriving (Eq, Show, Generic)

instance ToQuery CustomOriginConfig

instance FromXML CustomOriginConfig where
    fromXMLOptions = xmlOptions

instance ToXML CustomOriginConfig where
    toXMLOptions = xmlOptions

-- | A complex type that contains zero or more CustomErrorResponse elements.
data CustomErrorResponses = CustomErrorResponses
    { cesItems :: [CustomErrorResponse]
      -- ^ Optional: A complex type that contains custom error responses for this
      -- distribution. If Quantity is 0, you can omit Items.
    , cesQuantity :: !Int
      -- ^ The number of custom error responses for this distribution.
    } deriving (Eq, Show, Generic)

instance ToQuery CustomErrorResponses

instance FromXML CustomErrorResponses where
    fromXMLOptions = xmlOptions

instance ToXML CustomErrorResponses where
    toXMLOptions = xmlOptions

-- | A complex type that describes how you'd prefer CloudFront to respond to
-- requests that result in either a 4xx or 5xx response. You can control
-- whether a custom error page should be displayed, what the desired response
-- code should be for this error page and how long should the error response
-- be cached by CloudFront. If you don't want to specify any custom error
-- responses, include only an empty CustomErrorResponses element. To delete
-- all custom error responses in an existing distribution, update the
-- distribution configuration and include only an empty CustomErrorResponses
-- element. To add, change, or remove one or more custom error responses,
-- update the distribution configuration and specify all of the custom error
-- responses that you want to include in the updated distribution.
data CustomErrorResponse = CustomErrorResponse
    { cerErrorCachingMinTTL :: Maybe Integer
      -- ^ The minimum amount of time you want HTTP error codes to stay in CloudFront
      -- caches before CloudFront queries your origin to see whether the object has
      -- been updated. You can specify a value from 0 to 31,536,000.
    , cerErrorCode :: !Int
      -- ^ The 4xx or 5xx HTTP status code that you want to customize. For a list of
      -- HTTP status codes that you can customize, see CloudFront documentation.
    , cerResponseCode :: Maybe Text
      -- ^ The HTTP status code that you want CloudFront to return with the custom
      -- error page to the viewer. For a list of HTTP status codes that you can
      -- replace, see CloudFront Documentation.
    , cerResponsePagePath :: Maybe Text
      -- ^ The path of the custom error page (for example, /custom_404.html). The path
      -- is relative to the distribution and must begin with a slash (/). If the
      -- path includes any non-ASCII characters or unsafe characters as defined in
      -- RFC 1783 (http://www.ietf.org/rfc/rfc1738.txt), URL encode those
      -- characters. Do not URL encode any other characters in the path, or
      -- CloudFront will not return the custom error page to the viewer.
    } deriving (Eq, Show, Generic)

instance ToQuery CustomErrorResponse

instance FromXML CustomErrorResponse where
    fromXMLOptions = xmlOptions

instance ToXML CustomErrorResponse where
    toXMLOptions = xmlOptions

-- | A complex type that specifies how CloudFront handles cookies.
data CookiePreference = CookiePreference
    { cpForward :: !ItemSelection
      -- ^ Use this element to specify whether you want CloudFront to forward cookies
      -- to the origin that is associated with this cache behavior. You can specify
      -- all, none or whitelist. If you choose All, CloudFront forwards all cookies
      -- regardless of how many your application uses.
    , cpWhitelistedNames :: Maybe CookieNames
      -- ^ A complex type that specifies the whitelisted cookies, if any, that you
      -- want CloudFront to forward to your origin that is associated with this
      -- cache behavior.
    } deriving (Eq, Show, Generic)

instance ToQuery CookiePreference

instance FromXML CookiePreference where
    fromXMLOptions = xmlOptions

instance ToXML CookiePreference where
    toXMLOptions = xmlOptions

-- | A complex type that specifies the whitelisted cookies, if any, that you
-- want CloudFront to forward to your origin that is associated with this
-- cache behavior.
data CookieNames = CookieNames
    { cnItems :: [Text]
      -- ^ Optional: A complex type that contains whitelisted cookies for this cache
      -- behavior. If Quantity is 0, you can omit Items.
    , cnQuantity :: !Int
      -- ^ The number of whitelisted cookies for this cache behavior.
    } deriving (Eq, Show, Generic)

instance ToQuery CookieNames

instance FromXML CookieNames where
    fromXMLOptions = xmlOptions

instance ToXML CookieNames where
    toXMLOptions = xmlOptions

-- | Summary of the information about a CloudFront origin access identity.
data CloudFrontOriginAccessIdentitySummary = CloudFrontOriginAccessIdentitySummary
    { cfoaisComment :: !Text
      -- ^ The comment for this origin access identity, as originally specified when
      -- created.
    , cfoaisId :: !Text
      -- ^ The ID for the origin access identity. For example: E74FTE3AJFJ256A.
    , cfoaisS3CanonicalUserId :: !Text
      -- ^ The Amazon S3 canonical user ID for the origin access identity, which you
      -- use when giving the origin access identity read permission to an object in
      -- Amazon S3.
    } deriving (Eq, Show, Generic)

instance ToQuery CloudFrontOriginAccessIdentitySummary

instance FromXML CloudFrontOriginAccessIdentitySummary where
    fromXMLOptions = xmlOptions

instance ToXML CloudFrontOriginAccessIdentitySummary where
    toXMLOptions = xmlOptions

-- | The CloudFrontOriginAccessIdentityList type.
data CloudFrontOriginAccessIdentityList = CloudFrontOriginAccessIdentityList
    { cfoailIsTruncated :: !Bool
      -- ^ A flag that indicates whether more origin access identities remain to be
      -- listed. If your results were truncated, you can make a follow-up pagination
      -- request using the Marker request parameter to retrieve more items in the
      -- list.
    , cfoailItems :: [CloudFrontOriginAccessIdentitySummary]
      -- ^ A complex type that contains one CloudFrontOriginAccessIdentitySummary
      -- element for each origin access identity that was created by the current AWS
      -- account.
    , cfoailMarker :: !Text
      -- ^ The value you provided for the Marker request parameter.
    , cfoailMaxItems :: !Int
      -- ^ The value you provided for the MaxItems request parameter.
    , cfoailNextMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the value you
      -- can use for the Marker request parameter to continue listing your origin
      -- access identities where they left off.
    , cfoailQuantity :: !Int
      -- ^ The number of CloudFront origin access identities that were created by the
      -- current AWS account.
    } deriving (Eq, Show, Generic)

instance ToQuery CloudFrontOriginAccessIdentityList

instance FromXML CloudFrontOriginAccessIdentityList where
    fromXMLOptions = xmlOptions

instance ToXML CloudFrontOriginAccessIdentityList where
    toXMLOptions = xmlOptions

-- | The identity's configuration information.
data CloudFrontOriginAccessIdentityConfig = CloudFrontOriginAccessIdentityConfig
    { cfoaicCallerReference :: !Text
      -- ^ A unique number that ensures the request can't be replayed. If the
      -- CallerReference is new (no matter the content of the
      -- CloudFrontOriginAccessIdentityConfig object), a new origin access identity
      -- is created. If the CallerReference is a value you already sent in a
      -- previous request to create an identity, and the content of the
      -- CloudFrontOriginAccessIdentityConfig is identical to the original request
      -- (ignoring white space), the response includes the same information returned
      -- to the original request. If the CallerReference is a value you already sent
      -- in a previous request to create an identity but the content of the
      -- CloudFrontOriginAccessIdentityConfig is different from the original
      -- request, CloudFront returns a CloudFrontOriginAccessIdentityAlreadyExists
      -- error.
    , cfoaicComment :: !Text
      -- ^ Any comments you want to include about the origin access identity.
    } deriving (Eq, Show, Generic)

instance ToQuery CloudFrontOriginAccessIdentityConfig

instance FromXML CloudFrontOriginAccessIdentityConfig where
    fromXMLOptions = xmlOptions

instance ToXML CloudFrontOriginAccessIdentityConfig where
    toXMLOptions = xmlOptions

-- | The origin access identity's information.
data CloudFrontOriginAccessIdentity = CloudFrontOriginAccessIdentity
    { cfoaiCloudFrontOriginAccessIdentityConfig :: Maybe CloudFrontOriginAccessIdentityConfig
      -- ^ The current configuration information for the identity.
    , cfoaiId :: !Text
      -- ^ The ID for the origin access identity. For example: E74FTE3AJFJ256A.
    , cfoaiS3CanonicalUserId :: !Text
      -- ^ The Amazon S3 canonical user ID for the origin access identity, which you
      -- use when giving the origin access identity read permission to an object in
      -- Amazon S3.
    } deriving (Eq, Show, Generic)

instance ToQuery CloudFrontOriginAccessIdentity

instance FromXML CloudFrontOriginAccessIdentity where
    fromXMLOptions = xmlOptions

instance ToXML CloudFrontOriginAccessIdentity where
    toXMLOptions = xmlOptions

-- | A complex type that contains zero or more CacheBehavior elements.
data CacheBehaviors = CacheBehaviors
    { ccItems :: [CacheBehavior]
      -- ^ Optional: A complex type that contains cache behaviors for this
      -- distribution. If Quantity is 0, you can omit Items.
    , ccQuantity :: !Int
      -- ^ The number of cache behaviors for this distribution.
    } deriving (Eq, Show, Generic)

instance ToQuery CacheBehaviors

instance FromXML CacheBehaviors where
    fromXMLOptions = xmlOptions

instance ToXML CacheBehaviors where
    toXMLOptions = xmlOptions

-- | A complex type that describes how CloudFront processes requests. You can
-- create up to 10 cache behaviors.You must create at least as many cache
-- behaviors (including the default cache behavior) as you have origins if you
-- want CloudFront to distribute objects from all of the origins. Each cache
-- behavior specifies the one origin from which you want CloudFront to get
-- objects. If you have two origins and only the default cache behavior, the
-- default cache behavior will cause CloudFront to get objects from one of the
-- origins, but the other origin will never be used. If you don't want to
-- specify any cache behaviors, include only an empty CacheBehaviors element.
-- Don't include an empty CacheBehavior element, or CloudFront returns a
-- MalformedXML error. To delete all cache behaviors in an existing
-- distribution, update the distribution configuration and include only an
-- empty CacheBehaviors element. To add, change, or remove one or more cache
-- behaviors, update the distribution configuration and specify all of the
-- cache behaviors that you want to include in the updated distribution.
data CacheBehavior = CacheBehavior
    { cbAllowedMethods :: Maybe AllowedMethods
      -- ^ A complex type that controls which HTTP methods CloudFront processes and
      -- forwards to your Amazon S3 bucket or your custom origin. There are two
      -- options: - CloudFront forwards only GET and HEAD requests. - CloudFront
      -- forwards DELETE, GET, HEAD, OPTIONS, PATCH, POST, and PUT requests. If you
      -- choose the second option, you may need to restrict access to your Amazon S3
      -- bucket or to your custom origin so users can't perform operations that you
      -- don't want them to. For example, you may not want users to have permission
      -- to delete objects from your origin.
    , cbForwardedValues :: ForwardedValues
      -- ^ A complex type that specifies how CloudFront handles query strings and
      -- cookies.
    , cbMinTTL :: !Integer
      -- ^ The minimum amount of time that you want objects to stay in CloudFront
      -- caches before CloudFront queries your origin to see whether the object has
      -- been updated.You can specify a value from 0 to 3,153,600,000 seconds (100
      -- years).
    , cbPathPattern :: !Text
      -- ^ The pattern (for example, images/*.jpg) that specifies which requests you
      -- want this cache behavior to apply to. When CloudFront receives an end-user
      -- request, the requested path is compared with path patterns in the order in
      -- which cache behaviors are listed in the distribution. The path pattern for
      -- the default cache behavior is * and cannot be changed. If the request for
      -- an object does not match the path pattern for any cache behaviors,
      -- CloudFront applies the behavior in the default cache behavior.
    , cbTargetOriginId :: !Text
      -- ^ The value of ID for the origin that you want CloudFront to route requests
      -- to when a request matches the path pattern either for a cache behavior or
      -- for the default cache behavior.
    , cbTrustedSigners :: TrustedSigners
      -- ^ A complex type that specifies the AWS accounts, if any, that you want to
      -- allow to create signed URLs for private content. If you want to require
      -- signed URLs in requests for objects in the target origin that match the
      -- PathPattern for this cache behavior, specify true for Enabled, and specify
      -- the applicable values for Quantity and Items. For more information, go to
      -- Using a Signed URL to Serve Private Content in the Amazon CloudFront
      -- Developer Guide. If you don't want to require signed URLs in requests for
      -- objects that match PathPattern, specify false for Enabled and 0 for
      -- Quantity. Omit Items. To add, change, or remove one or more trusted
      -- signers, change Enabled to true (if it's currently false), change Quantity
      -- as applicable, and specify all of the trusted signers that you want to
      -- include in the updated distribution.
    , cbViewerProtocolPolicy :: !ViewerProtocolPolicy
      -- ^ Use this element to specify the protocol that users can use to access the
      -- files in the origin specified by TargetOriginId when a request matches the
      -- path pattern in PathPattern. If you want CloudFront to allow end users to
      -- use any available protocol, specify allow-all. If you want CloudFront to
      -- require HTTPS, specify https.
    } deriving (Eq, Show, Generic)

instance ToQuery CacheBehavior

instance FromXML CacheBehavior where
    fromXMLOptions = xmlOptions

instance ToXML CacheBehavior where
    toXMLOptions = xmlOptions

-- | A complex type that controls which HTTP methods CloudFront processes and
-- forwards to your Amazon S3 bucket or your custom origin. There are two
-- options: - CloudFront forwards only GET and HEAD requests. - CloudFront
-- forwards DELETE, GET, HEAD, OPTIONS, PATCH, POST, and PUT requests. If you
-- choose the second option, you may need to restrict access to your Amazon S3
-- bucket or to your custom origin so users can't perform operations that you
-- don't want them to. For example, you may not want users to have permission
-- to delete objects from your origin.
data AllowedMethods = AllowedMethods
    { amItems :: [Method]
      -- ^ A complex type that contains the HTTP methods that you want CloudFront to
      -- process and forward to your origin.
    , amQuantity :: !Int
      -- ^ The number of HTTP methods that you want CloudFront to forward to your
      -- origin. Valid values are 2 (for GET and HEAD requests) and 7 (for DELETE,
      -- GET, HEAD, OPTIONS, PATCH, POST, and PUT requests).
    } deriving (Eq, Show, Generic)

instance ToQuery AllowedMethods

instance FromXML AllowedMethods where
    fromXMLOptions = xmlOptions

instance ToXML AllowedMethods where
    toXMLOptions = xmlOptions

-- | A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this streaming distribution.
data Aliases = Aliases
    { aItems :: [Text]
      -- ^ Optional: A complex type that contains CNAME elements, if any, for this
      -- distribution. If Quantity is 0, you can omit Items.
    , aQuantity :: !Int
      -- ^ The number of CNAMEs, if any, for this distribution.
    } deriving (Eq, Show, Generic)

instance ToQuery Aliases

instance FromXML Aliases where
    fromXMLOptions = xmlOptions

instance ToXML Aliases where
    toXMLOptions = xmlOptions

-- | CloudFront automatically adds this element to the response only if you've
-- set up the distribution to serve private content with signed URLs. The
-- element lists the key pair IDs that CloudFront is aware of for each trusted
-- signer. The Signer child element lists the AWS account number of the
-- trusted signer (or an empty Self element if the signer is you). The Signer
-- element also includes the IDs of any active key pairs associated with the
-- trusted signer's AWS account. If no KeyPairId element appears for a Signer,
-- that signer can't create working signed URLs.
data ActiveTrustedSigners = ActiveTrustedSigners
    { atsEnabled :: !Bool
      -- ^ Each active trusted signer.
    , atsItems :: [Signer]
      -- ^ A complex type that contains one Signer complex type for each unique
      -- trusted signer that is specified in the TrustedSigners complex type,
      -- including trusted signers in the default cache behavior and in all of the
      -- other cache behaviors.
    , atsQuantity :: !Int
      -- ^ The number of unique trusted signers included in all cache behaviors. For
      -- example, if three cache behaviors all list the same three AWS accounts, the
      -- value of Quantity for ActiveTrustedSigners will be 3.
    } deriving (Eq, Show, Generic)

instance ToQuery ActiveTrustedSigners

instance FromXML ActiveTrustedSigners where
    fromXMLOptions = xmlOptions

instance ToXML ActiveTrustedSigners where
    toXMLOptions = xmlOptions

-- | Use this element to specify the protocol that users can use to access the
-- files in the origin specified by TargetOriginId when a request matches the
-- path pattern in PathPattern. If you want CloudFront to allow end users to
-- use any available protocol, specify allow-all. If you want CloudFront to
-- require HTTPS, specify https.
data ViewerProtocolPolicy
    = ViewerProtocolPolicyAllowAll
    | ViewerProtocolPolicyHttpsOnly
      deriving (Eq, Ord, Generic)

instance Hashable ViewerProtocolPolicy

instance FromText ViewerProtocolPolicy where
    fromText "allow-all" = Right ViewerProtocolPolicyAllowAll
    fromText "https-only" = Right ViewerProtocolPolicyHttpsOnly
    fromText e = fromTextFail $ "Unrecognised ViewerProtocolPolicy: " <> e

instance Read ViewerProtocolPolicy where
    readsPrec _ = fromTextRead

instance ToText ViewerProtocolPolicy where
    toText ViewerProtocolPolicyAllowAll = "allow-all"
    toText ViewerProtocolPolicyHttpsOnly = "https-only"

instance Show ViewerProtocolPolicy where
    show = toTextShow

instance ToQuery ViewerProtocolPolicy where
    toQuery = toTextQuery

instance FromXML ViewerProtocolPolicy where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML ViewerProtocolPolicy where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | A complex type that contains information about price class for this
-- streaming distribution.
data PriceClass
    = PriceClassPriceClass_100
    | PriceClassPriceClass_200
    | PriceClassPriceClass_All
      deriving (Eq, Ord, Generic)

instance Hashable PriceClass

instance FromText PriceClass where
    fromText "PriceClass_100" = Right PriceClassPriceClass_100
    fromText "PriceClass_200" = Right PriceClassPriceClass_200
    fromText "PriceClass_All" = Right PriceClassPriceClass_All
    fromText e = fromTextFail $ "Unrecognised PriceClass: " <> e

instance Read PriceClass where
    readsPrec _ = fromTextRead

instance ToText PriceClass where
    toText PriceClassPriceClass_100 = "PriceClass_100"
    toText PriceClassPriceClass_200 = "PriceClass_200"
    toText PriceClassPriceClass_All = "PriceClass_All"

instance Show PriceClass where
    show = toTextShow

instance ToQuery PriceClass where
    toQuery = toTextQuery

instance FromXML PriceClass where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML PriceClass where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The origin protocol policy to apply to your origin.
data OriginProtocolPolicy
    = OriginProtocolPolicyHttpOnly
    | OriginProtocolPolicyMatchViewer
      deriving (Eq, Ord, Generic)

instance Hashable OriginProtocolPolicy

instance FromText OriginProtocolPolicy where
    fromText "http-only" = Right OriginProtocolPolicyHttpOnly
    fromText "match-viewer" = Right OriginProtocolPolicyMatchViewer
    fromText e = fromTextFail $ "Unrecognised OriginProtocolPolicy: " <> e

instance Read OriginProtocolPolicy where
    readsPrec _ = fromTextRead

instance ToText OriginProtocolPolicy where
    toText OriginProtocolPolicyHttpOnly = "http-only"
    toText OriginProtocolPolicyMatchViewer = "match-viewer"

instance Show OriginProtocolPolicy where
    show = toTextShow

instance ToQuery OriginProtocolPolicy where
    toQuery = toTextQuery

instance FromXML OriginProtocolPolicy where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML OriginProtocolPolicy where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for Method
data Method
    = MethodDELETE
    | MethodGET
    | MethodHEAD
    | MethodOPTIONS
    | MethodPATCH
    | MethodPOST
    | MethodPUT
      deriving (Eq, Ord, Generic)

instance Hashable Method

instance FromText Method where
    fromText "DELETE" = Right MethodDELETE
    fromText "GET" = Right MethodGET
    fromText "HEAD" = Right MethodHEAD
    fromText "OPTIONS" = Right MethodOPTIONS
    fromText "PATCH" = Right MethodPATCH
    fromText "POST" = Right MethodPOST
    fromText "PUT" = Right MethodPUT
    fromText e = fromTextFail $ "Unrecognised Method: " <> e

instance Read Method where
    readsPrec _ = fromTextRead

instance ToText Method where
    toText MethodDELETE = "DELETE"
    toText MethodGET = "GET"
    toText MethodHEAD = "HEAD"
    toText MethodOPTIONS = "OPTIONS"
    toText MethodPATCH = "PATCH"
    toText MethodPOST = "POST"
    toText MethodPUT = "PUT"

instance Show Method where
    show = toTextShow

instance ToQuery Method where
    toQuery = toTextQuery

instance FromXML Method where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML Method where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | Use this element to specify whether you want CloudFront to forward cookies
-- to the origin that is associated with this cache behavior. You can specify
-- all, none or whitelist. If you choose All, CloudFront forwards all cookies
-- regardless of how many your application uses.
data ItemSelection
    = ItemSelectionAll
    | ItemSelectionNone
    | ItemSelectionWhitelist
      deriving (Eq, Ord, Generic)

instance Hashable ItemSelection

instance FromText ItemSelection where
    fromText "all" = Right ItemSelectionAll
    fromText "none" = Right ItemSelectionNone
    fromText "whitelist" = Right ItemSelectionWhitelist
    fromText e = fromTextFail $ "Unrecognised ItemSelection: " <> e

instance Read ItemSelection where
    readsPrec _ = fromTextRead

instance ToText ItemSelection where
    toText ItemSelectionAll = "all"
    toText ItemSelectionNone = "none"
    toText ItemSelectionWhitelist = "whitelist"

instance Show ItemSelection where
    show = toTextShow

instance ToQuery ItemSelection where
    toQuery = toTextQuery

instance FromXML ItemSelection where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML ItemSelection where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The method that you want to use to restrict distribution of your content by
-- country: - none: No geo restriction is enabled, meaning access to content
-- is not restricted by client geo location. - blacklist: The Location
-- elements specify the countries in which you do not want CloudFront to
-- distribute your content. - whitelist: The Location elements specify the
-- countries in which you want CloudFront to distribute your content.
data GeoRestrictionType
    = GeoRestrictionTypeBlacklist
    | GeoRestrictionTypeNone
    | GeoRestrictionTypeWhitelist
      deriving (Eq, Ord, Generic)

instance Hashable GeoRestrictionType

instance FromText GeoRestrictionType where
    fromText "blacklist" = Right GeoRestrictionTypeBlacklist
    fromText "none" = Right GeoRestrictionTypeNone
    fromText "whitelist" = Right GeoRestrictionTypeWhitelist
    fromText e = fromTextFail $ "Unrecognised GeoRestrictionType: " <> e

instance Read GeoRestrictionType where
    readsPrec _ = fromTextRead

instance ToText GeoRestrictionType where
    toText GeoRestrictionTypeBlacklist = "blacklist"
    toText GeoRestrictionTypeNone = "none"
    toText GeoRestrictionTypeWhitelist = "whitelist"

instance Show GeoRestrictionType where
    show = toTextShow

instance ToQuery GeoRestrictionType where
    toQuery = toTextQuery

instance FromXML GeoRestrictionType where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML GeoRestrictionType where
    toXMLOptions = xmlOptions
    toXML        = toTextXML
