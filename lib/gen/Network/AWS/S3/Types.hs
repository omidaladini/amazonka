{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.S3.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.S3.Types where

import Network.AWS.Core
import Network.AWS.S3.Service

-- | FIXME: Type documentation for WebsiteConfiguration
data WebsiteConfiguration = WebsiteConfiguration
    { wcErrorDocument :: Maybe ErrorDocument
    , wcIndexDocument :: Maybe IndexDocument
    , wcRedirectAllRequestsTo :: Maybe RedirectAllRequestsTo
    , wcRoutingRules :: [RoutingRule]
    } deriving (Eq, Show, Generic)

instance ToQuery WebsiteConfiguration

instance FromXML WebsiteConfiguration where
    fromXMLOptions = xmlOptions

instance ToXML WebsiteConfiguration where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for VersioningConfiguration
data VersioningConfiguration = VersioningConfiguration
    { vcMfaDelete :: Maybe MfaDelete
      -- ^ Specifies whether MFA delete is enabled in the bucket versioning
      -- configuration. This element is only returned if the bucket has been
      -- configured with MFA delete. If the bucket has never been so configured,
      -- this element is not returned.
    , vcStatus :: Maybe Status
      -- ^ The versioning state of the bucket.
    } deriving (Eq, Show, Generic)

instance ToQuery VersioningConfiguration

instance FromXML VersioningConfiguration where
    fromXMLOptions = xmlOptions

instance ToXML VersioningConfiguration where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for Version
data Version = Version
    { vETag :: Maybe Text
    , vIsLatest :: Maybe Bool
      -- ^ Specifies whether the object is (true) or is not (false) the latest version
      -- of an object.
    , vKey :: Maybe Text
      -- ^ The object key.
    , vLastModified :: Maybe UTCTime
      -- ^ Date and time the object was last modified.
    , vOwner :: Maybe Owner
    , vSize :: Maybe Text
      -- ^ Size in bytes of the object.
    , vStorageClass :: Maybe StorageClass
      -- ^ The class of storage used to store the object.
    , vVersionId :: Maybe Text
      -- ^ Version ID of an object.
    } deriving (Eq, Show, Generic)

instance ToQuery Version

instance FromXML Version where
    fromXMLOptions = xmlOptions

instance ToXML Version where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for Upload
data Upload = Upload
    { uInitiated :: Maybe UTCTime
      -- ^ Date and time at which the multipart upload was initiated.
    , uInitiator :: Maybe Initiator
      -- ^ Identifies who initiated the multipart upload.
    , uKey :: Maybe Text
      -- ^ Key of the object for which the multipart upload was initiated.
    , uOwner :: Maybe Owner
    , uStorageClass :: Maybe StorageClass
      -- ^ The class of storage used to store the object.
    , uUploadId :: Maybe Text
      -- ^ Upload ID that identifies the multipart upload.
    } deriving (Eq, Show, Generic)

instance ToQuery Upload

instance FromXML Upload where
    fromXMLOptions = xmlOptions

instance ToXML Upload where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for Transition
data Transition = Transition
    { teDate :: Maybe UTCTime
      -- ^ Indicates at what date the object is to be moved or deleted. Should be in
      -- GMT ISO 8601 Format.
    , teDays :: Maybe Int
      -- ^ Indicates the lifetime, in days, of the objects that are subject to the
      -- rule. The value must be a non-zero positive integer.
    , teStorageClass :: Maybe StorageClass
      -- ^ The class of storage used to store the object.
    } deriving (Eq, Show, Generic)

instance ToQuery Transition

instance FromXML Transition where
    fromXMLOptions = xmlOptions

instance ToXML Transition where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for TopicConfiguration
data TopicConfiguration = TopicConfiguration
    { tcEvent :: Maybe Event
      -- ^ Bucket event for which to send notifications.
    , tcTopic :: Maybe Text
      -- ^ Amazon SNS topic to which Amazon S3 will publish a message to report the
      -- specified events for the bucket.
    } deriving (Eq, Show, Generic)

instance ToQuery TopicConfiguration

instance FromXML TopicConfiguration where
    fromXMLOptions = xmlOptions

instance ToXML TopicConfiguration where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for Tagging
newtype Tagging = Tagging
    { tdTagSet :: [Tag]
      -- ^ FIXME: Type documentation for TagSet
    } deriving (Eq, Show, Generic)

instance ToQuery Tagging

instance FromXML Tagging where
    fromXMLOptions = xmlOptions

instance ToXML Tagging where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for Tag
data Tag = Tag
    { tKey :: !Text
      -- ^ Name of the tag.
    , tValue :: !Text
      -- ^ Value of the tag.
    } deriving (Eq, Show, Generic)

instance ToQuery Tag

instance FromXML Tag where
    fromXMLOptions = xmlOptions

instance ToXML Tag where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for Rule
data Rule = Rule
    { rdExpiration :: Maybe Expiration
    , rdID :: Maybe Text
      -- ^ Unique identifier for the rule. The value cannot be longer than 255
      -- characters.
    , rdPrefix :: Maybe Text
      -- ^ Prefix identifying one or more objects to which the rule applies.
    , rdStatus :: Maybe Status
      -- ^ If 'Enabled', the rule is currently being applied. If 'Disabled', the rule
      -- is not currently being applied.
    , rdTransition :: Maybe Transition
    } deriving (Eq, Show, Generic)

instance ToQuery Rule

instance FromXML Rule where
    fromXMLOptions = xmlOptions

instance ToXML Rule where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for RoutingRule
data RoutingRule = RoutingRule
    { rrdCondition :: Maybe Condition
      -- ^ A container for describing a condition that must be met for the specified
      -- redirect to apply. For example, 1. If request is for pages in the /docs
      -- folder, redirect to the /documents folder. 2. If request results in HTTP
      -- error 4xx, redirect request to another host where you might process the
      -- error.
    , rrdRedirect :: Maybe Redirect
      -- ^ Container for redirect information. You can redirect requests to another
      -- host, to another page, or with another protocol. In the event of an error,
      -- you can can specify a different error code to return.
    } deriving (Eq, Show, Generic)

instance ToQuery RoutingRule

instance FromXML RoutingRule where
    fromXMLOptions = xmlOptions

instance ToXML RoutingRule where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for RestoreRequest
newtype RestoreRequest = RestoreRequest
    { rrDays :: Int
      -- ^ Lifetime of the active copy in days.
    } deriving (Eq, Show, Generic)

instance ToQuery RestoreRequest

instance FromXML RestoreRequest where
    fromXMLOptions = xmlOptions

instance ToXML RestoreRequest where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for RequestPaymentConfiguration
newtype RequestPaymentConfiguration = RequestPaymentConfiguration
    { rpcPayer :: Payer
      -- ^ Specifies who pays for the download and request fees.
    } deriving (Eq, Show, Generic)

instance ToQuery RequestPaymentConfiguration

instance FromXML RequestPaymentConfiguration where
    fromXMLOptions = xmlOptions

instance ToXML RequestPaymentConfiguration where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for RedirectAllRequestsTo
data RedirectAllRequestsTo = RedirectAllRequestsTo
    { rartHostName :: Maybe Text
      -- ^ Name of the host where requests will be redirected.
    , rartProtocol :: Maybe Protocol
      -- ^ Protocol to use (http, https) when redirecting requests. The default is the
      -- protocol that is used in the original request.
    } deriving (Eq, Show, Generic)

instance ToQuery RedirectAllRequestsTo

instance FromXML RedirectAllRequestsTo where
    fromXMLOptions = xmlOptions

instance ToXML RedirectAllRequestsTo where
    toXMLOptions = xmlOptions

-- | Container for redirect information. You can redirect requests to another
-- host, to another page, or with another protocol. In the event of an error,
-- you can can specify a different error code to return.
data Redirect = Redirect
    { rHostName :: Maybe Text
      -- ^ The host name to use in the redirect request.
    , rHttpRedirectCode :: Maybe Text
      -- ^ The HTTP redirect code to use on the response. Not required if one of the
      -- siblings is present.
    , rProtocol :: Maybe Protocol
      -- ^ Protocol to use (http, https) when redirecting requests. The default is the
      -- protocol that is used in the original request.
    , rReplaceKeyPrefixWith :: Maybe Text
      -- ^ The object key prefix to use in the redirect request. For example, to
      -- redirect requests for all pages with prefix docs/ (objects in the docs/
      -- folder) to documents/, you can set a condition block with KeyPrefixEquals
      -- set to docs/ and in the Redirect set ReplaceKeyPrefixWith to /documents.
      -- Not required if one of the siblings is present. Can be present only if
      -- ReplaceKeyWith is not provided.
    , rReplaceKeyWith :: Maybe Text
      -- ^ The specific object key to use in the redirect request. For example,
      -- redirect request to error.html. Not required if one of the sibling is
      -- present. Can be present only if ReplaceKeyPrefixWith is not provided.
    } deriving (Eq, Show, Generic)

instance ToQuery Redirect

instance FromXML Redirect where
    fromXMLOptions = xmlOptions

instance ToXML Redirect where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for Part
data Part = Part
    { pETag :: Maybe Text
      -- ^ Entity tag returned when the part was uploaded.
    , pPartNumber :: Maybe Int
      -- ^ Part number that identifies the part.
    } deriving (Eq, Show, Generic)

instance ToQuery Part

instance FromXML Part where
    fromXMLOptions = xmlOptions

instance ToXML Part where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for Owner
data Owner = Owner
    { odDisplayName :: Maybe Text
    , odID :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery Owner

instance FromXML Owner where
    fromXMLOptions = xmlOptions

instance ToXML Owner where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for Object
data Object = Object
    { oKey :: !Text
      -- ^ Key name of the object to delete.
    , oVersionId :: Maybe Text
      -- ^ VersionId for the specific version of the object to delete.
    } deriving (Eq, Show, Generic)

instance ToQuery Object

instance FromXML Object where
    fromXMLOptions = xmlOptions

instance ToXML Object where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for NotificationConfiguration
newtype NotificationConfiguration = NotificationConfiguration
    { ncTopicConfiguration :: TopicConfiguration
      -- ^ FIXME: Type documentation for TopicConfiguration
    } deriving (Eq, Show, Generic)

instance ToQuery NotificationConfiguration

instance FromXML NotificationConfiguration where
    fromXMLOptions = xmlOptions

instance ToXML NotificationConfiguration where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for MultipartUpload
newtype MultipartUpload = MultipartUpload
    { mudParts :: [Part]
      -- ^ FIXME: Type documentation for Part
    } deriving (Eq, Show, Generic)

instance ToQuery MultipartUpload

instance FromXML MultipartUpload where
    fromXMLOptions = xmlOptions

instance ToXML MultipartUpload where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for LoggingEnabled
data LoggingEnabled = LoggingEnabled
    { leTargetBucket :: Maybe Text
      -- ^ Specifies the bucket where you want Amazon S3 to store server access logs.
      -- You can have your logs delivered to any bucket that you own, including the
      -- same bucket that is being logged. You can also configure multiple buckets
      -- to deliver their logs to the same target bucket. In this case you should
      -- choose a different TargetPrefix for each source bucket so that the
      -- delivered log files can be distinguished by key.
    , leTargetGrants :: [Grant]
    , leTargetPrefix :: Maybe Text
      -- ^ This element lets you specify a prefix for the keys that the log files will
      -- be stored under.
    } deriving (Eq, Show, Generic)

instance ToQuery LoggingEnabled

instance FromXML LoggingEnabled where
    fromXMLOptions = xmlOptions

instance ToXML LoggingEnabled where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for LifecycleConfiguration
newtype LifecycleConfiguration = LifecycleConfiguration
    { lcRules :: [Rule]
      -- ^ FIXME: Type documentation for Rule
    } deriving (Eq, Show, Generic)

instance ToQuery LifecycleConfiguration

instance FromXML LifecycleConfiguration where
    fromXMLOptions = xmlOptions

instance ToXML LifecycleConfiguration where
    toXMLOptions = xmlOptions

-- | Identifies who initiated the multipart upload.
data Initiator = Initiator
    { iDisplayName :: Maybe Text
      -- ^ Name of the Principal.
    , iID :: Maybe Text
      -- ^ If the principal is an AWS account, it provides the Canonical User ID. If
      -- the principal is an IAM User, it provides a user ARN value.
    } deriving (Eq, Show, Generic)

instance ToQuery Initiator

instance FromXML Initiator where
    fromXMLOptions = xmlOptions

instance ToXML Initiator where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for IndexDocument
newtype IndexDocument = IndexDocument
    { idSuffix :: Maybe Text
      -- ^ A suffix that is appended to a request that is for a directory on the
      -- website endpoint (e.g. if the suffix is index.html and you make a request
      -- to samplebucket/images/ the data that is returned will be for the object
      -- with the key name images/index.html) The suffix must not be empty and must
      -- not include a slash character.
    } deriving (Eq, Show, Generic)

instance ToQuery IndexDocument

instance FromXML IndexDocument where
    fromXMLOptions = xmlOptions

instance ToXML IndexDocument where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for Grantee
data Grantee = Grantee
    { gdDisplayName :: Maybe Text
      -- ^ Screen name of the grantee.
    , gdEmailAddress :: Maybe Text
      -- ^ Email address of the grantee.
    , gdID :: Maybe Text
      -- ^ The canonical user ID of the grantee.
    , gdType :: !GranteeType
      -- ^ Type of grantee.
    , gdURI :: Maybe Text
      -- ^ URI of the grantee group.
    } deriving (Eq, Show, Generic)

instance ToQuery Grantee

instance FromXML Grantee where
    fromXMLOptions = xmlOptions

instance ToXML Grantee where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for Grant
data Grant = Grant
    { gGrantee :: Maybe Grantee
    , gPermission :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery Grant

instance FromXML Grant where
    fromXMLOptions = xmlOptions

instance ToXML Grant where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for Expiration
data Expiration = Expiration
    { eeDate :: Maybe UTCTime
      -- ^ Indicates at what date the object is to be moved or deleted. Should be in
      -- GMT ISO 8601 Format.
    , eeDays :: Maybe Int
      -- ^ Indicates the lifetime, in days, of the objects that are subject to the
      -- rule. The value must be a non-zero positive integer.
    } deriving (Eq, Show, Generic)

instance ToQuery Expiration

instance FromXML Expiration where
    fromXMLOptions = xmlOptions

instance ToXML Expiration where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for ErrorDocument
newtype ErrorDocument = ErrorDocument
    { edKey :: Maybe Text
      -- ^ The object key name to use when a 4XX class error occurs.
    } deriving (Eq, Show, Generic)

instance ToQuery ErrorDocument

instance FromXML ErrorDocument where
    fromXMLOptions = xmlOptions

instance ToXML ErrorDocument where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for Error
data Error = Error
    { eCode :: Maybe Text
    , eKey :: Maybe Text
    , eMessage :: Maybe Text
    , eVersionId :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery Error

instance FromXML Error where
    fromXMLOptions = xmlOptions

instance ToXML Error where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for Deleted
data Deleted = Deleted
    { ddDeleteMarker :: Maybe Bool
    , ddDeleteMarkerVersionId :: Maybe Text
    , ddKey :: Maybe Text
    , ddVersionId :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery Deleted

instance FromXML Deleted where
    fromXMLOptions = xmlOptions

instance ToXML Deleted where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for DeleteMarker
data DeleteMarker = DeleteMarker
    { dmIsLatest :: Maybe Bool
      -- ^ Specifies whether the object is (true) or is not (false) the latest version
      -- of an object.
    , dmKey :: Maybe Text
      -- ^ The object key.
    , dmLastModified :: Maybe UTCTime
      -- ^ Date and time the object was last modified.
    , dmOwner :: Maybe Owner
    , dmVersionId :: Maybe Text
      -- ^ Version ID of an object.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteMarker

instance FromXML DeleteMarker where
    fromXMLOptions = xmlOptions

instance ToXML DeleteMarker where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for Delete
data Delete = Delete
    { dObject :: [Object]
    , dQuiet :: Maybe Bool
      -- ^ Element to enable quiet mode for the request. When you add this element,
      -- you must set its value to true.
    } deriving (Eq, Show, Generic)

instance ToQuery Delete

instance FromXML Delete where
    fromXMLOptions = xmlOptions

instance ToXML Delete where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for CreateBucketConfiguration
newtype CreateBucketConfiguration = CreateBucketConfiguration
    { cbcLocationConstraint :: Maybe LocationConstraint
      -- ^ Specifies the region where the bucket will be created.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateBucketConfiguration

instance FromXML CreateBucketConfiguration where
    fromXMLOptions = xmlOptions

instance ToXML CreateBucketConfiguration where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for CopyPartResult
data CopyPartResult = CopyPartResult
    { cprETag :: Maybe Text
      -- ^ Entity tag of the object.
    , cprLastModified :: Maybe UTCTime
      -- ^ Date and time at which the object was uploaded.
    } deriving (Eq, Show, Generic)

instance ToQuery CopyPartResult

instance FromXML CopyPartResult where
    fromXMLOptions = xmlOptions

instance ToXML CopyPartResult where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for CopyObjectResult
data CopyObjectResult = CopyObjectResult
    { cordETag :: Maybe Text
    , cordLastModified :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery CopyObjectResult

instance FromXML CopyObjectResult where
    fromXMLOptions = xmlOptions

instance ToXML CopyObjectResult where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for Contents
data Contents = Contents
    { cdETag :: Maybe Text
    , cdKey :: Maybe Text
    , cdLastModified :: Maybe UTCTime
    , cdOwner :: Maybe Owner
    , cdSize :: Maybe Int
    , cdStorageClass :: Maybe StorageClass
      -- ^ The class of storage used to store the object.
    } deriving (Eq, Show, Generic)

instance ToQuery Contents

instance FromXML Contents where
    fromXMLOptions = xmlOptions

instance ToXML Contents where
    toXMLOptions = xmlOptions

-- | A container for describing a condition that must be met for the specified
-- redirect to apply. For example, 1. If request is for pages in the /docs
-- folder, redirect to the /documents folder. 2. If request results in HTTP
-- error 4xx, redirect request to another host where you might process the
-- error.
data Condition = Condition
    { cHttpErrorCodeReturnedEquals :: Maybe Text
      -- ^ The HTTP error code when the redirect is applied. In the event of an error,
      -- if the error code equals this value, then the specified redirect is
      -- applied. Required when parent element Condition is specified and sibling
      -- KeyPrefixEquals is not specified. If both are specified, then both must be
      -- true for the redirect to be applied.
    , cKeyPrefixEquals :: Maybe Text
      -- ^ The object key name prefix when the redirect is applied. For example, to
      -- redirect requests for ExamplePage.html, the key prefix will be
      -- ExamplePage.html. To redirect request for all pages with the prefix docs/,
      -- the key prefix will be /docs, which identifies all objects in the docs/
      -- folder. Required when the parent element Condition is specified and sibling
      -- HttpErrorCodeReturnedEquals is not specified. If both conditions are
      -- specified, both must be true for the redirect to be applied.
    } deriving (Eq, Show, Generic)

instance ToQuery Condition

instance FromXML Condition where
    fromXMLOptions = xmlOptions

instance ToXML Condition where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for CommonPrefixes
newtype CommonPrefixes = CommonPrefixes
    { cpPrefix :: Maybe Text
      -- ^ FIXME: Type documentation for Text
    } deriving (Eq, Show, Generic)

instance ToQuery CommonPrefixes

instance FromXML CommonPrefixes where
    fromXMLOptions = xmlOptions

instance ToXML CommonPrefixes where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for CORSRule
data CORSRule = CORSRule
    { corsrAllowedHeader :: [Text]
      -- ^ Specifies which headers are allowed in a pre-flight OPTIONS request.
    , corsrAllowedMethod :: [Text]
      -- ^ Identifies HTTP methods that the domain/origin specified in the rule is
      -- allowed to execute.
    , corsrAllowedOrigin :: [Text]
      -- ^ One or more origins you want customers to be able to access the bucket
      -- from.
    , corsrExposeHeader :: [Text]
      -- ^ One or more headers in the response that you want customers to be able to
      -- access from their applications (for example, from a JavaScript
      -- XMLHttpRequest object).
    , corsrMaxAgeSeconds :: Maybe Int
      -- ^ The time in seconds that your browser is to cache the preflight response
      -- for the specified resource.
    } deriving (Eq, Show, Generic)

instance ToQuery CORSRule

instance FromXML CORSRule where
    fromXMLOptions = xmlOptions

instance ToXML CORSRule where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for CORSConfiguration
newtype CORSConfiguration = CORSConfiguration
    { corscCORSRules :: [CORSRule]
      -- ^ FIXME: Type documentation for CORSRule
    } deriving (Eq, Show, Generic)

instance ToQuery CORSConfiguration

instance FromXML CORSConfiguration where
    fromXMLOptions = xmlOptions

instance ToXML CORSConfiguration where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for BucketLoggingStatus
newtype BucketLoggingStatus = BucketLoggingStatus
    { blsLoggingEnabled :: Maybe LoggingEnabled
      -- ^ FIXME: Type documentation for LoggingEnabled
    } deriving (Eq, Show, Generic)

instance ToQuery BucketLoggingStatus

instance FromXML BucketLoggingStatus where
    fromXMLOptions = xmlOptions

instance ToXML BucketLoggingStatus where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for Bucket
data Bucket = Bucket
    { bCreationDate :: Maybe UTCTime
      -- ^ Date the bucket was created.
    , bName :: Maybe Text
      -- ^ The name of the bucket.
    } deriving (Eq, Show, Generic)

instance ToQuery Bucket

instance FromXML Bucket where
    fromXMLOptions = xmlOptions

instance ToXML Bucket where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for AccessControlPolicy
data AccessControlPolicy = AccessControlPolicy
    { acpAccessControlList :: [Grant]
      -- ^ A list of grants.
    , acpOwner :: Maybe Owner
    } deriving (Eq, Show, Generic)

instance ToQuery AccessControlPolicy

instance FromXML AccessControlPolicy where
    fromXMLOptions = xmlOptions

instance ToXML AccessControlPolicy where
    toXMLOptions = xmlOptions

-- | The type of storage to use for the object. Defaults to 'STANDARD'.
data StorageClass
    = StorageClassREDUCED_REDUNDANCY
    | StorageClassSTANDARD
      deriving (Eq, Ord, Generic)

instance Hashable StorageClass

instance FromText StorageClass where
    fromText "REDUCED_REDUNDANCY" = Right StorageClassREDUCED_REDUNDANCY
    fromText "STANDARD" = Right StorageClassSTANDARD
    fromText e = fromTextFail $ "Unrecognised StorageClass: " <> e

instance Read StorageClass where
    readsPrec _ = fromTextRead

instance ToText StorageClass where
    toText StorageClassREDUCED_REDUNDANCY = "REDUCED_REDUNDANCY"
    toText StorageClassSTANDARD = "STANDARD"

instance Show StorageClass where
    show = toTextShow

instance ToQuery StorageClass where
    toQuery = toTextQuery

instance FromXML StorageClass where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML StorageClass where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | If 'Enabled', the rule is currently being applied. If 'Disabled', the rule
-- is not currently being applied.
data Status
    = StatusDisabled
    | StatusEnabled
      deriving (Eq, Ord, Generic)

instance Hashable Status

instance FromText Status where
    fromText "Disabled" = Right StatusDisabled
    fromText "Enabled" = Right StatusEnabled
    fromText e = fromTextFail $ "Unrecognised Status: " <> e

instance Read Status where
    readsPrec _ = fromTextRead

instance ToText Status where
    toText StatusDisabled = "Disabled"
    toText StatusEnabled = "Enabled"

instance Show Status where
    show = toTextShow

instance ToQuery Status where
    toQuery = toTextQuery

instance FromXML Status where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML Status where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The Server-side encryption algorithm used when storing this object in S3.
data ServerSideEncryption
    = ServerSideEncryptionAES256
      deriving (Eq, Ord, Generic)

instance Hashable ServerSideEncryption

instance FromText ServerSideEncryption where
    fromText "AES256" = Right ServerSideEncryptionAES256
    fromText e = fromTextFail $ "Unrecognised ServerSideEncryption: " <> e

instance Read ServerSideEncryption where
    readsPrec _ = fromTextRead

instance ToText ServerSideEncryption where
    toText ServerSideEncryptionAES256 = "AES256"

instance Show ServerSideEncryption where
    show = toTextShow

instance ToQuery ServerSideEncryption where
    toQuery = toTextQuery

instance FromXML ServerSideEncryption where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML ServerSideEncryption where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | Protocol to use (http, https) when redirecting requests. The default is the
-- protocol that is used in the original request.
data Protocol
    = ProtocolHttp
    | ProtocolHttps
      deriving (Eq, Ord, Generic)

instance Hashable Protocol

instance FromText Protocol where
    fromText "http" = Right ProtocolHttp
    fromText "https" = Right ProtocolHttps
    fromText e = fromTextFail $ "Unrecognised Protocol: " <> e

instance Read Protocol where
    readsPrec _ = fromTextRead

instance ToText Protocol where
    toText ProtocolHttp = "http"
    toText ProtocolHttps = "https"

instance Show Protocol where
    show = toTextShow

instance ToQuery Protocol where
    toQuery = toTextQuery

instance FromXML Protocol where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML Protocol where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | Specifies the permission given to the grantee.
data Permission
    = PermissionFULL_CONTROL
    | PermissionREAD
    | PermissionREAD_ACP
    | PermissionWRITE
    | PermissionWRITE_ACP
      deriving (Eq, Ord, Generic)

instance Hashable Permission

instance FromText Permission where
    fromText "FULL_CONTROL" = Right PermissionFULL_CONTROL
    fromText "READ" = Right PermissionREAD
    fromText "READ_ACP" = Right PermissionREAD_ACP
    fromText "WRITE" = Right PermissionWRITE
    fromText "WRITE_ACP" = Right PermissionWRITE_ACP
    fromText e = fromTextFail $ "Unrecognised Permission: " <> e

instance Read Permission where
    readsPrec _ = fromTextRead

instance ToText Permission where
    toText PermissionFULL_CONTROL = "FULL_CONTROL"
    toText PermissionREAD = "READ"
    toText PermissionREAD_ACP = "READ_ACP"
    toText PermissionWRITE = "WRITE"
    toText PermissionWRITE_ACP = "WRITE_ACP"

instance Show Permission where
    show = toTextShow

instance ToQuery Permission where
    toQuery = toTextQuery

instance FromXML Permission where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML Permission where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | Specifies who pays for the download and request fees.
data Payer
    = PayerBucketOwner
    | PayerRequester
      deriving (Eq, Ord, Generic)

instance Hashable Payer

instance FromText Payer where
    fromText "BucketOwner" = Right PayerBucketOwner
    fromText "Requester" = Right PayerRequester
    fromText e = fromTextFail $ "Unrecognised Payer: " <> e

instance Read Payer where
    readsPrec _ = fromTextRead

instance ToText Payer where
    toText PayerBucketOwner = "BucketOwner"
    toText PayerRequester = "Requester"

instance Show Payer where
    show = toTextShow

instance ToQuery Payer where
    toQuery = toTextQuery

instance FromXML Payer where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML Payer where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | Specifies whether MFA delete is enabled in the bucket versioning
-- configuration. This element is only returned if the bucket has been
-- configured with MFA delete. If the bucket has never been so configured,
-- this element is not returned.
data MfaDelete
    = MfaDeleteDisabled
    | MfaDeleteEnabled
      deriving (Eq, Ord, Generic)

instance Hashable MfaDelete

instance FromText MfaDelete where
    fromText "Disabled" = Right MfaDeleteDisabled
    fromText "Enabled" = Right MfaDeleteEnabled
    fromText e = fromTextFail $ "Unrecognised MfaDelete: " <> e

instance Read MfaDelete where
    readsPrec _ = fromTextRead

instance ToText MfaDelete where
    toText MfaDeleteDisabled = "Disabled"
    toText MfaDeleteEnabled = "Enabled"

instance Show MfaDelete where
    show = toTextShow

instance ToQuery MfaDelete where
    toQuery = toTextQuery

instance FromXML MfaDelete where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML MfaDelete where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | Specifies whether the metadata is copied from the source object or replaced
-- with metadata provided in the request.
data MetadataDirective
    = MetadataDirectiveCOPY
    | MetadataDirectiveREPLACE
      deriving (Eq, Ord, Generic)

instance Hashable MetadataDirective

instance FromText MetadataDirective where
    fromText "COPY" = Right MetadataDirectiveCOPY
    fromText "REPLACE" = Right MetadataDirectiveREPLACE
    fromText e = fromTextFail $ "Unrecognised MetadataDirective: " <> e

instance Read MetadataDirective where
    readsPrec _ = fromTextRead

instance ToText MetadataDirective where
    toText MetadataDirectiveCOPY = "COPY"
    toText MetadataDirectiveREPLACE = "REPLACE"

instance Show MetadataDirective where
    show = toTextShow

instance ToQuery MetadataDirective where
    toQuery = toTextQuery

instance FromXML MetadataDirective where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML MetadataDirective where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | Specifies the region where the bucket will be created.
data LocationConstraint
    = LocationConstraint
    | LocationConstraintAPNORTHEAST1
    | LocationConstraintAPSOUTHEAST1
    | LocationConstraintAPSOUTHEAST2
    | LocationConstraintEU
    | LocationConstraintEUWEST1
    | LocationConstraintSAEAST1
    | LocationConstraintUSWEST1
    | LocationConstraintUSWEST2
      deriving (Eq, Ord, Generic)

instance Hashable LocationConstraint

instance FromText LocationConstraint where
    fromText "" = Right LocationConstraint
    fromText "ap-northeast-1" = Right LocationConstraintAPNORTHEAST1
    fromText "ap-southeast-1" = Right LocationConstraintAPSOUTHEAST1
    fromText "ap-southeast-2" = Right LocationConstraintAPSOUTHEAST2
    fromText "EU" = Right LocationConstraintEU
    fromText "eu-west-1" = Right LocationConstraintEUWEST1
    fromText "sa-east-1" = Right LocationConstraintSAEAST1
    fromText "us-west-1" = Right LocationConstraintUSWEST1
    fromText "us-west-2" = Right LocationConstraintUSWEST2
    fromText e = fromTextFail $ "Unrecognised LocationConstraint: " <> e

instance Read LocationConstraint where
    readsPrec _ = fromTextRead

instance ToText LocationConstraint where
    toText LocationConstraint = ""
    toText LocationConstraintAPNORTHEAST1 = "ap-northeast-1"
    toText LocationConstraintAPSOUTHEAST1 = "ap-southeast-1"
    toText LocationConstraintAPSOUTHEAST2 = "ap-southeast-2"
    toText LocationConstraintEU = "EU"
    toText LocationConstraintEUWEST1 = "eu-west-1"
    toText LocationConstraintSAEAST1 = "sa-east-1"
    toText LocationConstraintUSWEST1 = "us-west-1"
    toText LocationConstraintUSWEST2 = "us-west-2"

instance Show LocationConstraint where
    show = toTextShow

instance ToQuery LocationConstraint where
    toQuery = toTextQuery

instance FromXML LocationConstraint where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML LocationConstraint where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | Type of grantee.
data GranteeType
    = GranteeTypeAmazonCustomerByEmail
    | GranteeTypeCanonicalUser
    | GranteeTypeGroup
      deriving (Eq, Ord, Generic)

instance Hashable GranteeType

instance FromText GranteeType where
    fromText "AmazonCustomerByEmail" = Right GranteeTypeAmazonCustomerByEmail
    fromText "CanonicalUser" = Right GranteeTypeCanonicalUser
    fromText "Group" = Right GranteeTypeGroup
    fromText e = fromTextFail $ "Unrecognised GranteeType: " <> e

instance Read GranteeType where
    readsPrec _ = fromTextRead

instance ToText GranteeType where
    toText GranteeTypeAmazonCustomerByEmail = "AmazonCustomerByEmail"
    toText GranteeTypeCanonicalUser = "CanonicalUser"
    toText GranteeTypeGroup = "Group"

instance Show GranteeType where
    show = toTextShow

instance ToQuery GranteeType where
    toQuery = toTextQuery

instance FromXML GranteeType where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML GranteeType where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | Bucket event for which to send notifications.
data Event
    = EventReducedRedundancyLostObject
      deriving (Eq, Ord, Generic)

instance Hashable Event

instance FromText Event where
    fromText "s3:ReducedRedundancyLostObject" = Right EventReducedRedundancyLostObject
    fromText e = fromTextFail $ "Unrecognised Event: " <> e

instance Read Event where
    readsPrec _ = fromTextRead

instance ToText Event where
    toText EventReducedRedundancyLostObject = "s3:ReducedRedundancyLostObject"

instance Show Event where
    show = toTextShow

instance ToQuery Event where
    toQuery = toTextQuery

instance FromXML Event where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML Event where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | Requests Amazon S3 to encode the object keys in the response and specifies
-- the encoding method to use. An object key may contain any Unicode
-- character; however, XML 1.0 parser cannot parse some characters, such as
-- characters with an ASCII value from 0 to 10. For characters that are not
-- supported in XML 1.0, you can add this parameter to request that Amazon S3
-- encode the keys in the response.
data EncodingType
    = EncodingTypeUrl
      deriving (Eq, Ord, Generic)

instance Hashable EncodingType

instance FromText EncodingType where
    fromText "url" = Right EncodingTypeUrl
    fromText e = fromTextFail $ "Unrecognised EncodingType: " <> e

instance Read EncodingType where
    readsPrec _ = fromTextRead

instance ToText EncodingType where
    toText EncodingTypeUrl = "url"

instance Show EncodingType where
    show = toTextShow

instance ToQuery EncodingType where
    toQuery = toTextQuery

instance FromXML EncodingType where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML EncodingType where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The canned ACL to apply to the object.
data ACL
    = ACLAuthenticatedRead
    | ACLBucketOwnerFullControl
    | ACLBucketOwnerRead
    | ACLPrivate
    | ACLPublicRead
    | ACLPublicReadWrite
      deriving (Eq, Ord, Generic)

instance Hashable ACL

instance FromText ACL where
    fromText "authenticated-read" = Right ACLAuthenticatedRead
    fromText "bucket-owner-full-control" = Right ACLBucketOwnerFullControl
    fromText "bucket-owner-read" = Right ACLBucketOwnerRead
    fromText "private" = Right ACLPrivate
    fromText "public-read" = Right ACLPublicRead
    fromText "public-read-write" = Right ACLPublicReadWrite
    fromText e = fromTextFail $ "Unrecognised ACL: " <> e

instance Read ACL where
    readsPrec _ = fromTextRead

instance ToText ACL where
    toText ACLAuthenticatedRead = "authenticated-read"
    toText ACLBucketOwnerFullControl = "bucket-owner-full-control"
    toText ACLBucketOwnerRead = "bucket-owner-read"
    toText ACLPrivate = "private"
    toText ACLPublicRead = "public-read"
    toText ACLPublicReadWrite = "public-read-write"

instance Show ACL where
    show = toTextShow

instance ToQuery ACL where
    toQuery = toTextQuery

instance FromXML ACL where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML ACL where
    toXMLOptions = xmlOptions
    toXML        = toTextXML
