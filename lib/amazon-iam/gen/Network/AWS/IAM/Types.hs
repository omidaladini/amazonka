{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.IAM.Types where

import Data.ByteString      (ByteString)
import Data.Hashable
import Data.Monoid
import Data.List.NonEmpty
import Data.Text            (Text)
import Data.HashMap.Strict  (HashMap)
import Data.Time
import GHC.Generics         (Generic)
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.IAM.Service

-- | A newly created virtual MFA device.
data VirtualMFADevice = VirtualMFADevice
    { vmfadBase32StringSeed :: Maybe Blob
      -- ^ The Base32 seed defined as specified in RFC3548. The Base32StringSeed is
      -- Base64-encoded.
    , vmfadEnableDate :: Maybe UTCTime
    , vmfadQRCodePNG :: Maybe Blob
      -- ^ A QR code PNG image that encodes
      -- otpauth://totp/$virtualMFADeviceName@$AccountName? secret=$Base32String
      -- where $virtualMFADeviceName is one of the create call arguments,
      -- AccountName is the user name if set (accountId otherwise), and Base32String
      -- is the seed in Base32 format. The Base32String is Base64-encoded.
    , vmfadSerialNumber :: !Text
      -- ^ The serial number associated with VirtualMFADevice.
    , vmfadUser :: Maybe User
      -- ^ The User data type contains information about a user. This data type is
      -- used as a response element in the following actions: CreateUser GetUser
      -- ListUsers.
    } deriving (Eq, Show, Generic)

instance ToQuery VirtualMFADevice

instance FromXML VirtualMFADevice where
    fromXMLOptions = xmlOptions

instance ToXML VirtualMFADevice where
    toXMLOptions = xmlOptions

-- | The User data type contains information about a user. This data type is
-- used as a response element in the following actions: CreateUser GetUser
-- ListUsers.
data User = User
    { uArn :: !Text
      -- ^ The Amazon Resource Name (ARN) specifying the user. For more information
      -- about ARNs and how to use them in policies, see Identifiers for IAM
      -- Entities in Using AWS Identity and Access Management.
    , uCreateDate :: !UTCTime
      -- ^ The date when the user was created.
    , uPath :: !Text
      -- ^ Path to the user. For more information about paths, see Identifiers for IAM
      -- Entities in Using AWS Identity and Access Management.
    , uUserId :: !Text
      -- ^ The stable and unique string identifying the user. For more information
      -- about IDs, see Identifiers for IAM Entities in Using AWS Identity and
      -- Access Management.
    , uUserName :: !Text
      -- ^ The name identifying the user.
    } deriving (Eq, Show, Generic)

instance ToQuery User

instance FromXML User where
    fromXMLOptions = xmlOptions

instance ToXML User where
    toXMLOptions = xmlOptions

-- | The SigningCertificate data type contains information about an X.509
-- signing certificate. This data type is used as a response element in the
-- actions UploadSigningCertificate and ListSigningCertificates.
data SigningCertificate = SigningCertificate
    { sdCertificateBody :: !Text
      -- ^ The contents of the signing certificate.
    , sdCertificateId :: !Text
      -- ^ The ID for the signing certificate.
    , sdStatus :: !StatusType
      -- ^ The status of the signing certificate. Active means the key is valid for
      -- API calls, while Inactive means it is not.
    , sdUploadDate :: Maybe UTCTime
      -- ^ The date when the signing certificate was uploaded.
    , sdUserName :: !Text
      -- ^ Name of the user the signing certificate is associated with.
    } deriving (Eq, Show, Generic)

instance ToQuery SigningCertificate

instance FromXML SigningCertificate where
    fromXMLOptions = xmlOptions

instance ToXML SigningCertificate where
    toXMLOptions = xmlOptions

-- | ServerCertificateMetadata contains information about a server certificate
-- without its certificate body, certificate chain, and private key. This data
-- type is used as a response element in the action UploadServerCertificate
-- and ListServerCertificates.
data ServerCertificateMetadata = ServerCertificateMetadata
    { scmArn :: !Text
      -- ^ The Amazon Resource Name (ARN) specifying the server certificate. For more
      -- information about ARNs and how to use them in policies, see Identifiers for
      -- IAM Entities in Using AWS Identity and Access Management.
    , scmPath :: !Text
      -- ^ Path to the server certificate. For more information about paths, see
      -- Identifiers for IAM Entities in Using AWS Identity and Access Management.
    , scmServerCertificateId :: !Text
      -- ^ The stable and unique string identifying the server certificate. For more
      -- information about IDs, see Identifiers for IAM Entities in Using AWS
      -- Identity and Access Management.
    , scmServerCertificateName :: !Text
      -- ^ The name that identifies the server certificate.
    , scmUploadDate :: Maybe UTCTime
      -- ^ The date when the server certificate was uploaded.
    } deriving (Eq, Show, Generic)

instance ToQuery ServerCertificateMetadata

instance FromXML ServerCertificateMetadata where
    fromXMLOptions = xmlOptions

instance ToXML ServerCertificateMetadata where
    toXMLOptions = xmlOptions

-- | Information about the server certificate.
data ServerCertificate = ServerCertificate
    { scCertificateBody :: !Text
      -- ^ The contents of the public key certificate.
    , scCertificateChain :: Maybe Text
      -- ^ The contents of the public key certificate chain.
    , scServerCertificateMetadata :: ServerCertificateMetadata
      -- ^ The meta information of the server certificate, such as its name, path, ID,
      -- and ARN.
    } deriving (Eq, Show, Generic)

instance ToQuery ServerCertificate

instance FromXML ServerCertificate where
    fromXMLOptions = xmlOptions

instance ToXML ServerCertificate where
    toXMLOptions = xmlOptions

-- | The list of SAML providers for this account.
data SAMLProviderListEntry = SAMLProviderListEntry
    { samlpleArn :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the SAML provider.
    , samlpleCreateDate :: Maybe UTCTime
      -- ^ The date and time when the SAML provider was created.
    , samlpleValidUntil :: Maybe UTCTime
      -- ^ The expiration date and time for the SAML provider.
    } deriving (Eq, Show, Generic)

instance ToQuery SAMLProviderListEntry

instance FromXML SAMLProviderListEntry where
    fromXMLOptions = xmlOptions

instance ToXML SAMLProviderListEntry where
    toXMLOptions = xmlOptions

-- | The Role data type contains information about a role. This data type is
-- used as a response element in the following actions: CreateRole GetRole
-- ListRoles.
data Role = Role
    { rArn :: !Text
      -- ^ The Amazon Resource Name (ARN) specifying the role. For more information
      -- about ARNs and how to use them in policies, see Identifiers for IAM
      -- Entities in Using AWS Identity and Access Management.
    , rAssumeRolePolicyDocument :: Maybe Text
      -- ^ The policy that grants an entity permission to assume the role. The
      -- returned policy is URL-encoded according to RFC 3986. For more information
      -- about RFC 3986, go to http://www.faqs.org/rfcs/rfc3986.html.
    , rCreateDate :: !UTCTime
      -- ^ The date when the role was created.
    , rPath :: !Text
      -- ^ Path to the role. For more information about paths, see Identifiers for IAM
      -- Entities in Using AWS Identity and Access Management.
    , rRoleId :: !Text
      -- ^ The stable and unique string identifying the role. For more information
      -- about IDs, see Identifiers for IAM Entities in Using AWS Identity and
      -- Access Management.
    , rRoleName :: !Text
      -- ^ The name identifying the role.
    } deriving (Eq, Show, Generic)

instance ToQuery Role

instance FromXML Role where
    fromXMLOptions = xmlOptions

instance ToXML Role where
    toXMLOptions = xmlOptions

-- | The PasswordPolicy data type contains information about the account
-- password policy. This data type is used as a response element in the action
-- GetAccountPasswordPolicy.
data PasswordPolicy = PasswordPolicy
    { ppAllowUsersToChangePassword :: Maybe Bool
      -- ^ Specifies whether to allow IAM users to change their own password.
    , ppExpirePasswords :: Maybe Bool
    , ppMaxPasswordAge :: Maybe Int
    , ppMinimumPasswordLength :: Maybe Int
      -- ^ Minimum length to require for IAM user passwords.
    , ppRequireLowercaseCharacters :: Maybe Bool
      -- ^ Specifies whether to require lowercase characters for IAM user passwords.
    , ppRequireNumbers :: Maybe Bool
      -- ^ Specifies whether to require numbers for IAM user passwords.
    , ppRequireSymbols :: Maybe Bool
      -- ^ Specifies whether to require symbols for IAM user passwords.
    , ppRequireUppercaseCharacters :: Maybe Bool
      -- ^ Specifies whether to require uppercase characters for IAM user passwords.
    } deriving (Eq, Show, Generic)

instance ToQuery PasswordPolicy

instance FromXML PasswordPolicy where
    fromXMLOptions = xmlOptions

instance ToXML PasswordPolicy where
    toXMLOptions = xmlOptions

-- | The MFADevice data type contains information about an MFA device. This data
-- type is used as a response element in the action ListMFADevices.
data MFADevice = MFADevice
    { mfadEnableDate :: !UTCTime
      -- ^ The date when the MFA device was enabled for the user.
    , mfadSerialNumber :: !Text
      -- ^ The serial number that uniquely identifies the MFA device. For virtual MFA
      -- devices, the serial number is the device ARN.
    , mfadUserName :: !Text
      -- ^ The user with whom the MFA device is associated.
    } deriving (Eq, Show, Generic)

instance ToQuery MFADevice

instance FromXML MFADevice where
    fromXMLOptions = xmlOptions

instance ToXML MFADevice where
    toXMLOptions = xmlOptions

-- | User name and password create date for the user.
data LoginProfile = LoginProfile
    { lpCreateDate :: !UTCTime
      -- ^ The date when the password for the user was created.
    , lpUserName :: !Text
      -- ^ The name of the user, which can be used for signing into the AWS Management
      -- Console.
    } deriving (Eq, Show, Generic)

instance ToQuery LoginProfile

instance FromXML LoginProfile where
    fromXMLOptions = xmlOptions

instance ToXML LoginProfile where
    toXMLOptions = xmlOptions

-- | The InstanceProfile data type contains information about an instance
-- profile. This data type is used as a response element in the following
-- actions: CreateInstanceProfile GetInstanceProfile ListInstanceProfiles
-- ListInstanceProfilesForRole.
data InstanceProfile = InstanceProfile
    { ipArn :: !Text
      -- ^ The Amazon Resource Name (ARN) specifying the instance profile. For more
      -- information about ARNs and how to use them in policies, see Identifiers for
      -- IAM Entities in Using AWS Identity and Access Management.
    , ipCreateDate :: !UTCTime
      -- ^ The date when the instance profile was created.
    , ipInstanceProfileId :: !Text
      -- ^ The stable and unique string identifying the instance profile. For more
      -- information about IDs, see Identifiers for IAM Entities in Using AWS
      -- Identity and Access Management.
    , ipInstanceProfileName :: !Text
      -- ^ The name identifying the instance profile.
    , ipPath :: !Text
      -- ^ Path to the instance profile. For more information about paths, see
      -- Identifiers for IAM Entities in Using AWS Identity and Access Management.
    , ipRoles :: [Role]
      -- ^ The role associated with the instance profile.
    } deriving (Eq, Show, Generic)

instance ToQuery InstanceProfile

instance FromXML InstanceProfile where
    fromXMLOptions = xmlOptions

instance ToXML InstanceProfile where
    toXMLOptions = xmlOptions

-- | The Group data type contains information about a group. This data type is
-- used as a response element in the following actions: CreateGroup GetGroup
-- ListGroups.
data Group = Group
    { gArn :: !Text
      -- ^ The Amazon Resource Name (ARN) specifying the group. For more information
      -- about ARNs and how to use them in policies, see Identifiers for IAM
      -- Entities in Using AWS Identity and Access Management.
    , gCreateDate :: !UTCTime
      -- ^ The date when the group was created.
    , gGroupId :: !Text
      -- ^ The stable and unique string identifying the group. For more information
      -- about IDs, see Identifiers for IAM Entities in Using AWS Identity and
      -- Access Management.
    , gGroupName :: !Text
      -- ^ The name that identifies the group.
    , gPath :: !Text
      -- ^ Path to the group. For more information about paths, see Identifiers for
      -- IAM Entities in Using AWS Identity and Access Management.
    } deriving (Eq, Show, Generic)

instance ToQuery Group

instance FromXML Group where
    fromXMLOptions = xmlOptions

instance ToXML Group where
    toXMLOptions = xmlOptions

-- | The AccessKey data type contains information about an AWS access key,
-- without its secret key. This data type is used as a response element in the
-- action ListAccessKeys.
data AccessKeyMetadata = AccessKeyMetadata
    { akmAccessKeyId :: Maybe Text
      -- ^ The ID for this access key.
    , akmCreateDate :: Maybe UTCTime
      -- ^ The date when the access key was created.
    , akmStatus :: Maybe StatusType
      -- ^ The status of the access key. Active means the key is valid for API calls,
      -- while Inactive means it is not.
    , akmUserName :: Maybe Text
      -- ^ Name of the user the key is associated with.
    } deriving (Eq, Show, Generic)

instance ToQuery AccessKeyMetadata

instance FromXML AccessKeyMetadata where
    fromXMLOptions = xmlOptions

instance ToXML AccessKeyMetadata where
    toXMLOptions = xmlOptions

-- | Information about the access key.
data AccessKey = AccessKey
    { akAccessKeyId :: !Text
      -- ^ The ID for this access key.
    , akCreateDate :: Maybe UTCTime
      -- ^ The date when the access key was created.
    , akSecretAccessKey :: !Text
      -- ^ The secret key used to sign requests.
    , akStatus :: !StatusType
      -- ^ The status of the access key. Active means the key is valid for API calls,
      -- while Inactive means it is not.
    , akUserName :: !Text
      -- ^ Name of the user the key is associated with.
    } deriving (Eq, Show, Generic)

instance ToQuery AccessKey

instance FromXML AccessKey where
    fromXMLOptions = xmlOptions

instance ToXML AccessKey where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for SummaryKeyType
data SummaryKeyType
    = SummaryKeyTypeAccessKeysPerUserQuota
    | SummaryKeyTypeAccountMFAEnabled
    | SummaryKeyTypeGroupPolicySizeQuota
    | SummaryKeyTypeGroups
    | SummaryKeyTypeGroupsPerUserQuota
    | SummaryKeyTypeGroupsQuota
    | SummaryKeyTypeMFADevices
    | SummaryKeyTypeMFADevicesInUse
    | SummaryKeyTypeServerCertificates
    | SummaryKeyTypeServerCertificatesQuota
    | SummaryKeyTypeSigningCertificatesPerUserQuota
    | SummaryKeyTypeUserPolicySizeQuota
    | SummaryKeyTypeUsers
    | SummaryKeyTypeUsersQuota
      deriving (Eq, Ord, Generic)

instance Hashable SummaryKeyType

instance FromText SummaryKeyType where
    fromText "AccessKeysPerUserQuota" = Right SummaryKeyTypeAccessKeysPerUserQuota
    fromText "AccountMFAEnabled" = Right SummaryKeyTypeAccountMFAEnabled
    fromText "GroupPolicySizeQuota" = Right SummaryKeyTypeGroupPolicySizeQuota
    fromText "Groups" = Right SummaryKeyTypeGroups
    fromText "GroupsPerUserQuota" = Right SummaryKeyTypeGroupsPerUserQuota
    fromText "GroupsQuota" = Right SummaryKeyTypeGroupsQuota
    fromText "MFADevices" = Right SummaryKeyTypeMFADevices
    fromText "MFADevicesInUse" = Right SummaryKeyTypeMFADevicesInUse
    fromText "ServerCertificates" = Right SummaryKeyTypeServerCertificates
    fromText "ServerCertificatesQuota" = Right SummaryKeyTypeServerCertificatesQuota
    fromText "SigningCertificatesPerUserQuota" = Right SummaryKeyTypeSigningCertificatesPerUserQuota
    fromText "UserPolicySizeQuota" = Right SummaryKeyTypeUserPolicySizeQuota
    fromText "Users" = Right SummaryKeyTypeUsers
    fromText "UsersQuota" = Right SummaryKeyTypeUsersQuota
    fromText e = fromTextFail $ "Unrecognised SummaryKeyType: " <> e

instance Read SummaryKeyType where
    readsPrec _ = fromTextRead

instance ToText SummaryKeyType where
    toText SummaryKeyTypeAccessKeysPerUserQuota = "AccessKeysPerUserQuota"
    toText SummaryKeyTypeAccountMFAEnabled = "AccountMFAEnabled"
    toText SummaryKeyTypeGroupPolicySizeQuota = "GroupPolicySizeQuota"
    toText SummaryKeyTypeGroups = "Groups"
    toText SummaryKeyTypeGroupsPerUserQuota = "GroupsPerUserQuota"
    toText SummaryKeyTypeGroupsQuota = "GroupsQuota"
    toText SummaryKeyTypeMFADevices = "MFADevices"
    toText SummaryKeyTypeMFADevicesInUse = "MFADevicesInUse"
    toText SummaryKeyTypeServerCertificates = "ServerCertificates"
    toText SummaryKeyTypeServerCertificatesQuota = "ServerCertificatesQuota"
    toText SummaryKeyTypeSigningCertificatesPerUserQuota = "SigningCertificatesPerUserQuota"
    toText SummaryKeyTypeUserPolicySizeQuota = "UserPolicySizeQuota"
    toText SummaryKeyTypeUsers = "Users"
    toText SummaryKeyTypeUsersQuota = "UsersQuota"

instance Show SummaryKeyType where
    show = toTextShow

instance ToQuery SummaryKeyType where
    toQuery = toTextQuery

instance FromXML SummaryKeyType where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML SummaryKeyType where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The status of the access key. Active means the key is valid for API calls,
-- while Inactive means it is not.
data StatusType
    = StatusTypeActive
    | StatusTypeInactive
      deriving (Eq, Ord, Generic)

instance Hashable StatusType

instance FromText StatusType where
    fromText "Active" = Right StatusTypeActive
    fromText "Inactive" = Right StatusTypeInactive
    fromText e = fromTextFail $ "Unrecognised StatusType: " <> e

instance Read StatusType where
    readsPrec _ = fromTextRead

instance ToText StatusType where
    toText StatusTypeActive = "Active"
    toText StatusTypeInactive = "Inactive"

instance Show StatusType where
    show = toTextShow

instance ToQuery StatusType where
    toQuery = toTextQuery

instance FromXML StatusType where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML StatusType where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The status (unassigned or assigned) of the devices to list. If you do not
-- specify an AssignmentStatus, the action defaults to Any which lists both
-- assigned and unassigned virtual MFA devices.
data AssignmentStatusType
    = AssignmentStatusTypeAny
    | AssignmentStatusTypeAssigned
    | AssignmentStatusTypeUnassigned
      deriving (Eq, Ord, Generic)

instance Hashable AssignmentStatusType

instance FromText AssignmentStatusType where
    fromText "Any" = Right AssignmentStatusTypeAny
    fromText "Assigned" = Right AssignmentStatusTypeAssigned
    fromText "Unassigned" = Right AssignmentStatusTypeUnassigned
    fromText e = fromTextFail $ "Unrecognised AssignmentStatusType: " <> e

instance Read AssignmentStatusType where
    readsPrec _ = fromTextRead

instance ToText AssignmentStatusType where
    toText AssignmentStatusTypeAny = "Any"
    toText AssignmentStatusTypeAssigned = "Assigned"
    toText AssignmentStatusTypeUnassigned = "Unassigned"

instance Show AssignmentStatusType where
    show = toTextShow

instance ToQuery AssignmentStatusType where
    toQuery = toTextQuery

instance FromXML AssignmentStatusType where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML AssignmentStatusType where
    toXMLOptions = xmlOptions
    toXML        = toTextXML
