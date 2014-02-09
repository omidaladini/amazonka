-- Module      : Network.AWS.IAM
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.IAM
    (
    -- * Operations
    -- ** ListInstanceProfilesForRole
      module Network.AWS.IAM.ListInstanceProfilesForRole
    -- ** CreateAccessKey
    , module Network.AWS.IAM.CreateAccessKey
    -- ** CreateVirtualMFADevice
    , module Network.AWS.IAM.CreateVirtualMFADevice
    -- ** DeleteAccountPasswordPolicy
    , module Network.AWS.IAM.DeleteAccountPasswordPolicy
    -- ** UpdateAccountPasswordPolicy
    , module Network.AWS.IAM.UpdateAccountPasswordPolicy
    -- ** GetUserPolicy
    , module Network.AWS.IAM.GetUserPolicy
    -- ** GetRole
    , module Network.AWS.IAM.GetRole
    -- ** DeactivateMFADevice
    , module Network.AWS.IAM.DeactivateMFADevice
    -- ** DeleteVirtualMFADevice
    , module Network.AWS.IAM.DeleteVirtualMFADevice
    -- ** ListRoles
    , module Network.AWS.IAM.ListRoles
    -- ** ListUserPolicies
    , module Network.AWS.IAM.ListUserPolicies
    -- ** DeleteRole
    , module Network.AWS.IAM.DeleteRole
    -- ** ListUsers
    , module Network.AWS.IAM.ListUsers
    -- ** PutUserPolicy
    , module Network.AWS.IAM.PutUserPolicy
    -- ** DeleteUserPolicy
    , module Network.AWS.IAM.DeleteUserPolicy
    -- ** CreateRole
    , module Network.AWS.IAM.CreateRole
    -- ** GetAccountSummary
    , module Network.AWS.IAM.GetAccountSummary
    -- ** ListGroupPolicies
    , module Network.AWS.IAM.ListGroupPolicies
    -- ** DeleteInstanceProfile
    , module Network.AWS.IAM.DeleteInstanceProfile
    -- ** RemoveRoleFromInstanceProfile
    , module Network.AWS.IAM.RemoveRoleFromInstanceProfile
    -- ** CreateInstanceProfile
    , module Network.AWS.IAM.CreateInstanceProfile
    -- ** CreateSAMLProvider
    , module Network.AWS.IAM.CreateSAMLProvider
    -- ** DeleteAccountAlias
    , module Network.AWS.IAM.DeleteAccountAlias
    -- ** RemoveUserFromGroup
    , module Network.AWS.IAM.RemoveUserFromGroup
    -- ** DeleteGroupPolicy
    , module Network.AWS.IAM.DeleteGroupPolicy
    -- ** PutGroupPolicy
    , module Network.AWS.IAM.PutGroupPolicy
    -- ** GetLoginProfile
    , module Network.AWS.IAM.GetLoginProfile
    -- ** GetGroupPolicy
    , module Network.AWS.IAM.GetGroupPolicy
    -- ** ChangePassword
    , module Network.AWS.IAM.ChangePassword
    -- ** ListServerCertificates
    , module Network.AWS.IAM.ListServerCertificates
    -- ** UpdateAssumeRolePolicy
    , module Network.AWS.IAM.UpdateAssumeRolePolicy
    -- ** GetInstanceProfile
    , module Network.AWS.IAM.GetInstanceProfile
    -- ** CreateLoginProfile
    , module Network.AWS.IAM.CreateLoginProfile
    -- ** GetSAMLProvider
    , module Network.AWS.IAM.GetSAMLProvider
    -- ** AddRoleToInstanceProfile
    , module Network.AWS.IAM.AddRoleToInstanceProfile
    -- ** ListGroupsForUser
    , module Network.AWS.IAM.ListGroupsForUser
    -- ** AddUserToGroup
    , module Network.AWS.IAM.AddUserToGroup
    -- ** GetUser
    , module Network.AWS.IAM.GetUser
    -- ** ListSigningCertificates
    , module Network.AWS.IAM.ListSigningCertificates
    -- ** DeleteSigningCertificate
    , module Network.AWS.IAM.DeleteSigningCertificate
    -- ** UpdateSigningCertificate
    , module Network.AWS.IAM.UpdateSigningCertificate
    -- ** ListVirtualMFADevices
    , module Network.AWS.IAM.ListVirtualMFADevices
    -- ** ResyncMFADevice
    , module Network.AWS.IAM.ResyncMFADevice
    -- ** DeleteAccessKey
    , module Network.AWS.IAM.DeleteAccessKey
    -- ** UpdateAccessKey
    , module Network.AWS.IAM.UpdateAccessKey
    -- ** ListAccessKeys
    , module Network.AWS.IAM.ListAccessKeys
    -- ** GetRolePolicy
    , module Network.AWS.IAM.GetRolePolicy
    -- ** CreateUser
    , module Network.AWS.IAM.CreateUser
    -- ** PutRolePolicy
    , module Network.AWS.IAM.PutRolePolicy
    -- ** UploadSigningCertificate
    , module Network.AWS.IAM.UploadSigningCertificate
    -- ** DeleteRolePolicy
    , module Network.AWS.IAM.DeleteRolePolicy
    -- ** GetAccountPasswordPolicy
    , module Network.AWS.IAM.GetAccountPasswordPolicy
    -- ** UpdateUser
    , module Network.AWS.IAM.UpdateUser
    -- ** DeleteUser
    , module Network.AWS.IAM.DeleteUser
    -- ** ListRolePolicies
    , module Network.AWS.IAM.ListRolePolicies
    -- ** CreateAccountAlias
    , module Network.AWS.IAM.CreateAccountAlias
    -- ** ListInstanceProfiles
    , module Network.AWS.IAM.ListInstanceProfiles
    -- ** EnableMFADevice
    , module Network.AWS.IAM.EnableMFADevice
    -- ** ListAccountAliases
    , module Network.AWS.IAM.ListAccountAliases
    -- ** DeleteSAMLProvider
    , module Network.AWS.IAM.DeleteSAMLProvider
    -- ** UpdateSAMLProvider
    , module Network.AWS.IAM.UpdateSAMLProvider
    -- ** CreateGroup
    , module Network.AWS.IAM.CreateGroup
    -- ** ListMFADevices
    , module Network.AWS.IAM.ListMFADevices
    -- ** UploadServerCertificate
    , module Network.AWS.IAM.UploadServerCertificate
    -- ** ListSAMLProviders
    , module Network.AWS.IAM.ListSAMLProviders
    -- ** GetServerCertificate
    , module Network.AWS.IAM.GetServerCertificate
    -- ** DeleteGroup
    , module Network.AWS.IAM.DeleteGroup
    -- ** UpdateGroup
    , module Network.AWS.IAM.UpdateGroup
    -- ** ListGroups
    , module Network.AWS.IAM.ListGroups
    -- ** UpdateLoginProfile
    , module Network.AWS.IAM.UpdateLoginProfile
    -- ** DeleteLoginProfile
    , module Network.AWS.IAM.DeleteLoginProfile
    -- ** GetGroup
    , module Network.AWS.IAM.GetGroup
    -- ** DeleteServerCertificate
    , module Network.AWS.IAM.DeleteServerCertificate
    -- ** UpdateServerCertificate
    , module Network.AWS.IAM.UpdateServerCertificate

    -- * Types
    -- ** VirtualMFADevice
    , VirtualMFADevice (..)
    -- ** User
    , User (..)
    -- ** SigningCertificate
    , SigningCertificate (..)
    -- ** ServerCertificateMetadata
    , ServerCertificateMetadata (..)
    -- ** ServerCertificate
    , ServerCertificate (..)
    -- ** SAMLProviderListEntry
    , SAMLProviderListEntry (..)
    -- ** Role
    , Role (..)
    -- ** PasswordPolicy
    , PasswordPolicy (..)
    -- ** MFADevice
    , MFADevice (..)
    -- ** LoginProfile
    , LoginProfile (..)
    -- ** InstanceProfile
    , InstanceProfile (..)
    -- ** Group
    , Group (..)
    -- ** AccessKeyMetadata
    , AccessKeyMetadata (..)
    -- ** AccessKey
    , AccessKey (..)
    -- ** SummaryKeyType
    , SummaryKeyType (..)
    -- ** StatusType
    , StatusType (..)
    -- ** AssignmentStatusType
    , AssignmentStatusType (..)

    -- * Errors
    , IAMError (..)
    ) where

import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

import Network.AWS.IAM.ListInstanceProfilesForRole
import Network.AWS.IAM.CreateAccessKey
import Network.AWS.IAM.CreateVirtualMFADevice
import Network.AWS.IAM.DeleteAccountPasswordPolicy
import Network.AWS.IAM.UpdateAccountPasswordPolicy
import Network.AWS.IAM.GetUserPolicy
import Network.AWS.IAM.GetRole
import Network.AWS.IAM.DeactivateMFADevice
import Network.AWS.IAM.DeleteVirtualMFADevice
import Network.AWS.IAM.ListRoles
import Network.AWS.IAM.ListUserPolicies
import Network.AWS.IAM.DeleteRole
import Network.AWS.IAM.ListUsers
import Network.AWS.IAM.PutUserPolicy
import Network.AWS.IAM.DeleteUserPolicy
import Network.AWS.IAM.CreateRole
import Network.AWS.IAM.GetAccountSummary
import Network.AWS.IAM.ListGroupPolicies
import Network.AWS.IAM.DeleteInstanceProfile
import Network.AWS.IAM.RemoveRoleFromInstanceProfile
import Network.AWS.IAM.CreateInstanceProfile
import Network.AWS.IAM.CreateSAMLProvider
import Network.AWS.IAM.DeleteAccountAlias
import Network.AWS.IAM.RemoveUserFromGroup
import Network.AWS.IAM.DeleteGroupPolicy
import Network.AWS.IAM.PutGroupPolicy
import Network.AWS.IAM.GetLoginProfile
import Network.AWS.IAM.GetGroupPolicy
import Network.AWS.IAM.ChangePassword
import Network.AWS.IAM.ListServerCertificates
import Network.AWS.IAM.UpdateAssumeRolePolicy
import Network.AWS.IAM.GetInstanceProfile
import Network.AWS.IAM.CreateLoginProfile
import Network.AWS.IAM.GetSAMLProvider
import Network.AWS.IAM.AddRoleToInstanceProfile
import Network.AWS.IAM.ListGroupsForUser
import Network.AWS.IAM.AddUserToGroup
import Network.AWS.IAM.GetUser
import Network.AWS.IAM.ListSigningCertificates
import Network.AWS.IAM.DeleteSigningCertificate
import Network.AWS.IAM.UpdateSigningCertificate
import Network.AWS.IAM.ListVirtualMFADevices
import Network.AWS.IAM.ResyncMFADevice
import Network.AWS.IAM.DeleteAccessKey
import Network.AWS.IAM.UpdateAccessKey
import Network.AWS.IAM.ListAccessKeys
import Network.AWS.IAM.GetRolePolicy
import Network.AWS.IAM.CreateUser
import Network.AWS.IAM.PutRolePolicy
import Network.AWS.IAM.UploadSigningCertificate
import Network.AWS.IAM.DeleteRolePolicy
import Network.AWS.IAM.GetAccountPasswordPolicy
import Network.AWS.IAM.UpdateUser
import Network.AWS.IAM.DeleteUser
import Network.AWS.IAM.ListRolePolicies
import Network.AWS.IAM.CreateAccountAlias
import Network.AWS.IAM.ListInstanceProfiles
import Network.AWS.IAM.EnableMFADevice
import Network.AWS.IAM.ListAccountAliases
import Network.AWS.IAM.DeleteSAMLProvider
import Network.AWS.IAM.UpdateSAMLProvider
import Network.AWS.IAM.CreateGroup
import Network.AWS.IAM.ListMFADevices
import Network.AWS.IAM.UploadServerCertificate
import Network.AWS.IAM.ListSAMLProviders
import Network.AWS.IAM.GetServerCertificate
import Network.AWS.IAM.DeleteGroup
import Network.AWS.IAM.UpdateGroup
import Network.AWS.IAM.ListGroups
import Network.AWS.IAM.UpdateLoginProfile
import Network.AWS.IAM.DeleteLoginProfile
import Network.AWS.IAM.GetGroup
import Network.AWS.IAM.DeleteServerCertificate
import Network.AWS.IAM.UpdateServerCertificate
