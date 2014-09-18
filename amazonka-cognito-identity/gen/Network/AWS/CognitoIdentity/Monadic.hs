{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CognitoIdentity.Monadic
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This module is provided for convenience. It offers an alternative to the
-- common idiom of supplying required fields to an operations's smart constructor,
-- using the operation's lenses to modify additional fields, and then sending
-- or paginating the request.
--
-- As an example: using "Network.AWS.CognitoIdentity" with the smart constructor and
-- basic lens syntax, before explicitly calling 'send':
--
-- @
-- import Control.Monad.Trans.AWS
-- import Network.AWS.CognitoIdentity
--
-- send $ (mkOperationName w x)
--      & onLensField1 .~ y
--      & onLensField2 .~ z
-- @
--
-- Versus using "Network.AWS.CognitoIdentity.Monadic" with the 'State' operator variants from
-- "Control.Lens.Setter" such as '.=' to modify any additional request
-- parameters before sending:
--
-- @
-- import Control.Applicative
-- import Network.AWS.CognitoIdentity.Monadic
--
-- operationName w x $ do
--     onLensField1 .= y
--     onLensField2 .= z
--
-- -- Or to void any additional parameters outside of those required using @return ()@:
-- operationName w x $ return ()
-- @
--
module Network.AWS.CognitoIdentity.Monadic
    (
    -- * CreateIdentityPool
    -- $CreateIdentityPool
      createIdentityPool
    , createIdentityPoolCatch

    -- * DeleteIdentityPool
    -- $DeleteIdentityPool
    , deleteIdentityPool
    , deleteIdentityPoolCatch

    -- * DescribeIdentityPool
    -- $DescribeIdentityPool
    , describeIdentityPool
    , describeIdentityPoolCatch

    -- * GetId
    -- $GetId
    , getId
    , getIdCatch

    -- * GetOpenIdToken
    -- $GetOpenIdToken
    , getOpenIdToken
    , getOpenIdTokenCatch

    -- * ListIdentities
    -- $ListIdentities
    , listIdentities
    , listIdentitiesCatch

    -- * ListIdentityPools
    -- $ListIdentityPools
    , listIdentityPools
    , listIdentityPoolsCatch

    -- * UnlinkIdentity
    -- $UnlinkIdentity
    , unlinkIdentity
    , unlinkIdentityCatch

    -- * UpdateIdentityPool
    -- $UpdateIdentityPool
    , updateIdentityPool
    , updateIdentityPoolCatch

    -- * Re-exported
    , module Network.AWS.CognitoIdentity

    , (.=)
    , (?=)
    , (<>=)
    , (%=)
    ) where

import Control.Monad.Trans.AWS as AWS
import Network.AWS.Prelude
import Network.AWS.CognitoIdentity


-- $CreateIdentityPool
-- Creates a new identity pool. The identity pool is a store of user identity
-- information that is specific to your AWS account. CreateIdentityPool The
-- following example shows a request and response for a CreateIdentityPool
-- operation. { "IdentityPoolName": "MyIdentityPool",
-- "IdentityPoolDescription": "My identity pool", "Unauthenticated": true,
-- "SupportedLoginProviders": { "graph.facebook.com": "Facebook_App_ID",
-- "accounts.google.com": "Google_App_ID", "www.amazon.com": "Amazon_App_ID" }
-- } { "IdentityPoolDescription": "My identity pool", "IdentityPoolId":
-- "us-east-1:1a234b56-7890-1cd2-3e45-f6g7hEXAMPLE", "IdentityPoolName":
-- "MyIdentityPool", "SupportedLoginProviders": { "www.amazon.com":
-- "Amazon_App_ID", "graph.facebook.com": "Facebook_App_ID",
-- "accounts.google.com": "Google_App_ID" }, "Unauthenticated": true }.
--
-- See: 'Network.AWS.CognitoIdentity.CreateIdentityPool'

createIdentityPool :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
    => Text -- ^ 'cipIdentityPoolName'
    -> Bool -- ^ 'cipAllowUnauthenticatedIdentities'
    -> State CreateIdentityPool a
    -> m CreateIdentityPoolResponse
createIdentityPool p1 p2 s =
    send $ (mkCreateIdentityPool p1 p2) &~ s

createIdentityPoolCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env m
                           )
    => Text -- ^ 'cipIdentityPoolName'
    -> Bool -- ^ 'cipAllowUnauthenticatedIdentities'
    -> State CreateIdentityPool a
    -> m (Either CognitoIdentityError CreateIdentityPoolResponse)
createIdentityPoolCatch p1 p2 s =
    sendCatch $ (mkCreateIdentityPool p1 p2) &~ s

-- $DeleteIdentityPool
-- Deletes a user pool. Once a pool is deleted, users will not be able to
-- authenticate with the pool. DeleteIdentityPool The following is an example
-- of a DeleteIdentityPool request. { "IdentityPoolId":
-- "us-east-1:1a234b56-7890-1cd2-3e45-f6g7hEXAMPLE" }.
--
-- See: 'Network.AWS.CognitoIdentity.DeleteIdentityPool'

deleteIdentityPool :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
    => Text -- ^ 'dipIdentityPoolId'
    -> m DeleteIdentityPoolResponse
deleteIdentityPool p1 =
    send (mkDeleteIdentityPool p1)

deleteIdentityPoolCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env m
                           )
    => Text -- ^ 'dipIdentityPoolId'
    -> m (Either CognitoIdentityError DeleteIdentityPoolResponse)
deleteIdentityPoolCatch p1 =
    sendCatch (mkDeleteIdentityPool p1)

-- $DescribeIdentityPool
-- Gets details about a particular identity pool, including the pool name, ID
-- description, creation date, and current number of users.
-- DescribeIdentityPool The following are an example request and response for
-- the DescribeIdentityPool operation. { "IdentityPoolId":
-- "us-east-1:af4311ca-835e-4b49-814c-2290EXAMPLE1" } {
-- "IdentityPoolDescription": "My identity pool", "IdentityPoolId":
-- "us-east-1:af4311ca-835e-4b49-814c-2290EXAMPLE1", "IdentityPoolName":
-- "MyIdentityPool", "SupportedLoginProviders": { "www.amazon.com":
-- "Amazon_App_ID", "graph.facebook.com": "Facebook_App_ID",
-- "accounts.google.com": "Google_App_ID" }, "Unauthenticated": true }.
--
-- See: 'Network.AWS.CognitoIdentity.DescribeIdentityPool'

describeIdentityPool :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env m
                        )
    => Text -- ^ 'dip1IdentityPoolId'
    -> m DescribeIdentityPoolResponse
describeIdentityPool p1 =
    send (mkDescribeIdentityPool p1)

describeIdentityPoolCatch :: ( MonadCatch m
                             , MonadResource m
                             , MonadReader Env m
                             )
    => Text -- ^ 'dip1IdentityPoolId'
    -> m (Either CognitoIdentityError DescribeIdentityPoolResponse)
describeIdentityPoolCatch p1 =
    sendCatch (mkDescribeIdentityPool p1)

-- $GetId
-- Generates (or retrieves) a Cognito ID. Supplying multiple logins will
-- create an implicit linked account. GetId The following example shows a
-- GetId request for an unauthenticated identity. { "AccountId":
-- "123456789012;", "IdentityPoolId":
-- "us-east-1:af4311ca-835e-4b49-814c-2290EXAMPLE1" } { "IdentityId":
-- "us-east-1:852d4250-9eec-4006-8f84-4e82EXAMPLE3" }.
--
-- See: 'Network.AWS.CognitoIdentity.GetId'

getId :: ( MonadCatch m
         , MonadResource m
         , MonadError AWS.Error m
         , MonadReader Env m
         )
    => Text -- ^ 'giAccountId'
    -> Text -- ^ 'giIdentityPoolId'
    -> State GetId a
    -> m GetIdResponse
getId p1 p2 s =
    send $ (mkGetId p1 p2) &~ s

getIdCatch :: ( MonadCatch m
              , MonadResource m
              , MonadReader Env m
              )
    => Text -- ^ 'giAccountId'
    -> Text -- ^ 'giIdentityPoolId'
    -> State GetId a
    -> m (Either CognitoIdentityError GetIdResponse)
getIdCatch p1 p2 s =
    sendCatch $ (mkGetId p1 p2) &~ s

-- $GetOpenIdToken
-- Gets an OpenID token, using a known Cognito ID. This known Cognito ID is
-- returned from GetId. You can optionally add additional logins for the
-- identity. Supplying multiple logins creates an implicit link.
-- GetOpenIdToken The following examples show a GetOpenIdToken request and
-- response, without the optional login values. { "IdentityId":
-- "us-east-1:852d4250-9eec-4006-8f84-4e82EXAMPLE3" } { "IdentityId":
-- "us-east-1:852d4250-9eec-4006-8f84-4e82EXAMPLE3", "Token":
-- "EXAmpLeTOkENUzUxMiIsInR5cCI6IkpXUyIsImtpZCI6InVzLWVhc3QtMTEifQ.eyJleHAiOjE0MDI2Njg0NTAsInN1YiI6InVzLWVhc3QtMTo5MjFhMzg0My0yZGQ2LTQ2YjgtYWIyZi1jNjc5NTUyZTM3MWUiLCJhdWQiOiJ1cy1lYXN0LTE6YWY0MzExY2EtODM1ZS00YjQ5LTgxNGMtMjI5MGQ4ZDU1YTZmIiwiaXNzIjoiaHR0cHM6Ly9jb2duaXRvLWlkZW50aXR5LXB1YmxpYy1pYWQtYmV0YS5hbWF6b24uY29tIiwiaWF0IjoxNDAyNjY3ODUwLCJhbXIiOlsidW5hdXRoZW50aWNhdGVkIl19.faWdRGsKxT8YqTBnAow1fNyXE57kjScKQ0lyFpFAUIl6VNVV-nQ_QD8XKHB_pAY2UNtxYFDoGhHrL3cqH_FLSfRLG-X3EaIrCsr0A6KIW7X69wsAxJQB-EvYru0UhDpcPaDyQUXTHFmRP9bzJMsSLi7nXm-OD4DCujX3vKwOhlSymbH9KbAG105t3_G_a8tsUbCV488nvlrA-7Omp0D18T1__XeZttldW1GODOK2OY2bK5-3eyodcqbVXaPTotxO5PTlYpzuMS1XfTejC8LJ2DocP_eBT7xhSr2qkro9Y6uCDph_-6ttYrXRaaLKZv3v1Lz6PGHrsPhJdK_bYRHhdg"
-- }.
--
-- See: 'Network.AWS.CognitoIdentity.GetOpenIdToken'

getOpenIdToken :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
    => Text -- ^ 'goitIdentityId'
    -> State GetOpenIdToken a
    -> m GetOpenIdTokenResponse
getOpenIdToken p1 s =
    send $ (mkGetOpenIdToken p1) &~ s

getOpenIdTokenCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env m
                       )
    => Text -- ^ 'goitIdentityId'
    -> State GetOpenIdToken a
    -> m (Either CognitoIdentityError GetOpenIdTokenResponse)
getOpenIdTokenCatch p1 s =
    sendCatch $ (mkGetOpenIdToken p1) &~ s

-- $ListIdentities
-- Lists the identities in a pool. ListIdentities The following are examples
-- of a request and a response for the ListIdentities action. {
-- "IdentityPoolId": "us-east-1:1a234b56-7890-1cd2-3e45-f6g7hEXAMPLE",
-- "MaxResults": 10 } { "Identities": [ { "IdentityId":
-- "us-east-1:2345a6b7-8cc3-4a60-8aeb-e11bEXAMPLE4" }, { "IdentityId":
-- "us-east-1:852d4250-9eec-4006-8f84-4e82EXAMPLE3" }, { "IdentityId":
-- "us-east-1:921a3843-2dd6-46b8-ab2f-c679EXAMPLE5" } ], "IdentityPoolId":
-- "us-east-1:1a234b56-7890-1cd2-3e45-f6g7hEXAMPLE" }.
--
-- See: 'Network.AWS.CognitoIdentity.ListIdentities'

listIdentities :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
    => Text -- ^ 'liIdentityPoolId'
    -> Integer -- ^ 'liMaxResults'
    -> State ListIdentities a
    -> m ListIdentitiesResponse
listIdentities p1 p2 s =
    send $ (mkListIdentities p1 p2) &~ s

listIdentitiesCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env m
                       )
    => Text -- ^ 'liIdentityPoolId'
    -> Integer -- ^ 'liMaxResults'
    -> State ListIdentities a
    -> m (Either CognitoIdentityError ListIdentitiesResponse)
listIdentitiesCatch p1 p2 s =
    sendCatch $ (mkListIdentities p1 p2) &~ s

-- $ListIdentityPools
-- Lists all of the Cognito identity pools registered for your account.
-- ListIdentityPools The following example shows a request and a response for
-- a ListIdentityPools operation. { "MaxResults": 10 } { "IdentityPools": [ {
-- "IdentityPoolId": "us-east-1:af4311ca-835e-4b49-814c-2290EXAMPLE1",
-- "IdentityPoolName": "MyPool" }, { "IdentityPoolId":
-- "us-east-1:f212b602-a526-4557-af13-8eedEXAMPLE2", "IdentityPoolName":
-- "MyPool2" } ] }.
--
-- See: 'Network.AWS.CognitoIdentity.ListIdentityPools'

listIdentityPools :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env m
                     )
    => Integer -- ^ 'lipMaxResults'
    -> State ListIdentityPools a
    -> m ListIdentityPoolsResponse
listIdentityPools p1 s =
    send $ (mkListIdentityPools p1) &~ s

listIdentityPoolsCatch :: ( MonadCatch m
                          , MonadResource m
                          , MonadReader Env m
                          )
    => Integer -- ^ 'lipMaxResults'
    -> State ListIdentityPools a
    -> m (Either CognitoIdentityError ListIdentityPoolsResponse)
listIdentityPoolsCatch p1 s =
    sendCatch $ (mkListIdentityPools p1) &~ s

-- $UnlinkIdentity
-- Unlinks a federated identity from an existing account. Unlinked logins will
-- be considered new identities next time they are seen. Removing the last
-- linked login will make this identity inaccessible.
--
-- See: 'Network.AWS.CognitoIdentity.UnlinkIdentity'

unlinkIdentity :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
    => Text -- ^ 'uiIdentityId'
    -> Map Text Text -- ^ 'uiLogins'
    -> [Text] -- ^ 'uiLoginsToRemove'
    -> m UnlinkIdentityResponse
unlinkIdentity p1 p2 p3 =
    send (mkUnlinkIdentity p1 p2 p3)

unlinkIdentityCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env m
                       )
    => Text -- ^ 'uiIdentityId'
    -> Map Text Text -- ^ 'uiLogins'
    -> [Text] -- ^ 'uiLoginsToRemove'
    -> m (Either CognitoIdentityError UnlinkIdentityResponse)
unlinkIdentityCatch p1 p2 p3 =
    sendCatch (mkUnlinkIdentity p1 p2 p3)

-- $UpdateIdentityPool
-- Updates a user pool. UpdateIdentityPool The following are a request and
-- response for the UpdateIdentityPool action. { "IdentityPoolId":
-- "us-east-1:1a234b56-7890-1cd2-3e45-f6g7hEXAMPLE", "IdentityPoolName":
-- "MyUpdatedPool", "IdentityPoolDescription": "An identity pool that needs
-- updating", "Unauthenticated": true, "SupportedLoginProviders": {
-- "www.amazon.com": "Amazon_App_ID", "graph.facebook.com": "Facebook_App_ID",
-- "accounts.google.com": "Google_App_ID" } } { "IdentityPoolDescription": "An
-- identity pool that needs updating", "IdentityPoolId":
-- "us-east-1:1a234b56-7890-1cd2-3e45-f6g7hEXAMPLE", "IdentityPoolName":
-- "MyUpdatedPool", "SupportedLoginProviders": { "www.amazon.com":
-- "Amazon_App_ID", "graph.facebook.com": "Facebook_App_ID",
-- "accounts.google.com": "Google_App_ID" }, "AllowUnauthenticatedIdentities":
-- true }.
--
-- See: 'Network.AWS.CognitoIdentity.UpdateIdentityPool'

updateIdentityPool :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
    => Text -- ^ 'uipIdentityPoolId'
    -> Text -- ^ 'uipIdentityPoolName'
    -> Bool -- ^ 'uipAllowUnauthenticatedIdentities'
    -> State UpdateIdentityPool a
    -> m UpdateIdentityPoolResponse
updateIdentityPool p1 p2 p3 s =
    send $ (mkUpdateIdentityPool p1 p2 p3) &~ s

updateIdentityPoolCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env m
                           )
    => Text -- ^ 'uipIdentityPoolId'
    -> Text -- ^ 'uipIdentityPoolName'
    -> Bool -- ^ 'uipAllowUnauthenticatedIdentities'
    -> State UpdateIdentityPool a
    -> m (Either CognitoIdentityError UpdateIdentityPoolResponse)
updateIdentityPoolCatch p1 p2 p3 s =
    sendCatch $ (mkUpdateIdentityPool p1 p2 p3) &~ s
