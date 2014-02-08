{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS.Internal.Signing
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal.Signing
    ( sign
    , v2
    , v3
    , v4
    , s3
    ) where

import           Control.Applicative
import           Control.Monad.IO.Class
import qualified Crypto.Hash.SHA1                  as SHA1
import qualified Crypto.Hash.SHA256                as SHA256
import qualified Crypto.MAC.HMAC                   as HMAC
import           Data.ByteString                   (ByteString)
import qualified Data.ByteString.Base16            as Base16
import qualified Data.ByteString.Base64            as Base64
import qualified Data.ByteString.Char8             as BS
import           Data.CaseInsensitive              (CI)
import qualified Data.CaseInsensitive              as Case
import           Data.Default
import           Data.Function                     (on)
import           Data.List                         (groupBy, nub, sort)
import           Data.Maybe
import           Data.Monoid
import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import qualified Data.Text.Encoding                as Text
import           Data.Time                         (getCurrentTime)
import           Network.AWS.Headers
import           Network.AWS.Internal.Types
import           Network.AWS.Internal.Types.Common
import           Network.AWS.Text
import           Network.AWS.Time
import           Network.HTTP.Conduit
import           Network.HTTP.Types                (Header, StdMethod, Query)
import qualified Network.HTTP.Types                as HTTP

sign :: RawRequest -> AWS Request
sign raw@RawRequest{..} = do
    auth <- getAuth
    reg  <- region rawService
    time <- liftIO getCurrentTime

    let sig = svcSigner rawService
        hs  = hHost (endpoint rawService reg) : rawHeaders

    return $! sig (raw { rawHeaders = hs }) auth reg time

v2 :: AWSSigner
v2 raw@RawRequest{..} auth reg time =
    signed rawMethod _host rawPath query headers rawBody
  where
    Common{..} = common raw reg

    query = encoded <> "&Signature=" <> HTTP.urlEncode True signature

    signature = Base64.encode
        . hmacSHA256 (secretAccessKey auth)
        $ BS.intercalate "\n"
            [ BS.pack $ show rawMethod
            , _host
            , rawPath
            , encoded
            ]

    encoded = HTTP.renderQuery False
        $ _query
       ++ [ ("Version",          Just _version)
          , ("SignatureVersion", Just "2")
          , ("SignatureMethod",  Just "HmacSHA256")
          , ("Timestamp",        Just $ formatISO8601 time)
          , ("AWSAccessKeyId",   Just $ accessKeyId auth)
          ]
       ++ maybeToList ((\t -> ("SecurityToken", Just t)) <$> securityToken auth)

    headers = hDate (formatISO8601 time) : rawHeaders

v3 :: AWSSigner
v3 raw@RawRequest{..} auth reg time =
    signed rawMethod _host rawPath query headers rawBody
  where
    Common{..} = common raw reg

    query   = HTTP.renderQuery False _query
    headers = hDate (formatRFC822 time)
        : hAMZAuth authorisation
        : maybeToList (hAMZToken <$> securityToken auth)
       ++ rawHeaders

    authorisation = "AWS3-HTTPS AWSAccessKeyId="
        <> accessKeyId auth
        <> ", Algorithm=HmacSHA256, Signature="
        <> Base64.encode (hmacSHA256 (secretAccessKey auth) $ formatRFC822 time)

v4 :: AWSSigner
v4 raw@RawRequest{..} auth reg time =
    signed rawMethod _host rawPath query (hAuth authorisation : headers) rawBody
  where
    Common{..} = common raw reg

    query   = HTTP.renderQuery False . sort $ ("Version", Just _version) : _query
    headers = hAMZDate time
            : maybeToList (hAMZToken <$> securityToken auth)
           ++ rawHeaders

    authorisation = mconcat
        [ algorithm
        , " Credential="
        , accessKeyId auth
        , "/"
        , credentialScope
        , ", SignedHeaders="
        , signedHeaders
        , ", Signature="
        , signature
        ]

    signature  = Base16.encode $ hmacSHA256 signingKey stringToSign
    signingKey = foldl1 hmacSHA256 $ ("AWS4" <> secretAccessKey auth) : scope

    stringToSign = BS.intercalate "\n"
        [ algorithm
        , formatAWS time
        , credentialScope
        , Base16.encode $ SHA256.hash canonicalRequest
        ]

    credentialScope = BS.intercalate "/" scope

    algorithm = "AWS4-HMAC-SHA256"
    scope     = [formatBasic time, BS.pack $ show reg, _service, "aws4_request"]

    canonicalRequest = BS.intercalate "\n"
        [ BS.pack $ show rawMethod
        , rawPath
        , query
        , canonicalHeaders
        , signedHeaders
        , bodySHA256
        ]

    canonicalHeaders = mconcat $ map flattenValues grouped

    signedHeaders = BS.intercalate ";" . nub $
        map (Case.foldedCase . fst) grouped

    grouped = groupHeaders headers

    bodySHA256 = Base16.encode $ SHA256.hash ""
     -- sinkHash :: (Monad m, Hash ctx d) => Consumer ByteString m SHA256

s3 :: ByteString -> AWSSigner
s3 bucket raw@RawRequest{..} auth reg time =
    signed rawMethod _host rawPath query (authorisation : headers) rawBody
  where
    Common{..} = common raw reg

    query = HTTP.renderQuery False _query

    authorisation = hAuth $ BS.concat ["AWS ", accessKeyId auth, ":", signature]

    signature = Base64.encode $ hmacSHA1 (secretAccessKey auth) stringToSign

    stringToSign = BS.concat
        [ BS.pack $ show rawMethod
        , "\n"
        , optionalHeader "content-md5"
        , "\n"
        , optionalHeader "content-type"
        , "\n"
        , date
        , "\n"
        , canonicalHeaders
        , canonicalResource
        ]

    optionalHeader = fromMaybe "" . (`lookupHeader` headers)

    canonicalHeaders = BS.intercalate "\n"
        . map flattenValues
        . filter (BS.isPrefixOf "x-amz-" . Case.foldedCase . fst)
        $ groupHeaders headers

    headers = hDate date
        : maybeToList (hAMZToken <$> securityToken auth)
       ++ rawHeaders

    date = Text.encodeUtf8 $ formatRFC822 time

    canonicalResource = Text.encodeUtf8 $
        wrap '/' bucket <> stripPrefix "/" rawPath

    -- relevantQueryKeys =
    --     [ "acl"
    --     , "cors"
    --     , "defaultObjectAcl"
    --     , "location"
    --     , "logging"
    --     , "partNumber"
    --     , "policy"
    --     , "requestPayment"
    --     , "torrent"
    --     , "versioning"
    --     , "versionId"
    --     , "versions"
    --     , "website"
    --     , "uploads"
    --     , "uploadId"
    --     , "response-content-type"
    --     , "response-content-language"
    --     , "response-expires"
    --     , "response-cache-control"
    --     , "response-content-disposition"
    --     , "response-content-encoding"
    --     , "delete"
    --     , "lifecycle"
    --     , "tagging"
    --     , "restore"
    --     , "storageClass"
    --     , "notification"
    --     ]

data Common = Common
    { _service :: !ByteString
    , _version :: !ByteString
    , _host    :: !ByteString
    , _path    :: !ByteString
    , _query   :: Query
    }

common :: RawRequest -> Region -> Common
common RawRequest{..} reg = Common
    { _service = Text.encodeUtf8 $ svcName rawService
    , _version = Text.encodeUtf8 $ svcVersion rawService
    , _host    = Text.encodeUtf8 $ endpoint rawService reg
    , _path    = Text.encodeUtf8 rawPath
    , _query   = HTTP.queryTextToQuery $ sort rawQuery
    }

signed :: StdMethod
       -> ByteString
       -> ByteString
       -> ByteString
       -> [Header]
       -> RequestBody
       -> Request
signed meth host path qry hs body = def
    { secure         = True
    , method         = BS.pack $ show meth
    , host           = host
    , port           = 443
    , path           = path
    , queryString    = qry
    , requestHeaders = hs
    , requestBody    = body
    , checkStatus    = \_ _ _ -> Nothing
    }

hmacSHA1 :: ByteString -> ByteString -> ByteString
hmacSHA1 key msg = HMAC.hmac SHA1.hash 64 key msg

hmacSHA256 :: ByteString -> ByteString -> ByteString
hmacSHA256 key msg = HMAC.hmac SHA256.hash 64 key msg

groupHeaders :: [Header] -> [Header]
groupHeaders = sort . map f . groupBy ((==) `on` fst)
  where
    f (h:hs) = (fst h, BS.intercalate "," . sort . map snd $ h : hs)
    f []     = ("", "")

lookupHeader :: ByteString -> [Header] -> Maybe ByteString
lookupHeader key = lookup (Case.mk key)

flattenValues :: Header -> ByteString
flattenValues (k, v) = mconcat [Case.foldedCase k, ":", strip ' ' v, "\n"]

strip :: Char -> ByteString -> ByteString
strip c = Text.encodeUtf8 . Text.dropAround (== c) . Text.decodeUtf8

accessKeyId :: Auth -> ByteString
accessKeyId = Text.encodeUtf8 . authAccessKeyId

secretAccessKey :: Auth -> ByteString
secretAccessKey = Text.encodeUtf8 . authSecretAccessKey

securityToken :: Auth -> Maybe ByteString
securityToken = fmap Text.encodeUtf8 . authSecurityToken
