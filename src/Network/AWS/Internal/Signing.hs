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
    , vS3
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
import qualified Data.CaseInsensitive              as CI
import           Data.Default
import           Data.Function                     (on)
import           Data.List                         (groupBy, nub, sort)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                         as Text
import qualified Data.Text.Encoding                as Text
import           Data.Time                         (getCurrentTime)
import           Network.AWS.Headers
import           Network.AWS.Internal.Types
import           Network.AWS.Internal.Types.Common
import           Network.AWS.Text
import           Network.AWS.Time
import           Network.HTTP.Conduit
import           Network.HTTP.Types                (Header)
import qualified Network.HTTP.Types                as HTTP

sign :: RawRequest -> AWS Request
sign RawRequest{..} = do
    Auth{..} <- getAuth
    reg      <- region
    time     <- liftIO getCurrentTime

    let host = endpoint reg

    return . svcSigner $ Signee
        { sigAccess  = Text.encodeUtf8 authAccessKeyId
        , sigSecret  = Text.encodeUtf8 authSecretAccessKey
        , sigToken   = Text.encodeUtf8 <$> authSecurityToken
        , sigTime    = time
        , sigRegion  = reg
        , sigService = svcName
        , sigVersion = svcVersion
        , sigMethod  = BS.pack $ show rawMethod
        , sigHost    = host
        , sigPath    = Text.encodeUtf8 rawPath
        , sigQuery   = HTTP.queryTextToQuery $ sort rawQuery
        , sigHeaders = hHost host : rawHeaders
        , sigBody    = rawBody
        }
  where
    Service{..} = rawService

    region = fmap (Text.encodeUtf8 . toText) $
        case svcEndpoint of
            Global -> return def
            _      -> getRegion

    endpoint reg =
        case svcEndpoint of
            Custom t -> t
            Global   -> svcName <> ".amazonaws.com"
            Regional -> BS.intercalate "." $ [svcName, reg, "amazonaws.com"]

signed :: ByteString
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

v2 :: Signee -> Request
v2 Signee{..} = signed sigMethod sigHost sigPath query headers sigBody
  where
    query = encoded <> "&Signature=" <> HTTP.urlEncode True signature

    signature = Base64.encode
        . hmacSHA256 sigSecret
        $ BS.intercalate "\n"
            [ sigMethod
            , sigHost
            , sigPath
            , encoded
            ]

    encoded = HTTP.renderQuery False
        $ sigQuery
       ++ [ ("Version",          Just sigVersion)
          , ("SignatureVersion", Just "2")
          , ("SignatureMethod",  Just "HmacSHA256")
          , ("Timestamp",        Just iso8601)
          , ("AWSAccessKeyId",   Just sigAccess)
          ]
       ++ maybeToList ((\t -> ("SecurityToken", Just t)) <$> sigToken)

    headers = hDate iso8601 : sigHeaders

    iso8601 = Text.encodeUtf8 $ formatISO8601 sigTime

v3 :: Signee -> Request
v3 Signee{..} = signed sigMethod sigHost sigPath query headers sigBody
  where
    query = HTTP.renderQuery False sigQuery

    headers = hDate rfc822
        : hAMZAuth authorisation
        : maybeToList (hAMZToken <$> sigToken)
       ++ sigHeaders

    authorisation = "AWS3-HTTPS AWSAccessKeyId="
        <> sigAccess
        <> ", Algorithm=HmacSHA256, Signature="
        <> Base64.encode (hmacSHA256 sigSecret rfc822)

    rfc822 = Text.encodeUtf8 $ formatRFC822 sigTime

v4 :: Signee -> Request
v4 Signee{..} =
    signed sigMethod sigHost sigPath query (hAuth authorisation : headers) sigBody
  where
    query = HTTP.renderQuery False . sort $ ("Version", Just sigVersion) : sigQuery

    headers = hAMZDate sigTime
        : maybeToList (hAMZToken <$> sigToken)
       ++ sigHeaders

    authorisation = mconcat
        [ algorithm
        , " Credential="
        , sigAccess
        , "/"
        , credentialScope
        , ", SignedHeaders="
        , signedHeaders
        , ", Signature="
        , signature
        ]

    signature = Base16.encode $ hmacSHA256 signingKey stringToSign

    signingKey = foldl1 hmacSHA256 $ ("AWS4" <> sigSecret) : scope

    stringToSign = BS.intercalate "\n"
        [ algorithm
        , Text.encodeUtf8 $ formatAWS sigTime
        , credentialScope
        , Base16.encode $ SHA256.hash canonicalRequest
        ]

    credentialScope = BS.intercalate "/" scope

    algorithm = "AWS4-HMAC-SHA256"

    scope =
        [ Text.encodeUtf8 $ formatBasic sigTime
        , sigRegion
        , sigService
        , "aws4sigRequest"
        ]

    canonicalRequest = BS.intercalate "\n"
        [ sigMethod
        , sigPath
        , query
        , canonicalHeaders
        , signedHeaders
        , bodySHA256
        ]

    canonicalHeaders = mconcat $ map flattenValues grouped

    signedHeaders = BS.intercalate ";" . nub $
        map (CI.foldedCase . fst) grouped

    grouped = groupHeaders headers

    bodySHA256 = Base16.encode $ SHA256.hash ""
     -- sinkHash :: (Monad m, Hash ctx d) => Consumer ByteString m SHA256

vS3 :: ByteString -> Signee -> Request
vS3 bucket Signee{..} =
    signed sigMethod sigHost sigPath query (authorisation : headers) sigBody
  where
    query = HTTP.renderQuery False sigQuery

    authorisation = hAuth $ "AWS " <> sigAccess <> ":" <> signature

    signature = Base64.encode $ hmacSHA1 sigSecret stringToSign

    stringToSign = BS.concat
        [ sigMethod
        , "\n"
        , optionalHeader "content-md5"
        , "\n"
        , optionalHeader "content-type"
        , "\n"
        , rfc822
        , "\n"
        , canonicalHeaders
        , canonicalResource
        ]

    optionalHeader = fromMaybe "" . (`lookupHeader` headers)

    canonicalHeaders = BS.intercalate "\n"
        . map flattenValues
        . filter (BS.isPrefixOf "x-amz-" . CI.foldedCase . fst)
        $ groupHeaders headers

    headers = hDate rfc822
        : maybeToList (hAMZToken <$> sigToken)
       ++ sigHeaders

    rfc822 = Text.encodeUtf8 $ formatRFC822 sigTime

    canonicalResource =
        mappend bucket $
            if BS.isPrefixOf "/" sigPath
                then sigPath
                else "/" <> sigPath

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
lookupHeader key = lookup (CI.mk key)

flattenValues :: Header -> ByteString
flattenValues (k, v) = mconcat [CI.foldedCase k, ":", strip ' ' v, "\n"]
  where
    strip c = Text.encodeUtf8 . Text.dropAround (== c) . Text.decodeUtf8
