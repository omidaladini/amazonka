{-# LANGUAGE RecordWildCards #-}

-- Module      : Network.AWS.Signing.V4
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Signing.V4 (V4) where

import Network.AWS.Data
import Network.AWS.Signing.Types
import Network.AWS.Types

data V4

-- endpoint Service{..} reg =
--     case svcEndpoint of
--         Global   -> svcName <> ".amazonaws.com"
--         Regional -> BS.intercalate "." $ [svcName, reg, "amazonaws.com"]
--         Custom t -> t

-- try and make it look the same as the documentation's flow?

instance SigningAlgorithm V4 where
    finalise = undefined

--    finalise s@Service{..} c@Context{..} Auth{..} r t = undefined

--       where
--         host = endpoint s r
--         meth = toByteString ctxMethod
--         path = toByteString ctxPath
--         reg  = toByteString r

--         headers = hAMZDate t : maybeToList (hAMZToken <$> sigToken) ++ ctxHeaders

--         algorithm = "AWS4-HMAC-SHA256"

--         authorisation = mconcat
--             [ algorithm
--             , " Credential="
--             , authAccess
--             , "/"
--             , credentialScope
--             , ", SignedHeaders="
--             , signedHeaders
--             , ", Signature="
--             , signature
--             ]

--         signature = Base16.encode $ hmacSHA256 signingKey stringToSign

--         signingKey = foldl1 hmacSHA256 $ ("AWS4" <> authSecret) : scope

--         stringToSign = BS.intercalate "\n"
--             [ algorithm
--             , toByteString (AWSTime t)
--             , credentialScope
--             , Base16.encode $ SHA256.hash canonicalRequest
--             ]

--         credentialScope = BS.intercalate "/" scope

--         scope =
--             [ toByteString (BasicTime t)
--             , reg
--             , svcName
--             , "aws4sigRequest"
--             ]

--         canonicalRequest = BS.intercalate "\n"
--             [ meth
--             , path
--             , query
--             , canonicalHeaders
--             , signedHeaders
--             , bodySHA256
--             ]

--         canonicalHeaders = mconcat $ map flattenValues grouped

--         signedHeaders = BS.intercalate ";" . nub $ map (CI.foldedCase . fst) grouped

--         grouped = groupHeaders headers

--         bodySHA256 = Base16.encode $ SHA256.hash ""