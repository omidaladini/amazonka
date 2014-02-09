{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Main
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Main (main) where

import           Control.Applicative
import           Control.Arrow
import           Control.Error
import           Control.Monad
import           Data.Aeson           as Aeson
import           Data.Char            (isDigit)
import           Data.Foldable        (foldl')
import qualified Data.HashMap.Strict  as Map
import qualified Data.List            as List
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Text.Lazy.IO    as LText
import qualified Data.Vector          as Vector
import           Generator.Helpers
import           Generator.Model
import           Generator.Operations
import           Generator.Shapes
import           System.Directory
import           System.Environment
import           System.Exit
import           Text.EDE             (Template)
import qualified Text.EDE             as EDE
import           Text.EDE.Filters

-- FIXME:
-- Ability to wrap shapes in a data/newtype representing the service
-- A 'generic' foldMap instance for mapping over a single Shape and all it's descendents

version :: Text
version = "0.1.0"

main :: IO ()
main = getArgs >>= parse
  where
    parse as
        | "-h" `elem` as = usage >> exitSuccess
        | otherwise = runScript $ do
            title "Running..."

            ts <- templates
            js <- if null as then models else return as

            ms <- forM js $ \p -> do
                title $ "Parsing " ++ p
                m <- loadModel p
                model "lib/gen/Network/AWS" ts m
                return m

            title "Expanding amazonka cabal configuration"
            cabalFile version "lib" ts ms

            msg "Copying amazonka LICENSE"
            scriptIO . copyFile "LICENSE" $ "lib/LICENSE"

            msg $ "Generated " ++ show (length ms) ++ " models successfully."
            end "Completed."

    usage = do
        n <- getProgName
        putStrLn $ "Usage: " ++ n ++ " [PATH] ..."

cabalFile :: Text -> FilePath -> Templates -> [Model] -> Script ()
cabalFile ver dir Templates{..} ms =
    render "lib/amazonka.cabal" tCabalFile $ EDE.fromPairs
        [ "models"        .= map js ms
        , "cabal_name"    .= ("amazonka" :: Text)
        , "cabal_version" .= ver
        ]
  where
    js m@Model{..} = EDE.fromPairs
        [ "module"          .= mName
        , "operations"      .= map oName mOperations
        , "endpoint_prefix" .= mEndpointPrefix
        , "api_version"     .= mApiVersion
        ]

model :: FilePath -> Templates -> Model -> Script ()
model dir Templates{..} m@Model{..} = do
    let root = dir </> Text.unpack mName

    scriptIO $ createDirectoryIfMissing True root

    -- <dir>/<Service>.hs
    renderInterface (root <.> "hs") tInterface

    -- <dir>/<Service>/Service.hs
    renderService (root </> "Service.hs") $
        case mServiceType of
            RestXML | "s3" == mEndpointPrefix -> tS3Service
            RestXML                           -> tRestXMLService
            RestJSON                          -> tRestJSONService
            JSON                              -> tJSONService
            Query                             -> tQueryService

    -- <dir>/<Service>/Types.hs
    renderTypes (root </> "Types.hs") $
        case mServiceType of
            RestXML | "s3" == mEndpointPrefix -> tXMLTypes
            RestXML                           -> tXMLTypes
            RestJSON                          -> tJSONTypes
            JSON                              -> tJSONTypes
            Query                             -> tXMLTypes

    -- <dir>/<Service>/[Operation..].hs
    forM_ ops $ \op ->
        renderOperation (root </> Text.unpack (oName op) <.> "hs") op $
            case mServiceType of
                RestXML | "s3" == mEndpointPrefix -> tS3Operation
                RestXML                           -> tRestXMLOperation
                RestJSON                          -> tRestJSONOperation
                JSON                              -> tJSONOperation
                Query                             -> tQueryOperation
  where
    (_,   typs) = types pres m

    (pres, ops) =
        foldl' (\(acc, xs) -> second (: xs) . updateOperation acc)
               (mempty, [])
               mOperations

    Object oJSON = toJSON m

    mJSON = oJSON <> EDE.fromPairs
        [ "module" .= mName
        , "types"  .= typs
        ]

    renderInterface p t =
        render p t mJSON

    renderService p t =
        render p t $ mJSON <> EDE.fromPairs ["errors" .= errors m]

    renderTypes p t =
        render p t mJSON

    renderOperation p o@Operation{..} t = render p t $ mJSON <> o'
      where
        Object o' = toJSON $ o
            { oInput  = streaming True  <$> oInput
            , oOutput = streaming False <$> oOutput
            }

        streaming i s = s { sFields = Map.map (body i) $ sFields s }

        body i s@SPrim{..} | sStreaming = s
            { sShapeName = Just $
                if i
                    then "RequestBody"
                    else "ResumableSource AWS ByteString"
            , sRequired  = True
            , sStrict    = False
            }
        body _ x = x

data Templates = Templates
    { tInterface         :: Template
    , tXMLTypes          :: Template
    , tJSONTypes         :: Template
    , tS3Service         :: Template
    , tRestXMLService    :: Template
    , tRestJSONService   :: Template
    , tJSONService       :: Template
    , tQueryService      :: Template
    , tS3Operation       :: Template
    , tRestXMLOperation  :: Template
    , tRestJSONOperation :: Template
    , tJSONOperation     :: Template
    , tQueryOperation    :: Template
    , tCabalFile         :: Template
    , tCabalLibFile      :: Template
    }

templates :: Script Templates
templates = title "Listing tmpl" *>
    (Templates
        <$> load "tmpl/interface.ede"
        <*> load "tmpl/types-xml.ede"
        <*> load "tmpl/types-json.ede"
        <*> load "tmpl/service-s3.ede"
        <*> load "tmpl/service-rest-xml.ede"
        <*> load "tmpl/service-rest-json.ede"
        <*> load "tmpl/service-json.ede"
        <*> load "tmpl/service-query.ede"
        <*> load "tmpl/operation-s3.ede"
        <*> load "tmpl/operation-rest-xml.ede"
        <*> load "tmpl/operation-rest-json.ede"
        <*> load "tmpl/operation-json.ede"
        <*> load "tmpl/operation-query.ede"
        <*> load "tmpl/cabal.ede"
        <*> load "tmpl/cabal-lib.ede")
  where
    load p = msg ("Parsing " ++ p) *>
        scriptIO (EDE.eitherParseFile p) >>= hoistEither

models :: Script [FilePath]
models = do
    title $ "Listing " ++ dir
    xs <- scriptIO $ getDirectoryContents dir
    return . map (dir </>) $ filter f xs
  where
    f xs = ".json" `List.isSuffixOf` xs && not ("_" `List.isPrefixOf` xs)
    dir  = "vendor/botocore/botocore/data/aws"

(</>) :: FilePath -> FilePath -> FilePath
(</>) x y = concat [z, "/", y]
  where
    z = if "/" `List.isSuffixOf` x
            then take (length x - 1) x
            else x

(<.>) :: FilePath -> String -> FilePath
(<.>) p ext = concat [p, ".", ext]

title :: String -> Script ()
title s = scriptIO $ putStrLn "" >> putStrLn (" => " ++ s)

end :: String -> Script ()
end s = scriptIO $ putStrLn (" => " ++ s) >> putStrLn ""

msg :: String -> Script ()
msg = scriptIO . putStrLn . ("  - " ++)

render :: FilePath -> Template -> Object -> Script ()
render p t o = do
    msg $ "Writing " ++ p
    hs <- hoistEither $ EDE.eitherRenderWith filters t o
    scriptIO $ LText.writeFile p hs
  where
    filters = defaultFilters <> Map.fromList
        [ ("enumPrefix",  Fun TText TText name)
        , ("enumFormat",  Fun TText TText format)
        , ("headers",     Fun TMap  TMap  headers)
        , ("onlyHeaders", Fun TMap  TBool onlyHeaders)
        , ("fields",      Fun TMap  TMap  fields)
        , ("payload",     Fun TMap  TBool payload)
        , ("dropLower",   Fun TText TText dropLower)
        , ("required",    Fun TMap  TList required)
        , ("pad",         Fun TText TText pad)
        ]

    -- EC2
    name "AccountAttributeName"      = ""
    name "ArchitectureValues"        = ""
    name "ContainerFormat"           = ""
    name "CurrencyCodeValues"        = ""
    name "DiskImageFormat"           = ""
    name "EventCode"                 = ""
    name "ExportEnvironment"         = ""
    name "HypervisorType"            = ""
    name "InstanceType"              = ""
    name "OfferingTypeValues"        = ""
    name "ProductCodeValues"         = ""
    name "RIProductDescription"      = ""
    name "RecurringChargeFrequency"  = ""
    name "ReportInstanceReasonCodes" = ""
    name "ResourceType"              = "Resource"
    name "SnapshotAttributeName"     = "Snapshot"
    name "VolumeAttachmentState"     = "Volume"

    -- Kinesis
    name "ShardIteratorType"         = ""
    name "StreamStatus"              = ""

    name n                           = upperFirst n

    pad t = Text.replicate (Text.length t) " "

    format n
        | x <- Text.takeWhile (/= '_') n
        , not $ Text.null x
        , isDigit $ Text.last x = Text.toUpper x <> xlarge (Text.drop (Text.length x) n)
        | otherwise             = fromMaybe n $ Text.stripPrefix "S3:" n

    xlarge n
        | "Xl" `Text.isPrefixOf` n = "XL" <> Text.drop 2 n
        | otherwise                = n

    headers m = fromMaybe (error $ "Unable to filter headers: " ++ show m) $ do
        Object fs <- Map.lookup "fields" m
        return $ Map.filter f fs
      where
        f (Object s)
            | Map.lookup "location" s == Just "header" = True
        f _ = False

    onlyHeaders m = fromMaybe (error $ "Unable to determine onlyHeaders") $ do
       Object fs <- Map.lookup "fields" m
       let n = Map.size $ Map.filter f fs
       return $ n - Map.size (headers m) < 1
      where
        f (Object s) = Map.lookup "shape_name" s /= Just "Metadata"
        f _          = False

    fields m = fromMaybe (error $ "Unable to filter fields: " ++ show m) $ do
        Object fs <- Map.lookup "fields" m
        return $ Map.filter f fs
      where
        f (Object s) = Map.lookup "location" s /= Just "header"
        f _          = False

    payload m = fromMaybe False $ do
        Object fs <- Map.lookup "fields" m
        return . any f $ Map.elems fs
      where
        f (Object s) = maybe False (\(Bool b) -> b) $ Map.lookup "payload" s
        f _          = False

    required m = fromMaybe (error $ "Unable to filter required: " ++ show m) $ do
       Object fs <- Map.lookup "fields" m
       return . Vector.fromList
              . map toJSON
              . List.sortBy g
              . List.filter p
              . map f $ Map.toList fs
      where
        f (k, v) = OrdShape (dropLower k) v

        g a b = h (osKey a) (osKey b)

        h y x | x == y             = EQ
              | x == "Bucket"      = GT
              | y == "Bucket"      = LT
              | x == "Key"         = GT
              | y == "Key"         = LT
              | x == "RequestBody" = LT
              | y == "RequestBody" = GT
              | otherwise          = y `compare` x

        p (OrdShape _ (Object o)) = Map.lookup "required" o == Just (Bool True)
        p _                       = False

data OrdShape = OrdShape
    { osKey :: !Text
    , osVal :: !Value
    }

instance ToJSON OrdShape where
    toJSON (OrdShape k (Object o)) = Object $ Map.insert "param" (toJSON k) o
    toJSON (OrdShape k _) = error $ "Error inserting OrdShape key: " ++ Text.unpack k
