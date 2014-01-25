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
import           Data.Aeson
import           Data.Char           (isDigit)
import           Data.Foldable       (foldl')
import qualified Data.HashMap.Strict as Map
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as Set
import qualified Data.List           as List
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Data.Text.Lazy.IO   as LText
import           Helpers
import           Model
import           System.Directory
import           System.Environment
import           System.Exit
import           Text.EDE            (Template)
import qualified Text.EDE            as EDE
import           Text.EDE.Filters

-- FIXME:
-- ElasticCache: shape_names Endpoint and AvailablityZone need to be disambiguated

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

            title "Expanding cabal configuration"
            cabalFile "lib" ts ms

            title $ "Generated " ++ show (length ms) ++ " models successfully."
            end "Completed."

    usage = do
        n <- getProgName
        putStrLn $ "Usage: " ++ n ++ " [PATH] ..."

cabalFile :: FilePath -> Templates -> [Model] -> Script ()
cabalFile dir Templates{..} ms = do
    scriptIO $ createDirectoryIfMissing True dir
    render (dir </> "amazonka.cabal") tCabalFile $ EDE.fromPairs
        [ "models" .= map js ms
        ]
  where
    js Model{..} = EDE.fromPairs
        [ "module"     .= mName
        , "operations" .= map oName mOperations
        ]

model :: FilePath -> Templates -> Model -> Script ()
model dir Templates{..} m@Model{..} = do
    let root = dir </> Text.unpack mName

    scriptIO $ createDirectoryIfMissing True root

    -- <dir>/<Service>.hs
    renderInterface (root <.> "hs") tInterface

    -- <dir>/<Service>/Service.hs
    renderService (root </> "Service.hs") $
        case mType of
            RestXml   -> tRestXMLService
            RestJson  -> tRestJSONService
            Json      -> tJSONService
            Query     -> tQueryService

    -- <dir>/<Service>/Types.hs
    renderTypes (root </> "Types.hs") tTypes

    -- <dir>/<Service>/[Operation..].hs
    forM_ ops $ \op ->
        renderOperation (root </> Text.unpack (oName op) <.> "hs") op $
            case mType of
                RestXml | "s3" == mEndpointPrefix -> tS3Operation
                RestXml                           -> tRestXMLOperation
                RestJson                          -> tRestJSONOperation
                Json                              -> tJSONOperation
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

    renderOperation p o@Operation{..} t = do
        let Object o' = toJSON o
        render p t $ mJSON <> o'

updateOperation :: HashSet Text -> Operation -> (HashSet Text, Operation)
updateOperation s1 o@Operation{..} = f oInput oOutput
  where
    f (Just x) (Just y) =
        let (pre, (s2, inp)) = g s1 x
            opre             = pre <> "rs"
            out              = prefixes opre $ replace y
        in  ( Set.insert opre s2
            , o { oInput  = Just inp
                , oOutput = Just out
                , oPagination = p pre opre <$> oPagination
                }
            )

    f (Just x) Nothing =
        let (_, (s2, inp)) = g s1 x
        in  (s2, o { oInput = Just inp })

    f Nothing (Just y) =
        let (_, (s2, out)) = g s1 y
        in  (s2, o { oOutput = Just out })

    f Nothing Nothing =
        (s1, o)

    g s = disambiguate s . replace

    p x y l@Pagination{..} = l
        { pInputToken  = x <> upperFirst pInputToken
        , pOutputToken = y <> upperFirst pOutputToken
        }

errors :: Model -> [Shape]
errors = map replace
    . List.sort
    . List.nubBy cmp
    . concatMap oErrors
    . mOperations
  where
    a `cmp` b = sShapeName a == sShapeName b

types :: HashSet Text -> Model -> (HashSet Text, [Shape])
types set = disambiguateMany set
    . filter (except . sShapeName)
    . map replace
    . List.sort
    . List.nubBy cmp
    . concatMap shapes
    . mOperations
  where
    except (Just "Text") = False
    except _             = True

    a `cmp` b = sShapeName a == sShapeName b

    shapes Operation{..} = concatMap flatten
         $ fromMaybe [] (Map.elems . sFields <$> oInput)
        ++ fromMaybe [] (Map.elems . sFields <$> oOutput)

disambiguateMany :: HashSet Text -> [Shape] -> (HashSet Text, [Shape])
disambiguateMany set =
    foldl' (\(acc, xs) -> second (: xs) . snd . disambiguate acc) (set, [])

disambiguate :: HashSet Text -> Shape -> (Text, (HashSet Text, Shape))
disambiguate set s@SStruct{..} = (pre, (next, prefixes pre s))
  where
    (next, pre) = unique set
        . fromMaybe "pre"
        $ Text.toLower . lowerFilter <$> sShapeName

disambiguate set s = ("", (set, s))

unique :: HashSet Text -> Text -> (HashSet Text, Text)
unique set p
    | p `Set.member` set = unique set $ Text.init p `Text.snoc` succ (Text.last p)
    | otherwise = (Set.insert p set, p)

prefixes :: Text -> Shape -> Shape
prefixes pre s@SStruct{..} = s
    { sFields = Map.fromList . map f $ Map.toList sFields
    }
  where
    f (k, v) = (pre <> k, v)
prefixes _ s = s

flatten :: Shape -> [Shape]
flatten p@SPrim {..}
    | sType == PEnum = [p]
    | otherwise      = []
flatten SList   {..} = flatten sItem
flatten SMap    {..} = flatten sKey ++ flatten sValue
flatten s = s { sFields = fields } : concatMap flatten (Map.elems fields)
  where
    fields = Map.fromList
        . map name
        . Map.toList
        $ sFields s

    name (k, s')
        | Nothing <- sShapeName s' = (k, s' { sShapeName = (<> k) <$> sShapeName s })
        | otherwise                = (k, s')

-- FIXME:
-- The same way shape field names are disambiguated, enum values need to be

replace :: Shape -> Shape
replace s@SStruct {..} = s { sFields = Map.map replace sFields }
replace l@SList   {..} = l { sItem = replace sItem }
replace m@SMap    {..} = m { sKey = replace sKey, sValue = replace sValue }
replace p@SPrim   {..}
    | sType == PEnum
    , sShapeName == Just "String" = p { sShapeName = Just $ name sType }
    | sType == PEnum = p { sShapeName = upperFirst <$> sShapeName }
    | otherwise      = p { sShapeName = Just $ name sType }
  where
    name PString | sShapeName == Just "ResourceName" = "ResourceName"
    name PString    = "Text"
    name PEnum      = "Text"
    name PInteger   = "Int"
    name PDouble    = "Double"
    name PBoolean   = "Bool"
    name PBlob      = "ByteString"
    name PTimestamp = "UTCTime"
    name PLong      = "Integer"

data Templates = Templates
    { tInterface         :: Template
    , tTypes             :: Template
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
    }

templates :: Script Templates
templates = title "Listing tmpl" *>
    (Templates
        <$> load "tmpl/interface.ede"
        <*> load "tmpl/types.ede"
        <*> load "tmpl/service-rest-xml.ede"
        <*> load "tmpl/service-rest-json.ede"
        <*> load "tmpl/service-json.ede"
        <*> load "tmpl/service-query.ede"
        <*> load "tmpl/operation-s3.ede"
        <*> load "tmpl/operation-rest-xml.ede"
        <*> load "tmpl/operation-rest-json.ede"
        <*> load "tmpl/operation-json.ede"
        <*> load "tmpl/operation-query.ede"
        <*> load "tmpl/cabal.ede")
  where
    load p = msg ("Parsing " ++ p) *>
        scriptIO (EDE.eitherParseFile p) >>= hoistEither

models :: Script [FilePath]
models = fmap (take 1) $ do
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
    hs <- hoistEither $ EDE.eitherRenderWith filters t o
    msg $ "Writing " ++ p
    scriptIO $ LText.writeFile p hs
  where
    filters = defaultFilters <> Map.fromList
        [ ("enumPrefix", Fun TText TText name)
        , ("enumFormat", Fun TText TText format)
        ]

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
    name "SnapshotAttributeName"     = ""
    name "VolumeAttachmentState"     = "Volume"
    name n                           = upperFirst n

    format n
        | x <- Text.takeWhile (/= '_') n
        , isDigit $ Text.last x = Text.toUpper x <> xlarge (Text.drop (Text.length x) n)
        | otherwise             = n

    xlarge n
        | "Xl" `Text.isPrefixOf` n = "XL" <> Text.drop 2 n
        | otherwise                = n
