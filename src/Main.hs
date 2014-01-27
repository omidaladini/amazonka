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
        case mServiceType of
            RestXML | "s3" == mEndpointPrefix -> tS3Service
            RestXML                           -> tRestXMLService
            RestJSON                          -> tRestJSONService
            JSON                              -> tJSONService
            Query                             -> tQueryService

    -- <dir>/<Service>/Types.hs
    renderTypes (root </> "Types.hs") tTypes

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

updateOperation :: HashSet Text -> Operation -> (HashSet Text, Operation)
updateOperation s1 o@Operation{..} = f oInput oOutput
  where
    f :: Maybe Shape -> Maybe Shape -> (HashSet Text, Operation)
    f (Just x) (Just y) =
        let (pre, (s2, inp)) = g s1 x
            opre             = pre <> "rs"
            out              = prefixes opre $ replace (Just oName) y
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

    g s = disambiguate s . replace (Just oName)

    p x y l@Pagination{..} = l
        { pInputToken  = mappend x $ upperFirst pInputToken
        , pOutputToken = mappend y $ upperFirst pOutputToken
        }

errors :: Model -> [Shape]
errors Model{..} = map (replace $ Just mName)
    . List.sort
    . List.nubBy cmp
    $ concatMap oErrors mOperations
  where
    a `cmp` b = sShapeName a == sShapeName b

types :: HashSet Text -> Model -> (HashSet Text, [Shape])
types set Model{..} = disambiguateMany set
    . List.nubBy cmp
    . filter (except . sShapeName)
    . List.sort
    $ concatMap shapes mOperations
  where
    except (Just "Text") = False
    except _             = True

    a `cmp` b = sShapeName a == sShapeName b

    shapes Operation{..} = concatMap flatten
         $ fromMaybe [] (f <$> oInput)
        ++ fromMaybe [] (f <$> oOutput)
      where
        f = map (\(x, y) -> replace (g x) y) . Map.toList . sFields

        g "" = Just oName
        g x  = Just x

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
    , sPrefix = pre
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

replace :: Maybe Text -> Shape -> Shape
replace k s' = setName k $ go s'
  where
    go s@SStruct {..} = s
        { sFields = Map.fromList . map (\(x, y) -> (x, replace (Just x) y)) $ Map.toList sFields
        }
    go l@SList   {..} = l { sItem = replace (f $ sXmlname <|> sShapeName) sItem }
    go m@SMap    {..} = m { sKey = replace (f sShapeName) sKey, sValue = replace (f sShapeName) sValue }
    go p@SPrim   {..}
        | sType == PEnum
        , sShapeName == Just "String" = p { sShapeName = Just $ name sType }
        | sType == PEnum = p { sShapeName = upperFirst <$> sShapeName }
        | otherwise      = p { sShapeName = Just $ name sType }

    name PString | Just "ResourceName" <- sShapeName s' = "ResourceName"
    name PString    = "Text"
    name PEnum      = "Text"
    name PInteger   = "Int"
    name PDouble    = "Double"
    name PBoolean   = "Bool"
    name PBlob      = "ByteString"
    name PTimestamp = "UTCTime"
    name PLong      = "Integer"

    f x = x <|> k

setName :: Maybe Text -> Shape -> Shape
setName k s = s { sShapeName = Just $ f name }
  where
    name = case (sShapeName s, sXmlname s, k) of
        (Just x, _, _)                          -> ("shape_name", x)
        (Nothing, Just "xsi:type", Just "Type") -> ("parent shape", "GranteeType")
        (Nothing, Just x, _)                    -> ("xmlname", x)
        (Nothing, Nothing, Just x)              -> ("parent shape", x)
        (Nothing, Nothing, Nothing)             -> ("none", "")

    f (n, "") = error $ "Empty or no name (setName) for: " ++ show (n :: Text, s)
    f (_, "CompleteMultipartUpload") = "MultipartUpload"
    f (_, x) = x


data Templates = Templates
    { tInterface         :: Template
    , tTypes             :: Template
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
    }

templates :: Script Templates
templates = title "Listing tmpl" *>
    (Templates
        <$> load "tmpl/interface.ede"
        <*> load "tmpl/types.ede"
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
    msg $ "Writing " ++ p
    hs <- hoistEither $ EDE.eitherRenderWith filters t o
    scriptIO $ LText.writeFile p hs
  where
    filters = defaultFilters <> Map.fromList
        [ ("enumPrefix",  Fun TText TText name)
        , ("enumFormat",  Fun TText TText format)
        , ("headers",     Fun TMap TMap headers)
        , ("onlyHeaders", Fun TMap TBool onlyHeaders)
        , ("fields",      Fun TMap TMap fields)
        , ("payload",     Fun TMap TBool payload)
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
    name "SnapshotAttributeName"     = "Snapshot"
    name "VolumeAttachmentState"     = "Volume"
    name n                           = upperFirst n

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
        f (Object s) = Map.lookup "location" s == Just "header"
        f _          = False

    onlyHeaders m = fromMaybe (error $ "Unable to determine onlyHeaders") $ do
       Object fs <- Map.lookup "fields" m
       return $ Map.size fs - Map.size (headers m) == 0

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
