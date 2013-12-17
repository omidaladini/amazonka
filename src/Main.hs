{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Main
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Main (main) where

import           Control.Applicative
import           Control.Error
import           Control.Monad
import           Data.Aeson
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as BS
import qualified Data.ByteString.Lazy    as LBS
import           Data.Foldable           (foldl')
import qualified Data.HashMap.Strict     as Map
import qualified Data.List               as List
import           Data.Maybe
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text
import qualified Data.Text.Lazy.Builder  as LText
import qualified Data.Text.Lazy.Encoding as LText
import qualified Data.Text.Lazy.IO       as LText
import           GHC.Generics
import           System.Directory
import qualified Text.EDE                as EDE

data Templates = Templates
    { tmplInterface :: EDE.Template
    , tmplService   :: EDE.Template
    , tmplQuery     :: EDE.Template
    , tmplRestXML   :: EDE.Template
    }

data Export = Export
    { expModule :: !Text
    , expTypes  :: [Text]
    }

instance ToJSON Export where
    toJSON Export{..} = object
        [ "module" .= expModule
        , "types"  .= expTypes
        ]

main :: IO ()
main = runScript $ do
    title "Launching the dethstarr..."
    ts <- templates
    ms <- models
    mapM_ (service ts) ms

templates :: Script Templates
templates = title "Processing tmpl/*.ede" *>
    (Templates
        <$> load "tmpl/interface.ede"
        <*> load "tmpl/service.ede"
        <*> load "tmpl/query.ede"
        <*> load "tmpl/rest-xml.ede")
  where
    load p = do
        msg $ "Parsing " ++ p
        scriptIO (LText.readFile p) >>= hoistEither . EDE.eitherParse

models :: Script [FilePath]
models = do
    title $ "Listing " ++ dir
    xs <- scriptIO $ getDirectoryContents dir
    return . map (dir ++) $ filter f xs
  where
    f xs = ".json" `List.isSuffixOf` xs && not ("_" `List.isPrefixOf` xs)
    dir  = "vendor/botocore/botocore/data/aws/"

service :: Templates -> FilePath -> Script ()
service t@Templates{..} path = do
    title $ "Deserialising " ++ path

    bs       <- scriptIO $ LBS.readFile path
    Object s <- hoistEither $ eitherDecode bs

    n <- tryLast ("Failed to determine service from: " ++ path) .
        Text.split (== '/') $ Text.pack path

    -- Take service full name then drop the Amazon prefix and spaces to make module name
    String m <- "service_full_name" =| s

    let dname = Text.unpack $ fromMaybe n $ ".json" `Text.stripSuffix` n
        mname = mconcat . Text.words $ ["Amazon", "AWS"] `strip` m
        dir   = "gen/" ++ Text.unpack mname
        obj   = s <> Map.fromList ["module" .= mname]
        out   = dir ++ "/Service.hs"

    scriptIO $ createDirectoryIfMissing True dir

    render out obj tmplService

    Object os <- "operations" =| obj

    let ops    = Map.toList os
        onames = map fst ops

    xops <- mapM (uncurry (operation t dir mname)) ops

    interface t "gen" mname xops

interface :: Templates -> FilePath -> Text -> [Export] -> Script ()
interface t@Templates{..} dir mname ops = do
    let out      = concat [dir, "/", Text.unpack mname, ".hs"]
        Object o = object ["module" .= mname, "operations" .= ops]

    render out o tmplInterface

-- TODO:
-- - return toJSON Export type from operation
-- - namespace modules by version again?

operation :: Templates -> FilePath -> Text -> Text -> Value -> Script Export
operation t@Templates{..} dir mname oname (Object o) = do
    let out = concat [dir, "/", Text.unpack oname, ".hs"]

    -- test if query or rest-xml
    render out o tmplQuery

    return $! Export oname [oname]
operation _ _ mname oname _ =
    left . Text.unpack . mconcat $
        [ "Attempted to process non-object operation for: "
        , mname
        , " - "
        , oname
        ]

(=|) :: Text -> Object -> Script Value
(=|) k o = Map.lookup k o ?? ("Unable to lookup key: " ++ Text.unpack k)

strip :: [Text] -> Text -> Text
strip ps t = foldl' (\acc x -> fromMaybe acc $ x `Text.stripPrefix` acc) t ps

title :: String -> Script ()
title s = scriptIO $ putStrLn "" >> putStrLn (" => " ++ s)

msg :: String -> Script ()
msg = scriptIO . putStrLn . ("  - " ++)

render :: FilePath -> Object -> EDE.Template -> Script ()
render p o t = do
   hs <- hoistEither $ EDE.eitherRender o t
   msg $ "Writing " ++ p
   scriptIO . LText.writeFile p $ LText.toLazyText hs
