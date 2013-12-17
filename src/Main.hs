{-# LANGUAGE OverloadedStrings #-}

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

import           Amazonka.Model
import           Amazonka.Log
import           Control.Applicative
import           Control.Error
import qualified Data.List           as List
import qualified Data.Text.Lazy.IO   as LText
import           System.Directory
import qualified Text.EDE            as EDE
import           Text.Show.Pretty

data Templates = Templates
    { tmplInterface :: EDE.Template
    , tmplService   :: EDE.Template
    , tmplQuery     :: EDE.Template
    , tmplRestXML   :: EDE.Template
    }

main :: IO ()
main = runScript $ do
    title "Launching the dethstarr..."
    ts <- templates
    ms <- models >>= mapM loadModel
    mapM_ (msg . ppShow) ms

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
models = fmap (take 1) $ do
    title $ "Listing " ++ dir
    xs <- scriptIO $ getDirectoryContents dir
    return . map (dir ++) $ filter f xs
  where
    f xs = ".json" `List.isSuffixOf` xs && not ("_" `List.isPrefixOf` xs)
    dir  = "vendor/botocore/botocore/data/aws/"

-- strip :: [Text] -> Text -> Text
-- strip ps t = foldl' (\acc x -> fromMaybe acc $ x `Text.stripPrefix` acc) t ps

-- service :: Templates -> FilePath -> Script ()
-- service t@Templates{..} path = do
--     title $ "Deserialising " ++ path

--     Object s <- scriptIO (LBS.readFile path) >>= hoistEither . eitherDecode

--     n <- tryLast ("Failed to determine service from: " ++ path) .
--         Text.split (== '/') $ Text.pack path

--     m <- failWith "Unknown key service_full_name" $
--         Object s ^? key "service_full_name" . _String

--     let mname = mconcat . Text.words $ ["Amazon", "AWS"] `strip` m
--         dir   = "gen/" ++ Text.unpack mname

--     scriptIO $ createDirectoryIfMissing True dir

--     render (dir ++ "/Service.hs")
--            (s <> Map.fromList ["module" .= mname])
--            tmplService

--     t <- types s

--     msg $ show t

-- --types :: Object -> EitherT String IO [Shape]
-- types svc = do
--     mapM (uncurry shapes) (concat ops)
--   where
--     ops = Object svc ^.. key "operations" . _Object . to Map.toList

-- shapes :: Text -> Value -> Script ()
-- shapes op v = do
--     msg $ Text.unpack op
--     msg $ show prse
--     msg ""
--   where
--     members k = v ^? key k >>= (^? key "members" . _Object)

--     prse = map (\x -> parseEither parseJSON x :: Either String Shape)
--         . fromMaybe []
--         $ Map.elems <$> members "input"

--     -- flatten :: Shape -> [Shape]
--     -- flatten = concatMap flatten . fromMaybe [] . _members

--          -- . _Object
--          -- . toListOf traverse
-- --         . to (^? key k)

-- -- types :: (Applicative m, Monad m) => Object -> EitherT String m [Shape]
-- -- types o = do
-- --     os <- failWith "operations" $ (Just (Object o) ^? key "operations" :: Maybe Object)
-- --     is <- mapM (\x -> failWith "input" $ Just x ^? key "input") $ Map.elems os
-- --     concat <$> mapM shapes is

-- -- interface :: Templates -> FilePath -> Text -> [Export] -> Script ()
-- -- interface t@Templates{..} dir mname ops = do
-- --     let out      = concat [dir, "/", Text.unpack mname, ".hs"]
-- --         Object o = object ["module" .= mname, "operations" .= ops]
-- --     render out o tmplInterface

-- -- operation :: EDE.Template -> FilePath -> Text -> Text -> Value -> Script Export
-- -- operation tmpl dir mname oname (Object o) = do
-- --     let out = concat [dir, "/", Text.unpack oname, ".hs"]
-- --     render out o tmpl
-- --     return $! Export oname [oname]
-- -- operation _ _ mname oname _ =
-- --     left . Text.unpack . mconcat $
-- --         [ "Attempted to process non-object operation for: "
-- --         , mname
-- --         , " - "
-- --         , oname
-- --         ]

-- render :: FilePath -> Object -> EDE.Template -> Script ()
-- render p o t = do
--     hs <- hoistEither $ EDE.eitherRender o t
--     msg $ "Writing " ++ p
--     scriptIO . LText.writeFile p $ LText.toLazyText hs
