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

import           Amazonka.Log
import           Amazonka.Model
import           Control.Applicative
import           Control.Error
import           Control.Monad
import           Data.Aeson
import qualified Data.HashMap.Strict    as Map
import qualified Data.List              as List
import           Data.Monoid
import qualified Data.Text              as Text
import qualified Data.Text.Lazy.Builder as LText
import qualified Data.Text.Lazy.IO      as LText
import           System.Directory
import           Text.EDE               (Template)
import qualified Text.EDE               as EDE
import           Text.Show.Pretty

main :: IO ()
main = runScript $ do
    title "Running..."
    ms <- models >>= mapM loadModel
    ts <- templates
    title $ "Generated " ++ show (length ms) ++ " models successfully."
    end "Completed."

model :: FilePath -> Model -> Templates -> Script ()
model dir m@Model{..} Templates{..} = do
    title $ "Processing " ++ Text.unpack mServiceFullName

    let model = Text.unpack mName
        root  = dir </> model

    msg $ "Creating " ++ root
    scriptIO $ createDirectoryIfMissing True root

    -- <dir>/<Service>.hs
    renderInterface (root <.> "hs") tInterface

    -- <dir>/<Service>/Service.hs
    renderService (root </> "Service.hs") $
        case mType of
            RestXml  -> tRestXMLService
            RestJson -> tRestJSONService
            Json     -> tJSONService
            Query    -> tQueryService

    -- <dir>/<Service>/Types.hs
    renderTypes (root </> "Types.hs") tTypes

    -- <dir>/<Service>/[Operation..].hs
    forM_ mOperations $ \op ->
        renderOperation (root </> Text.unpack (oName op) <.> "hs") op $
            case mType of
                RestXml  -> tRestXMLOperation
                RestJson -> tRestJSONOperation
                Json     -> tJSONOperation
                Query    -> tQueryOperation
  where
    renderInterface p t = do
        msg $ "Rendering Interface"

    renderService p t = do
        msg "Rendering Service"
        let Object o = toJSON m
        render p t $ o <> EDE.fromPairs
            [ "module" .= mName
            ]

    renderTypes p t = do
        msg "Rendering Types"
        render p t $ EDE.fromPairs
            [ "module" .= mName
            , "types"  .= types m
            ]

    renderOperation p Operation{..} t = do
        msg $ "Rendering " ++ Text.unpack oName

render :: FilePath -> Template -> Object -> Script ()
render p t o = do
    hs <- hoistEither $ EDE.eitherRender o t
    msg $ "Writing " ++ p
    scriptIO . LText.writeFile p $ LText.toLazyText hs

types :: Model -> [Shape]
types = List.nubBy cmp . concatMap shapes . mOperations
  where
    a `cmp` b = sShapeName a == sShapeName b

    shapes Operation{..} = concatMap flatten
         $ oErrors
        ++ maybeToList oInput
        ++ maybeToList oOutput

    flatten SStruct {..} = concatMap flatten $ Map.elems sFields
    flatten SList   {..} = [sItem]
    flatten SMap    {..} = [sKey, sValue]
    flatten s            = [s]

data Templates = Templates
    { tInterface         :: Template
    , tTypes             :: Template
    , tRestXMLService    :: Template
    , tRestJSONService   :: Template
    , tJSONService       :: Template
    , tQueryService      :: Template
    , tRestXMLOperation  :: Template
    , tRestJSONOperation :: Template
    , tJSONOperation     :: Template
    , tQueryOperation    :: Template
    }

templates :: Script Templates
templates = title "Listing ./tmpl" *>
    (Templates
        <$> load "./tmpl/interface.ede"
        <*> load "./tmpl/types.ede"
        <*> load "./tmpl/service-rest-xml.ede"
        <*> load "./tmpl/service-rest-json.ede"
        <*> load "./tmpl/service-json.ede"
        <*> load "./tmpl/service-query.ede"
        <*> load "./tmpl/operation-rest-xml.ede"
        <*> load "./tmpl/operation-rest-json.ede"
        <*> load "./tmpl/operation-json.ede"
        <*> load "./tmpl/operation-query.ede")
  where
    load p = do
        msg $ "Parsing " ++ p
        scriptIO (LText.readFile p) >>= hoistEither . EDE.eitherParse

models :: Script [FilePath]
models = fmap (take 1) $ do
    title $ "Listing " ++ dir
    xs <- scriptIO $ getDirectoryContents dir
    return . map (dir </>) $ filter f xs
  where
    f xs = ".json" `List.isSuffixOf` xs && not ("_" `List.isPrefixOf` xs)
    dir  = "./vendor/botocore/botocore/data/aws"

(</>) :: FilePath -> FilePath -> FilePath
(</>) x y = concat [z, "/", y]
  where
    z = if "/" `List.isSuffixOf` x
            then take (length x - 1) x
            else x

(<.>) :: FilePath -> String -> FilePath
(<.>) p ext = concat [p, ".", ext]
