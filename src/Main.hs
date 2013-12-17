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
import qualified Data.HashMap.Strict     as Map
import qualified Data.List               as List
import qualified Data.Text               as Text
import qualified Data.Text.Lazy.Builder  as LText
import qualified Data.Text.Lazy.Encoding as LText
import qualified Data.Text.Lazy.IO       as LText
import           System.Directory
import           Text.EDE                (Template)
import qualified Text.EDE                as EDE
import           Text.Show.Pretty

data Templates = Templates
    { tInterface :: Template
    , tService   :: Template
    , tQuery     :: Template
    , tRestXML   :: Template
    }

main :: IO ()
main = runScript $ do
    title "Running..."

    Templates{..} <- templates
    ms@(m:_) <- models >>= mapM loadModel

    title $ "Processing " ++ Text.unpack (mServiceFullName m)

    let model = Text.unpack $ mName m
        root  = "gen" </> model

    msg $ "Creating " ++ root
    scriptIO $ createDirectoryIfMissing True root

    -- gen/<Service>.hs
    renderInterface (root <.> "hs") m tInterface

    -- gen/<Service>/Service.hs
    renderService (root </> "Service.hs") m tService

    -- gen/<Service>/Types.hs
    renderTypes (root </> "Types.hs") m tService

    -- gen/<Service>/[Operation..].hs
    forM_ (mOperations m) $ \op -> do
        renderOperation (root </> Text.unpack (oName op) <.> "hs") m op $
            case mType m of
                RestXml  -> tRestXML
                RestJson -> tRestXML
                Json     -> tQuery
                Query    -> tQuery

    title $ "Generated " ++ show (length ms) ++ " models successfully."
    end "Completed."

renderInterface :: FilePath -> Model -> Template -> Script ()
renderInterface p Model{..} t = do
    msg $ "Rendering " ++ Text.unpack mName <.> "hs"

renderService :: FilePath -> Model -> Template -> Script ()
renderService p Model{..} t = do
    msg "Rendering Service.hs"

renderTypes :: FilePath -> Model -> Template -> Script ()
renderTypes p Model{..} t = do
    msg "Rendering Types.hs"

renderOperation :: FilePath -> Model -> Operation -> Template -> Script ()
renderOperation p Model{..} Operation{..} t = do
    msg $ "Rendering " ++ Text.unpack oName <.> "hs"

render :: FilePath -> Object -> Template -> Script ()
render p o t = do
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
