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
import           Data.ByteString        (ByteString)
import qualified Data.HashMap.Strict    as Map
import qualified Data.List              as List
import           Data.Monoid
import qualified Data.Text              as Text
import           Data.Text              (Text)
import qualified Data.Text.Lazy.Builder as LText
import qualified Data.Text.Lazy.IO      as LText
import           Model
import           System.Directory
import           System.Environment
import           System.Exit
import           Text.EDE               (Template)
import qualified Text.EDE               as EDE

main :: IO ()
main = getArgs >>= parse
  where
    parse as
        | "-h" `elem` as = usage >> exitSuccess
        | otherwise = runScript $ do
            title "Running..."

            ts <- templates
            ms <- if null as then models else return as

            forM_ ms $ \p -> do
                title $ "Parsing " ++ p
                loadModel p >>= model "gen" ts

            title $ "Generated " ++ show (length ms) ++ " models successfully."
            end "Completed."

    usage = do
        n <- getProgName
        putStrLn $ "Usage: " ++ n ++ " [PATH] ..."

model :: FilePath -> Templates -> Model -> Script ()
model dir Templates{..} m@Model{..} = do
    let root = dir </> Text.unpack mName

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
    Object oJSON = toJSON m

    mJSON = oJSON <> EDE.fromPairs
        [ "module" .= mName
        , "types"  .= types m
        ]

    renderInterface p t = do
        render p t mJSON

    renderService p t = do
        render p t mJSON

    renderTypes p t = do
        render p t mJSON

    renderOperation p o@Operation{..} t = do
        let Object o' = toJSON $ o
                            { oInput  = replace <$> oInput
                            , oOutput = replace <$> oOutput
                            }
        render p t $ mJSON <> o'

render :: FilePath -> Template -> Object -> Script ()
render p t o = do
    hs <- hoistEither $ EDE.eitherRender t o
    msg $ "Writing " ++ p
    scriptIO $ LText.writeFile p hs

types :: Model -> [Shape]
types = filter (except . sShapeName)
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
         $ oErrors
        ++ maybeToList oInput
        ++ maybeToList oOutput

flatten SStruct {..} = concatMap flatten $ Map.elems sFields
flatten l@SList {..} = [sItem]
flatten SMap    {..} = flatten sKey ++ flatten sValue
flatten p@SPrim {..} = []

replace s@SStruct {..} = s { sFields = Map.map replace sFields }
replace l@SList   {..} = l { sItem = replace sItem }
replace m@SMap    {..} = m { sKey = replace sKey, sValue = replace sValue }
replace p@SPrim   {..} = p { sShapeName = Just $ name sType }
  where
    name PString    = "Text"
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
    , tRestXMLOperation  :: Template
    , tRestJSONOperation :: Template
    , tJSONOperation     :: Template
    , tQueryOperation    :: Template
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
        <*> load "tmpl/operation-rest-xml.ede"
        <*> load "tmpl/operation-rest-json.ede"
        <*> load "tmpl/operation-json.ede"
        <*> load "tmpl/operation-query.ede")
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
