{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Generator.Render
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Generator.Render where

import           Control.Applicative
import           Control.Error
import           Control.Monad
import           Data.Aeson
import qualified Data.Foldable       as Fold
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Data.Text.Lazy.IO   as LText
import           Data.Text.Util
import           Generator.AST
import           Generator.Log
import           Generator.ToJSON    ()
import           System.Directory
import           System.FilePath     hiding (normalise)
import           Text.EDE            (Template)
import qualified Text.EDE            as EDE
import           Text.EDE.Filters

base :: FilePath
base = "lib"

class ToPath a where
    path :: a -> FilePath

instance ToPath Abbrev where
    path (Abbrev a) = base </> "Network/AWS" </> Text.unpack a <.> "hs"

instance ToPath NS where
    path (NS xs) = base </> (Text.unpack $ Text.intercalate "/" xs) <.> "hs"

data Templates = Templates
    { tmplCabal   :: Template
    , tmplMake    :: Template
    , tmplVersion :: Template
    , tmplCurrent :: Template
    , tmplService :: ServiceType -> (Template, Template)
    }

getTemplates :: Script Templates
getTemplates = do
    dir  <- scriptIO getCurrentDirectory

    let load f = loadTemplate (dir </> "tmpl" </> f <.> "ede")

    ctor <- Templates
        <$> load "cabal"
        <*> load "makefile"
        <*> load "version"
        <*> load "current"

    !xml <- (,) <$> load "types-rest-xml"  <*> load "operation-rest-xml"
    !rjs <- (,) <$> load "types-rest-json" <*> load "operation-rest-json"
    !s3  <- (,) <$> load "types-s3"        <*> load "operation-s3"
    !js  <- (,) <$> load "types-json"      <*> load "operation-json"
    !qry <- (,) <$> load "types-query"     <*> load "operation-query"

    return $! ctor $ \t ->
        case t of
            RestXml  -> xml
            RestJson -> rjs
            RestS3   -> s3
            Json     -> js
            Query    -> qry

loadTemplate :: FilePath -> Script Template
loadTemplate f =
    scriptIO (say "Parse Template" f *> EDE.eitherParseFile f)
        >>= hoistEither

render :: FilePath -> Templates -> [Service] -> Script ()
render dir Templates{..} ss = do
    forM_ ss $ \s@Service{..} -> do
        let (types, oper) = tmplService svcType

        forM_ svcOperations $ \o@Operation{..} ->
            write (path opNamespace) oper o

        write (path svcTypesNamespace) types s
        write (path svcVersionNamespace) tmplVersion s

    forM_ (current ss) $ \s ->
        write (path (svcName s)) tmplCurrent s

    write "amazonka.cabal" tmplCabal (Cabal ss)
    write "Makefile" tmplMake (Cabal ss)
  where
    write f t e = render' dir f t (env e)

render' :: ToJSON a => FilePath -> FilePath -> Template -> a -> Script ()
render' d f t e = scriptIO (say "Render" file) >> write
  where
    file = d </> f

    write = do
        hs <- hoistEither $ EDE.eitherRenderWith filters t (env e)
        scriptIO $ do
            createDirectoryIfMissing True (dropFileName file)
            LText.writeFile file hs

env :: ToJSON a => a -> Object
env x =
    case toJSON x of
        Object o -> o
        e        -> error ("Failed to extract JSON Object from: " ++ show e)

filters :: HashMap Text Fun
filters = EDE.defaultFilters <> Map.fromList fs
  where
    fs = funN "pad"    pad         [4, 8]
      ++ funN "indent" indent      [4, 6, 8]
      ++ funN "wrap"   (wrap "")   [66, 76, 80]
      ++ funN "above"  (wrap "| ") [66, 76]
      ++ funN "below"  (wrap "^ ") [66, 76]

    wrap p n t =
        case normalise n t of
            []       -> ""
            (x : xs) -> Text.intercalate "\n" . map ("-- " <>) $ p <> x : xs

    funN k g = Fold.foldl' (f k g) []

    f k g xs n = (k <> Text.pack (show n), Fun TText TText (g n)) : xs