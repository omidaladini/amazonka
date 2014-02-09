{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Generator.Operations
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Generator.Operations where

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
import           Generator.Helpers
import           Generator.Model
import           Generator.Shapes
import           Text.EDE.Filters

updateOperation :: HashSet Text -> Operation -> (HashSet Text, Operation)
updateOperation s1 o@Operation{..} = f oInput oOutput
  where
    f :: Maybe Shape -> Maybe Shape -> (HashSet Text, Operation)
    f (Just x) (Just y) =
        let (pre, (s2, inp)) = g s1 x
            opre             = pre <> "r"
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

    g s x = disambiguate s . replace (Just oName) $ x
        { sShapeName = stripSuffix "Response" . stripSuffix "Request" <$> sShapeName x
        }

    p x y l@Pagination{..} = l
        { pInputToken  = mappend x $ upperFirst pInputToken
        , pOutputToken = mappend y $ upperFirst pOutputToken
        }

    stripSuffix x y = fromMaybe y $ Text.stripSuffix x y
