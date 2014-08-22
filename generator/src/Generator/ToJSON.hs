{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TupleSections        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Generator.ToJSON
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Generator.ToJSON where

import           Control.Arrow
import           Control.Lens               ((^.))
import           Data.Aeson
import           Data.Aeson.Types
import           Data.CaseInsensitive       (CI)
import qualified Data.CaseInsensitive       as CI
import qualified Data.HashMap.Strict        as Map
import           Data.Monoid                hiding (Sum)
import           Data.String.CaseConversion
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import           GHC.Generics
import           Generator.AST
import           Generator.Transform
import           Network.HTTP.Types.Method

instance ToJSON a => ToJSON (CI a) where
    toJSON = toJSON . CI.original

instance ToJSON Abbrev where
    toJSON = toJSON . unAbbrev

instance ToJSON NS where
    toJSON = toJSON . Text.intercalate "." . unNS

instance ToJSON Version where
    toJSON = toJSON . unVersion

instance ToJSON Doc where
    toJSON = toJSON . unDoc

instance ToJSON Time where
    toJSON = toCtor lowered

instance ToJSON Checksum where
    toJSON = toCtor lowered

instance ToJSON ServiceType where
    toJSON = toJSON . show

instance ToJSON Signature where
    toJSON = toCtor id

instance ToJSON JSONV where
    toJSON = toJSON . unJSONV

instance ToJSON Library where
    toJSON (Library l) = toJSON l

instance ToJSON Cabal where
    toJSON = toField (recase Camel Under . drop 4)

instance ToJSON Service where
    toJSON s = Object (x <> y)
      where
        Object x = toField (recase Camel Under . drop 4) s
        Object y = object ["modules" .= serviceNamespaces s]

instance ToJSON Error where
    toJSON = toField (recase Camel Under . drop 3)

instance ToJSON Operation where
    toJSON = toField (recase Camel Under . drop 3)

instance ToJSON Request where
    toJSON rq@Request{..} = Object (x <> y <> z)
      where
        Object x = toJSON _rqHttp
        Object y = toField (recase Camel Under . drop 3) rq
        Object z = object
            [ "padding"        .= Text.replicate (Text.length _rqName) " "
            , "default_method" .= (length _rqFields /= length _rqRequired)
            ]

instance ToJSON RespType where
    toJSON = toCtor (recase Camel Under . drop 1)

instance ToJSON Response where
    toJSON = toField (recase Camel Under . drop 3)

instance ToJSON Location where
    toJSON = toCtor (lowered . drop 1)

instance ToJSON Direction where
    toJSON d = (\(rq, rs) -> object ["request" .= rq, "response" .= rs]) $
        case d of
            DRequest  -> (True, False)
            DResponse -> (False, True)
            DBoth     -> (True, True)

instance ToJSON Common where
    toJSON = toField (recase Camel Under . drop 4)

instance ToJSON Struct
instance ToJSON List
instance ToJSON Map
instance ToJSON Sum
instance ToJSON Prim

instance ToJSON Shape where
    toJSON s = Object (x <> y)
      where
        Object x =
            let f = recase Camel Under . drop 4
             in case s of
                SStruct s' -> toField f s'
                SList   s' -> toField f s'
                SMap    s' -> toField f s'
                SSum    s' -> toField f s'
                SPrim   s' -> toField f s'

        Object y = toJSON (s ^. common)

instance ToJSON Primitive where
    toJSON = toCtor (drop 1)

instance ToJSON Ann where
    toJSON (Ann _     _ True t) = toJSON t
    toJSON (Ann False _ _    t) = toJSON ("Maybe " <> t)
    toJSON (Ann _     _ _    t) = toJSON t

instance ToJSON Ctor where
    toJSON = toJSON . lowered . drop 1 . show

instance ToJSON Type where
    toJSON t = Object (x <> y)
      where
        Object x = toField (recase Camel Under . drop 4) t
        Object y = toJSON (_typShape t)

instance ToJSON Field where
    toJSON f = Object (x <> y)
      where
        Object x = toJSON (_fldCommon f)
        Object y = object
            [ "type"          .= _fldType f
            , "prefix_length" .= Text.length (f ^. cmnPrefix)
            , "prefixed"      .= _fldPrefixed f
            , "monoid"        .= _anMonoid  (_fldType f)
            , "default"       .= _anDefault (_fldType f)
            ]

instance ToJSON StdMethod where
    toJSON = toJSON . Text.toLower . Text.decodeUtf8 . renderStdMethod

instance ToJSON HTTP where
    toJSON = toField (recase Camel Under . drop 2)

instance ToJSON PathPart where
    toJSON p = case p of
        PConst c -> f "const" c
        PVar   v -> f "var" v
      where
        f k v = object ["type" .= (k :: Text), "value" .= v]

instance ToJSON [QueryPart] where
    toJSON = object . map (_qpKey &&& toJSON . _qpVal)

instance ToJSON Python where
    toJSON = toJSON . go
      where
        go p = case p of
            Empty      -> "id"
            Keyed    k -> k
            Index  x y -> "keyed " <> go y <> " " <> x
            Apply  x y -> go y <> " $ " <> x
            Choice x y -> "choice (" <> go x <> ") (" <> go y <> ")"

instance ToJSON Token where
    toJSON Token{..} = object
        [ "input"  .= _tokInput
        , "output" .= _tokOutput
        ]

instance ToJSON Pagination where
    toJSON p = object $
        case p of
            Next rk t ->
                [ "type"       .= ("next" :: Text)
                , "result_key" .= rk
                , "token"      .= t
                ]
            More k [t] ->
                [ "type"  .= ("one" :: Text)
                , "more"  .= k
                , "token" .= t
                ]
            More k ts ->
                let f x = (Text.pack ('p' : show x),)
                    m   = Map.fromList (zipWith f [1..length ts] ts)
                    fn  = ", isNothing "
                    pre = "and [isNothing " <> Text.intercalate fn (Map.keys m) <> "]"
                 in [ "type"   .= ("many" :: Text)
                    , "more"   .= k
                    , "tokens" .= m
                    , "negate" .= pre
                    ]

toField :: (Generic a, GToJSON (Rep a))
        => (String -> String)
        -> a
        -> Value
toField f = genericToJSON $ defaultOptions { fieldLabelModifier = f }

toCtor :: (Generic a, GToJSON (Rep a))
       => (String -> String)
       -> a
       -> Value
toCtor f = genericToJSON $ defaultOptions
    { constructorTagModifier = f
    , allNullaryToStringTag  = True
    , sumEncoding            = defaultTaggedObject { tagFieldName = "type" }
    }