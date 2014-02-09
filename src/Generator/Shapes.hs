{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Generator.Shapes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Generator.Shapes where

import           Control.Applicative
import           Control.Arrow
import           Control.Error
import           Data.Foldable       (foldl')
import qualified Data.HashMap.Strict as Map
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as Set
import qualified Data.List           as List
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Generator.Helpers
import           Generator.Model
import           Text.EDE.Filters

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
         $ fromMaybe [] (replaceFields <$> oInput)
        ++ fromMaybe [] (replaceFields <$> oOutput)
      where
        replaceFields = map (\(x, y) -> replace (name x) y)
            . Map.toList
            . sFields

        name "" = Just oName
        name x  = Just x

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
unique set p = (Set.insert pre set, pre)
  where
    pre = go Nothing

    go x =
        let t = maybe p (Text.snoc p) x
        in if Set.member t set
               then go $ maybe (Just 'd') (Just . next) x
               else t

    next x
        | x == 'z'  = 'a'
        | otherwise = succ x

prefixes :: Text -> Shape -> Shape
prefixes pre s@SStruct{..} = s
    { sFields = Map.fromList . map f $ Map.toList sFields
    , sPrefix = pre
    }
  where
    f (k, v) = (pre <> upperFirst k, v)
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
    go s@SStruct{..} = s
        { sFields = replaceFields sFields
        }

    go l@SList{..} = l
        { sItem = replace (eitherKey $ sXmlname <|> sShapeName) sItem
        }

    go m@SMap{..} = m
        { sKey   = replace (eitherKey sShapeName) sKey
        , sValue = replace (eitherKey sShapeName) sValue
        }

    go p@SPrim   {..}
        | sType == PEnum
        , sShapeName == Just "String" = p { sShapeName = Just $ name sType }
        | sType == PEnum = p { sShapeName = upperFirst <$> sShapeName }
        | otherwise      = p { sShapeName = Just $ name sType }

    name PEnum      = "Text"
    name PInteger   = "Int"
    name PDouble    = "Double"
    name PBoolean   = "Bool"
    name PBlob      = "Blob"
    name PTimestamp = "UTCTime"
    name PLong      = "Integer"
    name PString
        | cur `elem` ["ResourceName", "Key"] = cur
        | otherwise                          = "Text"
      where
        cur = fromMaybe "Text" $ sShapeName s'

    eitherKey x = x <|> k

    replaceFields = Map.fromList
        . map (\(x, y) -> (x, replace (Just x) y))
        . Map.toList

setName :: Maybe Text -> Shape -> Shape
setName k s = s { sShapeName = Just $ f name }
  where
    name = case (sShapeName s, sXmlname s, k) of
        (Just x, _, _)                          -> ("shape_name", x)
        (Nothing, Just "xsi:type", Just "Type") -> ("parent shape", "GranteeType")
        (Nothing, Just x, _)                    -> ("xmlname", x)
        (Nothing, Nothing, Just x)              -> ("parent shape", x)
        (Nothing, Nothing, Nothing)             -> ("none", "")

    f (_, "CompleteMultipartUpload") = "MultipartUpload"

    f (n, "") = error $ "Empty or no name (setName) for: " ++ show (n :: Text, s)
    f (_, x)  = x
