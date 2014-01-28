{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.CloudSearch.Types where

import Data.ByteString      (ByteString)
import Data.Hashable
import Data.Monoid
import Data.List.NonEmpty
import Data.Text            (Text)
import Data.HashMap.Strict  (HashMap)
import Data.Time
import GHC.Generics         (Generic)
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.CloudSearch.Service

-- | Options for an unsigned integer field. Present if IndexFieldType specifies
-- the field is of type unsigned integer.
newtype UIntOptions = UIntOptions
    { uioDefaultValue :: Maybe Int
      -- ^ The default value for an unsigned integer field. Optional.
    } deriving (Eq, Show, Generic)

instance ToQuery UIntOptions

instance FromXML UIntOptions where
    fromXMLOptions = xmlOptions

instance ToXML UIntOptions where
    toXMLOptions = xmlOptions

-- | Options for text field. Present if IndexFieldType specifies the field is of
-- type text.
data TextOptions = TextOptions
    { toDefaultValue :: Maybe Text
      -- ^ The default value for a text field. Optional.
    , toFacetEnabled :: Maybe Bool
      -- ^ Specifies whether facets are enabled for this field. Default: False.
    , toResultEnabled :: Maybe Bool
      -- ^ Specifies whether values of this field can be returned in search results
      -- and used for ranking. Default: False.
    , toTextProcessor :: Maybe Text
      -- ^ The text processor to apply to this field. Optional. Possible values:
      -- cs_text_no_stemming: turns off stemming for the field. Default: none.
    } deriving (Eq, Show, Generic)

instance ToQuery TextOptions

instance FromXML TextOptions where
    fromXMLOptions = xmlOptions

instance ToXML TextOptions where
    toXMLOptions = xmlOptions

-- | The synonym options configured for this search domain and the current
-- status of those options.
data SynonymOptionsStatus = SynonymOptionsStatus
    { souOptions :: !Text
      -- ^ Maps terms to their synonyms, serialized as a JSON document. The document
      -- has a single object with one property "synonyms" whose value is an object
      -- mapping terms to their synonyms. Each synonym is a simple string or an
      -- array of strings. The maximum size of a stopwords document is 100 KB.
      -- Example: { "synonyms": {"cat": ["feline", "kitten"], "puppy": "dog"} }.
    , souStatus :: OptionStatus
      -- ^ The status of an option, including when it was last updated and whether it
      -- is actively in use for searches.
    } deriving (Eq, Show, Generic)

instance ToQuery SynonymOptionsStatus

instance FromXML SynonymOptionsStatus where
    fromXMLOptions = xmlOptions

instance ToXML SynonymOptionsStatus where
    toXMLOptions = xmlOptions

-- | The stopword options configured for this search domain and the current
-- status of those options.
data StopwordOptionsStatus = StopwordOptionsStatus
    { sotOptions :: !Text
      -- ^ Lists stopwords serialized as a JSON document. The document has a single
      -- object with one property "stopwords" whose value is an array of strings.
      -- The maximum size of a stopwords document is 10 KB. Example: { "stopwords":
      -- ["a", "an", "the", "of"] }.
    , sotStatus :: OptionStatus
      -- ^ The status of an option, including when it was last updated and whether it
      -- is actively in use for searches.
    } deriving (Eq, Show, Generic)

instance ToQuery StopwordOptionsStatus

instance FromXML StopwordOptionsStatus where
    fromXMLOptions = xmlOptions

instance ToXML StopwordOptionsStatus where
    toXMLOptions = xmlOptions

-- | The stemming options configured for this search domain and the current
-- status of those options.
data StemmingOptionsStatus = StemmingOptionsStatus
    { sosOptions :: !Text
      -- ^ Maps terms to their stems, serialized as a JSON document. The document has
      -- a single object with one property "stems" whose value is an object mapping
      -- terms to their stems. The maximum size of a stemming document is 500 KB.
      -- Example: { "stems": {"people": "person", "walking": "walk"} }.
    , sosStatus :: OptionStatus
      -- ^ The status of an option, including when it was last updated and whether it
      -- is actively in use for searches.
    } deriving (Eq, Show, Generic)

instance ToQuery StemmingOptionsStatus

instance FromXML StemmingOptionsStatus where
    fromXMLOptions = xmlOptions

instance ToXML StemmingOptionsStatus where
    toXMLOptions = xmlOptions

-- | Trims common title words from a source document attribute when populating
-- an IndexField. This can be used to create an IndexField you can use for
-- sorting.
data SourceDataTrimTitle = SourceDataTrimTitle
    { sdttDefaultValue :: Maybe Text
      -- ^ The default value to use if the source attribute is not specified in a
      -- document. Optional.
    , sdttLanguage :: Maybe Text
      -- ^ An IETF RFC 4646 language code. Only the primary language is considered.
      -- English (en) is currently the only supported language.
    , sdttSeparator :: Maybe Text
      -- ^ The separator that follows the text to trim.
    , sdttSourceName :: !Text
      -- ^ The name of the document source field to add to this IndexField.
    } deriving (Eq, Show, Generic)

instance ToQuery SourceDataTrimTitle

instance FromXML SourceDataTrimTitle where
    fromXMLOptions = xmlOptions

instance ToXML SourceDataTrimTitle where
    toXMLOptions = xmlOptions

-- | Maps source document attribute values to new values when populating the
-- IndexField.
data SourceDataMap = SourceDataMap
    { sdmCases :: HashMap Text Text
      -- ^ A map that translates source field values to custom values.
    , sdmDefaultValue :: Maybe Text
      -- ^ The default value to use if the source attribute is not specified in a
      -- document. Optional.
    , sdmSourceName :: !Text
      -- ^ The name of the document source field to add to this IndexField.
    } deriving (Eq, Show, Generic)

instance ToQuery SourceDataMap

instance FromXML SourceDataMap where
    fromXMLOptions = xmlOptions

instance ToXML SourceDataMap where
    toXMLOptions = xmlOptions

-- | Copies data from a source document attribute to an IndexField.
data SourceData = SourceData
    { sdDefaultValue :: Maybe Text
      -- ^ The default value to use if the source attribute is not specified in a
      -- document. Optional.
    , sdSourceName :: !Text
      -- ^ The name of the document source field to add to this IndexField.
    } deriving (Eq, Show, Generic)

instance ToQuery SourceData

instance FromXML SourceData where
    fromXMLOptions = xmlOptions

instance ToXML SourceData where
    toXMLOptions = xmlOptions

-- | Identifies the source data for an index field. An optional data
-- transformation can be applied to the source data when populating the index
-- field. By default, the value of the source attribute is copied to the index
-- field.
data SourceAttribute = SourceAttribute
    { saSourceDataCopy :: Maybe SourceData
      -- ^ Copies data from a source document attribute to an IndexField.
    , saSourceDataFunction :: !SourceDataFunction
      -- ^ Identifies the transformation to apply when copying data from a source
      -- attribute.
    , saSourceDataMap :: Maybe SourceDataMap
      -- ^ Maps source document attribute values to new values when populating the
      -- IndexField.
    , saSourceDataTrimTitle :: Maybe SourceDataTrimTitle
      -- ^ Trims common title words from a source document attribute when populating
      -- an IndexField. This can be used to create an IndexField you can use for
      -- sorting.
    } deriving (Eq, Show, Generic)

instance ToQuery SourceAttribute

instance FromXML SourceAttribute where
    fromXMLOptions = xmlOptions

instance ToXML SourceAttribute where
    toXMLOptions = xmlOptions

-- | The service endpoint for updating documents in a search domain.
data ServiceEndpoint = ServiceEndpoint
    { seArn :: Maybe Text
      -- ^ An Amazon Resource Name (ARN). See Identifiers for IAM Entities in Using
      -- AWS Identity and Access Management for more information.
    , seEndpoint :: Maybe Text
      -- ^ The URL (including /version/pathPrefix) to which service requests can be
      -- submitted.
    } deriving (Eq, Show, Generic)

instance ToQuery ServiceEndpoint

instance FromXML ServiceEndpoint where
    fromXMLOptions = xmlOptions

instance ToXML ServiceEndpoint where
    toXMLOptions = xmlOptions

-- | The value of a RankExpression and its current status.
data RankExpressionStatus = RankExpressionStatus
    { resOptions :: NamedRankExpression
      -- ^ The expression that is evaluated for ranking or thresholding while
      -- processing a search request.
    , resStatus :: OptionStatus
      -- ^ The status of an option, including when it was last updated and whether it
      -- is actively in use for searches.
    } deriving (Eq, Show, Generic)

instance ToQuery RankExpressionStatus

instance FromXML RankExpressionStatus where
    fromXMLOptions = xmlOptions

instance ToXML RankExpressionStatus where
    toXMLOptions = xmlOptions

-- | The status of an option, including when it was last updated and whether it
-- is actively in use for searches.
data OptionStatus = OptionStatus
    { osCreationDate :: !UTCTime
      -- ^ A timestamp for when this option was created.
    , osPendingDeletion :: Maybe Bool
      -- ^ Indicates that the option will be deleted once processing is complete.
    , osState :: !OptionState
      -- ^ The state of processing a change to an option. Possible values:
      -- RequiresIndexDocuments: the option's latest value will not be visible in
      -- searches until IndexDocuments has been called and indexing is complete.
      -- Processing: the option's latest value is not yet visible in all searches
      -- but is in the process of being activated. Active: the option's latest value
      -- is completely visible. Any warnings or messages generated during processing
      -- are provided in Diagnostics.
    , osUpdateDate :: !UTCTime
      -- ^ A timestamp for when this option was last updated.
    , osUpdateVersion :: Maybe Int
      -- ^ A unique integer that indicates when this option was last updated.
    } deriving (Eq, Show, Generic)

instance ToQuery OptionStatus

instance FromXML OptionStatus where
    fromXMLOptions = xmlOptions

instance ToXML OptionStatus where
    toXMLOptions = xmlOptions

-- | The expression that is evaluated for ranking or thresholding while
-- processing a search request.
data NamedRankExpression = NamedRankExpression
    { nreRankExpression :: !Text
      -- ^ The expression to evaluate for ranking or thresholding while processing a
      -- search request. The RankExpression syntax is based on JavaScript
      -- expressions and supports: Integer, floating point, hex and octal literals
      -- Shortcut evaluation of logical operators such that an expression a || b
      -- evaluates to the value a if a is true without evaluting b at all JavaScript
      -- order of precendence for operators Arithmetic operators: + - * / % Boolean
      -- operators (including the ternary operator) Bitwise operators Comparison
      -- operators Common mathematic functions: abs ceil erf exp floor lgamma ln
      -- log2 log10 max min sqrt pow Trigonometric library functions: acosh acos
      -- asinh asin atanh atan cosh cos sinh sin tanh tan Random generation of a
      -- number between 0 and 1: rand Current time in epoch: time The min max
      -- functions that operate on a variable argument list Intermediate results are
      -- calculated as double precision floating point values. The final return
      -- value of a RankExpression is automatically converted from floating point to
      -- a 32-bit unsigned integer by rounding to the nearest integer, with a
      -- natural floor of 0 and a ceiling of max(uint32_t), 4294967295. Mathematical
      -- errors such as dividing by 0 will fail during evaluation and return a value
      -- of 0. The source data for a RankExpression can be the name of an IndexField
      -- of type uint, another RankExpression or the reserved name text_relevance.
      -- The text_relevance source is defined to return an integer from 0 to 1000
      -- (inclusive) to indicate how relevant a document is to the search request,
      -- taking into account repetition of search terms in the document and
      -- proximity of search terms to each other in each matching IndexField in the
      -- document. For more information about using rank expressions to customize
      -- ranking, see the Amazon CloudSearch Developer Guide.
    , nreRankName :: !Text
      -- ^ The name of a rank expression. Rank expression names must begin with a
      -- letter and can contain the following characters: a-z (lowercase), 0-9, and
      -- _ (underscore). Uppercase letters and hyphens are not allowed. The names
      -- "body", "docid", and "text_relevance" are reserved and cannot be specified
      -- as field or rank expression names.
    } deriving (Eq, Show, Generic)

instance ToQuery NamedRankExpression

instance FromXML NamedRankExpression where
    fromXMLOptions = xmlOptions

instance ToXML NamedRankExpression where
    toXMLOptions = xmlOptions

-- | Options for literal field. Present if IndexFieldType specifies the field is
-- of type literal.
data LiteralOptions = LiteralOptions
    { loDefaultValue :: Maybe Text
      -- ^ The default value for a literal field. Optional.
    , loFacetEnabled :: Maybe Bool
      -- ^ Specifies whether facets are enabled for this field. Default: False.
    , loResultEnabled :: Maybe Bool
      -- ^ Specifies whether values of this field can be returned in search results
      -- and used for ranking. Default: False.
    , loSearchEnabled :: Maybe Bool
      -- ^ Specifies whether search is enabled for this field. Default: False.
    } deriving (Eq, Show, Generic)

instance ToQuery LiteralOptions

instance FromXML LiteralOptions where
    fromXMLOptions = xmlOptions

instance ToXML LiteralOptions where
    toXMLOptions = xmlOptions

-- | The value of an IndexField and its current status.
data IndexFieldStatus = IndexFieldStatus
    { ifsOptions :: IndexField
      -- ^ Defines a field in the index, including its name, type, and the source of
      -- its data. The IndexFieldType indicates which of the options will be
      -- present. It is invalid to specify options for a type other than the
      -- IndexFieldType.
    , ifsStatus :: OptionStatus
      -- ^ The status of an option, including when it was last updated and whether it
      -- is actively in use for searches.
    } deriving (Eq, Show, Generic)

instance ToQuery IndexFieldStatus

instance FromXML IndexFieldStatus where
    fromXMLOptions = xmlOptions

instance ToXML IndexFieldStatus where
    toXMLOptions = xmlOptions

-- | Defines a field in the index, including its name, type, and the source of
-- its data. The IndexFieldType indicates which of the options will be
-- present. It is invalid to specify options for a type other than the
-- IndexFieldType.
data IndexField = IndexField
    { ifIndexFieldName :: !Text
      -- ^ The name of a field in the search index. Field names must begin with a
      -- letter and can contain the following characters: a-z (lowercase), 0-9, and
      -- _ (underscore). Uppercase letters and hyphens are not allowed. The names
      -- "body", "docid", and "text_relevance" are reserved and cannot be specified
      -- as field or rank expression names.
    , ifIndexFieldType :: !IndexFieldType
      -- ^ The type of field. Based on this type, exactly one of the UIntOptions,
      -- LiteralOptions or TextOptions must be present.
    , ifLiteralOptions :: Maybe LiteralOptions
      -- ^ Options for literal field. Present if IndexFieldType specifies the field is
      -- of type literal.
    , ifSourceAttributes :: [SourceAttribute]
      -- ^ An optional list of source attributes that provide data for this index
      -- field. If not specified, the data is pulled from a source attribute with
      -- the same name as this IndexField. When one or more source attributes are
      -- specified, an optional data transformation can be applied to the source
      -- data when populating the index field. You can configure a maximum of 20
      -- sources for an IndexField.
    , ifTextOptions :: Maybe TextOptions
      -- ^ Options for text field. Present if IndexFieldType specifies the field is of
      -- type text.
    , ifUIntOptions :: Maybe UIntOptions
      -- ^ Options for an unsigned integer field. Present if IndexFieldType specifies
      -- the field is of type unsigned integer.
    } deriving (Eq, Show, Generic)

instance ToQuery IndexField

instance FromXML IndexField where
    fromXMLOptions = xmlOptions

instance ToXML IndexField where
    toXMLOptions = xmlOptions

-- | The current status of the search domain.
data DomainStatus = DomainStatus
    { dsCreated :: Maybe Bool
      -- ^ True if the search domain is created. It can take several minutes to
      -- initialize a domain when CreateDomain is called. Newly created search
      -- domains are returned from DescribeDomains with a false value for Created
      -- until domain creation is complete.
    , dsDeleted :: Maybe Bool
      -- ^ True if the search domain has been deleted. The system must clean up
      -- resources dedicated to the search domain when DeleteDomain is called. Newly
      -- deleted search domains are returned from DescribeDomains with a true value
      -- for IsDeleted for several minutes until resource cleanup is complete.
    , dsDocService :: Maybe ServiceEndpoint
      -- ^ The service endpoint for updating documents in a search domain.
    , dsDomainId :: !Text
      -- ^ An internally generated unique identifier for a domain.
    , dsDomainName :: !Text
      -- ^ A string that represents the name of a domain. Domain names must be unique
      -- across the domains owned by an account within an AWS region. Domain names
      -- must start with a letter or number and can contain the following
      -- characters: a-z (lowercase), 0-9, and - (hyphen). Uppercase letters and
      -- underscores are not allowed.
    , dsNumSearchableDocs :: Maybe Int
      -- ^ The number of documents that have been submitted to the domain and indexed.
    , dsProcessing :: Maybe Bool
      -- ^ True if processing is being done to activate the current domain
      -- configuration.
    , dsRequiresIndexDocuments :: !Bool
      -- ^ True if IndexDocuments needs to be called to activate the current domain
      -- configuration.
    , dsSearchInstanceCount :: Maybe Int
      -- ^ The number of search instances that are available to process search
      -- requests.
    , dsSearchInstanceType :: Maybe Text
      -- ^ The instance type (such as search.m1.small) that is being used to process
      -- search requests.
    , dsSearchPartitionCount :: Maybe Int
      -- ^ The number of partitions across which the search index is spread.
    , dsSearchService :: Maybe ServiceEndpoint
      -- ^ The service endpoint for requesting search results from a search domain.
    } deriving (Eq, Show, Generic)

instance ToQuery DomainStatus

instance FromXML DomainStatus where
    fromXMLOptions = xmlOptions

instance ToXML DomainStatus where
    toXMLOptions = xmlOptions

-- | The name of the IndexField to use for search requests issued with the q
-- parameter. The default is the empty string, which automatically searches
-- all text fields.
data DefaultSearchFieldStatus = DefaultSearchFieldStatus
    { dsfsOptions :: !Text
      -- ^ The name of the IndexField to use as the default search field. The default
      -- is an empty string, which automatically searches all text fields.
    , dsfsStatus :: OptionStatus
      -- ^ The status of an option, including when it was last updated and whether it
      -- is actively in use for searches.
    } deriving (Eq, Show, Generic)

instance ToQuery DefaultSearchFieldStatus

instance FromXML DefaultSearchFieldStatus where
    fromXMLOptions = xmlOptions

instance ToXML DefaultSearchFieldStatus where
    toXMLOptions = xmlOptions

-- | A PolicyDocument that specifies access policies for the search domain's
-- services, and the current status of those policies.
data AccessPoliciesStatus = AccessPoliciesStatus
    { apsOptions :: !Text
      -- ^ An IAM access policy as described in The Access Policy Language in Using
      -- AWS Identity and Access Management. The maximum size of an access policy
      -- document is 100 KB. Example: {"Statement": [{"Effect":"Allow", "Action":
      -- "*", "Resource": "arn:aws:cs:us-east-1:1234567890:search/movies",
      -- "Condition": { "IpAddress": { aws:SourceIp": ["203.0.113.1/32"] } }},
      -- {"Effect":"Allow", "Action": "*", "Resource":
      -- "arn:aws:cs:us-east-1:1234567890:documents/movies", "Condition": {
      -- "IpAddress": { aws:SourceIp": ["203.0.113.1/32"] } }} ] }.
    , apsStatus :: OptionStatus
      -- ^ The status of an option, including when it was last updated and whether it
      -- is actively in use for searches.
    } deriving (Eq, Show, Generic)

instance ToQuery AccessPoliciesStatus

instance FromXML AccessPoliciesStatus where
    fromXMLOptions = xmlOptions

instance ToXML AccessPoliciesStatus where
    toXMLOptions = xmlOptions

-- | Identifies the transformation to apply when copying data from a source
-- attribute.
data SourceDataFunction
    = SourceDataFunctionCopy
    | SourceDataFunctionMap
    | SourceDataFunctionTrimTitle
      deriving (Eq, Ord, Generic)

instance Hashable SourceDataFunction

instance FromText SourceDataFunction where
    fromText "Copy" = Right SourceDataFunctionCopy
    fromText "Map" = Right SourceDataFunctionMap
    fromText "TrimTitle" = Right SourceDataFunctionTrimTitle
    fromText e = fromTextFail $ "Unrecognised SourceDataFunction: " <> e

instance Read SourceDataFunction where
    readsPrec _ = fromTextRead

instance ToText SourceDataFunction where
    toText SourceDataFunctionCopy = "Copy"
    toText SourceDataFunctionMap = "Map"
    toText SourceDataFunctionTrimTitle = "TrimTitle"

instance Show SourceDataFunction where
    show = toTextShow

instance ToQuery SourceDataFunction where
    toQuery = toTextQuery

instance FromXML SourceDataFunction where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML SourceDataFunction where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The state of processing a change to an option. Possible values:
-- RequiresIndexDocuments: the option's latest value will not be visible in
-- searches until IndexDocuments has been called and indexing is complete.
-- Processing: the option's latest value is not yet visible in all searches
-- but is in the process of being activated. Active: the option's latest value
-- is completely visible. Any warnings or messages generated during processing
-- are provided in Diagnostics.
data OptionState
    = OptionStateActive
    | OptionStateProcessing
    | OptionStateRequiresIndexDocuments
      deriving (Eq, Ord, Generic)

instance Hashable OptionState

instance FromText OptionState where
    fromText "Active" = Right OptionStateActive
    fromText "Processing" = Right OptionStateProcessing
    fromText "RequiresIndexDocuments" = Right OptionStateRequiresIndexDocuments
    fromText e = fromTextFail $ "Unrecognised OptionState: " <> e

instance Read OptionState where
    readsPrec _ = fromTextRead

instance ToText OptionState where
    toText OptionStateActive = "Active"
    toText OptionStateProcessing = "Processing"
    toText OptionStateRequiresIndexDocuments = "RequiresIndexDocuments"

instance Show OptionState where
    show = toTextShow

instance ToQuery OptionState where
    toQuery = toTextQuery

instance FromXML OptionState where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML OptionState where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The type of field. Based on this type, exactly one of the UIntOptions,
-- LiteralOptions or TextOptions must be present.
data IndexFieldType
    = IndexFieldTypeLiteral
    | IndexFieldTypeText
    | IndexFieldTypeUint
      deriving (Eq, Ord, Generic)

instance Hashable IndexFieldType

instance FromText IndexFieldType where
    fromText "literal" = Right IndexFieldTypeLiteral
    fromText "text" = Right IndexFieldTypeText
    fromText "uint" = Right IndexFieldTypeUint
    fromText e = fromTextFail $ "Unrecognised IndexFieldType: " <> e

instance Read IndexFieldType where
    readsPrec _ = fromTextRead

instance ToText IndexFieldType where
    toText IndexFieldTypeLiteral = "literal"
    toText IndexFieldTypeText = "text"
    toText IndexFieldTypeUint = "uint"

instance Show IndexFieldType where
    show = toTextShow

instance ToQuery IndexFieldType where
    toQuery = toTextQuery

instance FromXML IndexFieldType where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML IndexFieldType where
    toXMLOptions = xmlOptions
    toXML        = toTextXML
