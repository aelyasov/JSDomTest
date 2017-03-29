{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Genetic.DataJS where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Map (Map)
import Data.Monoid ((<>))
import GHC.Generics

import Analysis.CFG.Data
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graph (LEdge)
import Text.XML.Pretty (prettyHtmlByteString)

import Html5C.Tags

data JSArg = IntJS Int
           | FloatJS Float
           | StringJS String
           | BoolJS Bool
           | DomJS ByteString
           | ArrayJS [JSArg]
           deriving (Read, Ord, Eq)

instance Show JSArg where
  show (IntJS i)    = show i
  show (FloatJS f)  = show f
  show (StringJS s) = show s
  show (BoolJS b)   = show b
  show (DomJS s)    = prettyHtmlByteString s
  show (ArrayJS ar) = show ar
  
data Target = Target { jsCFG :: Gr NLab ELab, mutSrc :: LEdge ELab } deriving Show

data JSType = JS_INT
            | JS_FLOAT
            | JS_STRING
            | JS_BOOL
            | JS_DOM
            | JS_ARRAY JSType
            deriving Show

type JSSig = [JSType]

type JSInts    = [Int]
type JSStrings = [String]
type JSTags    = [HTML_TAG]
type JSIds     = [String]
type JSNames   = [String]
type JSClasses = [String]
type JSDoms    = (JSTags, JSIds, JSNames, JSClasses)

type JSCPool = (JSInts, JSStrings, JSDoms)

getJSDoms :: JSCPool -> JSDoms
getJSDoms (_, _, doms) = doms

getJSInts :: JSCPool -> JSInts
getJSInts (ints, _, _) = ints

getJSStrings :: JSCPool -> JSStrings
getJSStrings (_, strings, _) = strings

getJSIds :: JSCPool -> JSIds
getJSIds (_, _, (_, ids, _, _)) = ids

getJSClasses :: JSCPool -> JSClasses
getJSClasses (_, _, (_, _, _, classes)) = classes

isPrimJSArg :: JSArg -> Bool
isPrimJSArg (DomJS _ ) = False
isPrimJSArg _           = True

isDomJSArg :: JSArg -> Bool
isDomJSArg (DomJS _) = True
isDomJSArg _         = False


jsarg2bstr :: JSArg -> Text
jsarg2bstr (IntJS i)    = T.pack $ show i
jsarg2bstr (StringJS s) = T.pack s
jsarg2bstr (BoolJS b)   = case b of
                            True  -> T.pack "true"
                            False -> T.pack "false"
jsarg2bstr (DomJS bs)   = decodeUtf8 $ B.toStrict bs
jsarg2bstr (ArrayJS ar) = T.pack $ show ar


jsargs2bstrs :: [JSArg] -> Text
jsargs2bstrs args = T.intercalate "<|>" $ map jsarg2bstr args

data GAInput = GAInput 
    { jsFunArgs :: Text
    } deriving Show


data JSExecution =
  JSExecution { traceJS       :: GPath
              , branchDistJS  :: [BranchDist]
              , loopMapJS     :: Map String Int
              , environmentJS :: JSEnviroment  
              } deriving (Eq, Show, Generic)
                         

data JSEnviroment =
  JSEnviroment { getTagsJS    :: [String]
               , getNamesJS   :: [String]
               , getIdsJS     :: [String]  
               , getClassesJS :: [String]
               , getSelectors :: [String]
               } deriving (Show, Generic, Eq)


data BranchDist =
  BranchDist { getBrLab  :: Int 
             , getBrDist :: Int
             } deriving (Eq, Generic)

instance Show BranchDist where
  show (BranchDist lab dist) = show (lab, dist)


instance ToJSON GAInput where
    -- to invoke the 'encode' function apply this flag ":set -XOverloadedStrings"
    toJSON (GAInput jsFunArgs) = 
        object ["jsFunArgs" .= jsFunArgs]


instance FromJSON GAInput where
  parseJSON (Object v) = GAInput <$> (v .: "jsFunArgs")



instance FromJSON BranchDist where
  parseJSON (Object v) = BranchDist
                         <$> (v .: "label")
                         <*> (v .: "distance")
  parseJSON x = fail $ "unexpected json: " ++ show x


instance ToJSON BranchDist where
  toEncoding  (BranchDist label distance) =
    pairs ("label" .= label <> "distance" .= distance)


instance FromJSON JSEnviroment where
  parseJSON (Object v) = JSEnviroment
                         <$> (v .: "tags")
                         <*> (v .: "names")
                         <*> (v .: "ids")
                         <*> (v .: "classes")
                         <*> (v .: "selectors")
  parseJSON x = fail $ "unexpected json: " ++ show x


instance ToJSON JSEnviroment where
  toEncoding (JSEnviroment tags names ids classes selectors) =
    pairs (   "tags"      .= tags
           <> "names"     .= names
           <> "ids"       .= ids
           <> "classes"   .= classes
           <> "selectors" .= selectors)

    
instance FromJSON JSExecution where
    parseJSON (Object v) = JSExecution
                           <$> (v .: "trace")
                           <*> (v .: "branchDistance")
                           <*> (v .: "loopMap")
                           <*> (v .: "environment")
    parseJSON x = fail $ "unexpected json: " ++ show x

    
instance ToJSON JSExecution where
   toEncoding (JSExecution traces brDist loopMap env) =
    pairs (   "trace" .= traces
           <> "branchDistance" .= brDist
           <> "loopMap" .= loopMap
           <> "environment" .= env
          )


