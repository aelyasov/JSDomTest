{-# LANGUAGE OverloadedStrings #-}

module Genetic.DataJS where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding

import Analysis.CFG.Build
import Data.Graph.Inductive.PatriciaTree

import Html5C.Tags

data JSArg = IntJS Int
           | StringJS String
           | BoolJS Bool
           | DomJS ByteString
             deriving (Show, Read, Ord, Eq)


data Target = Target { jsCFG :: Gr NLab ELab, murSrc :: SLab }


data JSType = JS_INT | JS_FLOAT | JS_STRING | JS_BOOL | JS_DOM
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


jsargs2bstrs :: [JSArg] -> Text
jsargs2bstrs args = T.intercalate "<|>" $ map jsarg2bstr args
-- "[" `T.append`
--         ( T.intercalate ", " $ map (\a -> "\"" `T.append` 
--                                           (jsarg2bstr a) `T.append` 
--                                     "\"") args
--         ) `T.append` "]"
        


