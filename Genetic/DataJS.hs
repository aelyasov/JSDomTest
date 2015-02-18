module Genetic.DataJS where

import Data.ByteString.Lazy (ByteString)


import Html5C.Tags

data JSArg = IntJS Int
           | StringJS String
           | BoolJS Bool
           | DomJS ByteString
             deriving (Show, Read, Ord, Eq)

type Target = String

data JSType = JS_INT | JS_STRING | JS_BOOL | JS_DOM
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
