module JS.Types where

data JSArg = IntJS Int
           | StringJS String
           | BoolJS Bool
           | DomJS String
             deriving (Show, Read, Ord, Eq)

type Target = String

data JSType = JS_INT | JS_STRING | JS_BOOL | JS_DOM
            deriving Show

type JSSig = [JSType]

type JSInts    = [Int]
type JSStrings = [String]
type JSTags    = [String]
type JSIds     = [String]
type JSNames   = [String]
type JSDoms    = (JSTags, JSIds, JSNames)

type JSCPool = (JSInts, JSStrings, JSDoms)
