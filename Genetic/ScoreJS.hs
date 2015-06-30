{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Genetic.ScoreJS where

import Genetic.DataJS

import Network.HTTP.Types
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Conduit
import Control.Monad.IO.Class (liftIO)
import Control.Applicative
import Control.Monad

import Analysis.CFG.Build
import Data.Graph.Inductive.PatriciaTree
import qualified Data.Graph.Inductive.Graph as G

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe
import Data.List
import Data.Aeson

import Debug.Trace


fitnessScore :: Target -> [JSArg] -> IO (Maybe Double)
fitnessScore (Target cfg loc)  jargs = do
  man <- liftIO $ newManager conduitManagerSettings
  initReq <- parseUrl "http://localhost:8888"
  let req = initReq { method = "POST"
                    , requestHeaders = [(CI.mk "Content-Type", "text/html;charset=UTF-8")]
                    , queryString = "genetic=true"
                    , requestBody = RequestBodyLBS $ encode (GAInput (jsargs2bstrs jargs))
                    }
  (JSExecution trace_ distances_) <- liftM (fromMaybe (error "fitnessScore in response") . decode . responseBody) $ httpLbs req man
  let (aprLevel, branch) = approachLevel cfg loc trace_
      brLevel = trace ("branch: " ++ show branch ++ "; distances_: " ++ show distances_) $ maybe (error "branch label isn't found") getBrDist $ find ((branch==) . getBrLab) distances_
      normBrLevel = branchDistNormalize brLevel
      fitnessVal  = fromIntegral aprLevel + normBrLevel              
  return $ Just fitnessVal
  -- return $ Just $ (read $ C.unpack $ responseBody response :: Double)

branchDistNormalize :: Int -> Double
branchDistNormalize b = fromIntegral b / fromIntegral (b+1)


-- mkTestCFG "./Genetic/safeAdd.js" >>= \g -> fitnessScore (Target g 9) [DomJS test_html, StringJS "iframe"]
test_fitnessScore = fitnessScore (Target G.empty 9) [DomJS test_html, StringJS "iframe"]

-- test_html = "<!DOCTYPE HTML>\n<html><head><title>Title</title><base href=\".\" target=\"_blank\"></head><body itemscope=\"\" itemtype=\"http://schema.org/WebPage\"><h1><a></a>A2A2</h1><h1></h1></body></html>"


-- | the function has to be encompassed by parencess to be evaluated by JavaScript
test_fun = "function safeAdd(frameid) {\n    console.log(this);\n    var iframe = document.createElement(\"iframe\");\n    var anchor = document.getElementById(\"node\");\n    var frame = document.getElementById(frameid);\n    iframe.setAttribute(\"id\", frameid);\n    if (frame) { \n        frame.parentNode.removeChild(frame); \n    } else {\n\tiframe.appendChild(anchor);\n    }\n}\n"

test_html = "<!DOCTYPE HTML><html><head></head><body><div id=\"iframe\">IFrame</div><div id=\"node\">Node</div></body></html>"

data GAInput = GAInput 
    { jsFunArgs :: Text
    } deriving Show


instance ToJSON GAInput where
    -- to invoke the 'encode' function apply this flag ":set -XOverloadedStrings"
    toJSON (GAInput jsFunArgs) = 
        object ["jsFunArgs" .= jsFunArgs]


data JSExecution = JSExecution { traceJS      :: [Int]
                               , branchDistJS :: [BranchDist]
                               } deriving Show

data BranchDist = BranchDist { getBrLab  :: Int 
                             , getBrDist :: Int} deriving Show


instance FromJSON BranchDist where
    parseJSON jsn = do
       [x,y] <- parseJSON jsn
       return $ BranchDist x y


instance FromJSON JSExecution where
    parseJSON (Object v) = JSExecution <$> (v .: "_trace_") <*> (v .: "_branchDistance_" >>= parseJSON)
    parseJSON x = fail $ "unexpected json: " ++ show x


