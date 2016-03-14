{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Genetic.ScoreJS where

import Genetic.DataJS

import Network.HTTP.Types
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Conduit
import Control.Monad.IO.Class (liftIO)
import Control.Applicative
import Control.Monad
import Analysis.CFG.Data (NLab, ELab, GPath)

-- import Analysis.CFG.Build
import Analysis.CFG.Fitness (computeCfgLevel)
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graph (LEdge)

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe
import Data.List
import Data.Aeson
import qualified Control.Exception as E

import Debug.Trace
import System.Log.Logger (rootLoggerName, infoM, debugM, noticeM)


fitnessScore :: Target -> [JSArg] -> IO (Maybe Double)
fitnessScore tg@(Target cfg loc@(from, to, _))  jargs = do
  let logger = rootLoggerName
  debugM logger $ "Compute fitness score for the target: " ++ (show tg)
  debugM logger $ "Compute fitness score for the JS arguments:\n" ++ (show jargs)
  man <- liftIO $ newManager tlsManagerSettings
  initReq <- parseUrl "http://localhost:7777"
  let req = initReq { method = "POST"
                    , requestHeaders = [(CI.mk "Content-Type", "text/html;charset=UTF-8")]
                    , queryString = "genetic=true"
                    , requestBody = RequestBodyLBS $ encode (GAInput (jsargs2bstrs jargs))
                    }
      logger = rootLoggerName
      exitLoc = (-1, -1, "")      
  response <- (liftM responseBody $ httpLbs req man) `E.catch` \e -> putStrLn ("Caught " ++ show (e :: HttpException)) >> return "Foo"
  (JSExecution trace_ distances_) <- return $ (fromMaybe (error "fitnessScore in response") . decode) $ response 

  infoM logger $ "Execution trace: " ++ (show trace_)
  debugM logger $ "Branch distances: " ++ (show distances_)
  
  debugM logger $ "Computing approach level for the location: " ++ (show loc) ++ " along the path: " ++ (show trace_)
  fitnessVal1 <- if (loc == exitLoc) then return 0 else computeFitness cfg loc trace_ distances_
  debugM logger $ "Computing approach level for the location: " ++ (show exitLoc) ++ " along the path: " ++ (show trace_)
  fitnessVal2 <- computeFitness cfg exitLoc trace_ distances_
  let fitnessVal = fitnessVal1 + fitnessVal2
  noticeM logger $ "Final Fitness value is equal to: " ++ (show fitnessVal)
  return $ Just fitnessVal 
  
branchDistNormalize :: Int -> Double
branchDistNormalize b = fromIntegral b / fromIntegral (b+1)


computeFitness :: Gr NLab ELab -> LEdge ELab -> GPath -> [BranchDist] -> IO Double
computeFitness cfg (from, to, _) path disatnces = do
  let (cfgLevel, problemNode, isException) = computeCfgLevel cfg from path
      branchLevel      = maybe 0 getBrDist $ find ((problemNode==) . getBrLab) disatnces
      normBrLevel      = branchDistNormalize branchLevel
      problemNodeLevel = if (to `elem` path) then 0 else (if isException then 1 else (0.5 * normBrLevel))
      fitnessVal       = fromIntegral cfgLevel + problemNodeLevel
      logger           = rootLoggerName
  infoM logger $ "CFG level is equal to "           ++ (show cfgLevel) ++ " for the problemNode " ++ (show problemNode)
  infoM logger $ "The problem node is exceptional " ++ (show isException)
  infoM logger $ "Problem Node Level is equal to "  ++ (show problemNodeLevel)    
  infoM logger $ "Fitness value is equal to "       ++ (show fitnessVal) ++ " for the location " ++ (show to)
  -- getLine
  return fitnessVal   


-- mkTestCFG "./Genetic/safeAdd.js" >>= \g -> fitnessScore (Target g 9) [DomJS test_html, StringJS "iframe"]
-- test_fitnessScore = fitnessScore (Target G.empty 9) [DomJS test_html, StringJS "iframe"]

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


data JSExecution = JSExecution { traceJS      :: GPath
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


