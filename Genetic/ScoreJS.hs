{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Genetic.ScoreJS where

import Genetic.DataJS

import Network.HTTP.Types
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Conduit
import Control.Monad.IO.Class (liftIO)
import Control.Applicative
import Control.Monad
import Analysis.CFG.Data

-- import Analysis.CFG.Build
import Analysis.CFG.Fitness (computeCfgLevel, computeRealCfgLevelOne, distanceToExit)
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graph (LEdge)

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Text as T
import Data.Map
import Data.Text (Text)
import Data.Maybe
import Data.List
import Data.Aeson
import qualified Control.Exception as E
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Debug.Trace
import Util.Debug (setCondBreakPoint)
import System.Log.Logger (rootLoggerName, infoM, debugM, noticeM)


fitnessScore :: Target -> [JSArg] -> IO (Maybe Double, (JSSig, JSCPool))
fitnessScore tg@(Target cfg loc@(from, to, _))  jargs = do
  let logger = rootLoggerName
  debugM logger $ "Compute fitness score for the target: " ++ (show loc)
  debugM logger $ "Compute fitness score for the JS arguments:\n" ++ (show jargs)
  man <- liftIO $ newManager tlsManagerSettings
  initReq <- parseUrl "http://localhost:7777/genetic"
  let req = initReq { method = "POST"
                    , requestHeaders = [(CI.mk "Content-Type", "application/json;charset=UTF-8")]
                    , requestBody = RequestBodyLBS $ encode $ GAInput (jsargs2bstrs jargs)
                    }
      logger = rootLoggerName
      exitLoc = (-1, -1, "")      
  response <- (liftM responseBody $ httpLbs req man) `E.catch` \e -> putStrLn ("Caught " ++ show (e :: HttpException)) >> return "Fitness score calculation exception"

  (JSExecution trace_ distances_ loops_ enviroment_) <- return $ (fromMaybe (error $ "fitnessScore in response" ++ (show response)) . decode) $ response 

  noticeM logger $ "Execution trace: "    ++ (show trace_)
  infoM logger   $ "Branch distances: "   ++ (show distances_)
  infoM logger   $ "Loop iteration map: " ++ (show loops_)
  infoM logger   $ "New Enviroment: "     ++ (show enviroment_)
  
  infoM logger $ "Computing approach level for the location: " ++ (show loc) ++ " along the path: " ++ (show trace_)
  fitnessVal1 <- if (loc == exitLoc)
                 then return 0
                 else computeFitness cfg (map2IntMap loops_) to trace_ distances_
  -- infoM logger $ "Computing approach level for the location: " ++ (show exitLoc) ++ " along the path: " ++ (show trace_)
  fitnessVal2 <- computeFitness cfg (map2IntMap loops_) (-1) trace_ distances_
  -- fitnessVal2 <- return $ fromIntegral $ distanceToExit cfg trace_
  -- infoM logger $ "FitnessVal2: " ++ (show fitnessVal2)
  let fitnessVal = fitnessVal1 + 0.001 * fitnessVal2
  -- let fitnessVal = fitnessVal1
  noticeM logger $ "Final Fitness value is equal to: " ++ (show fitnessVal)

  setCondBreakPoint
  
  return (Just fitnessVal, ([], ([], [], ([], getIdsJS enviroment_, getNamesJS enviroment_, getClassesJS enviroment_)))) 

  
branchDistNormalize :: Int -> Double
branchDistNormalize b = fromIntegral b / fromIntegral (b+1)


computeFitness :: Gr NLab ELab -> LoopIterationMap -> SLab -> GPath -> [BranchDist] -> IO Double
computeFitness cfg loopIterMap target path disatnces = do
  let (cfgLevel, problemNode) = computeRealCfgLevelOne cfg loopIterMap path target
      branchLevel      = maybe 0 getBrDist $ find ((problemNode==) . getBrLab) disatnces
      normBrLevel      = branchDistNormalize branchLevel
      isException      = last path == (-100) && ((last $ init path) == problemNode)
      problemNodeLevel = if isException then 1 else (0.5 * normBrLevel)
      fitnessVal       = if target `elem` path then 0 else fromIntegral (cfgLevel - 1) + problemNodeLevel
      logger           = rootLoggerName
  infoM logger $ "ProblemNode: " ++ (show problemNode)
  infoM logger $ "CfgLevel: " ++ (show cfgLevel)
  infoM logger $ "ProblemNodeLevel: " ++ (show problemNodeLevel)
  return fitnessVal


-- mkTestCFG "./Genetic/safeAdd.js" >>= \g -> fitnessScore (Target g 9) [DomJS test_html, StringJS "iframe"]
-- test_fitnessScore = fitnessScore (Target G.empty 9) [DomJS test_html, StringJS "iframe"]

-- test_html = "<!DOCTYPE HTML>\n<html><head><title>Title</title><base href=\".\" target=\"_blank\"></head><body itemscope=\"\" itemtype=\"http://schema.org/WebPage\"><h1><a></a>A2A2</h1><h1></h1></body></html>"


-- | the function has to be encompassed by parencess to be evaluated by JavaScript
test_fun = "function safeAdd(frameid) {\n    console.log(this);\n    var iframe = document.createElement(\"iframe\");\n    var anchor = document.getElementById(\"node\");\n    var frame = document.getElementById(frameid);\n    iframe.setAttribute(\"id\", frameid);\n    if (frame) { \n        frame.parentNode.removeChild(frame); \n    } else {\n\tiframe.appendChild(anchor);\n    }\n}\n"

test_html = "<!DOCTYPE HTML><html><head></head><body><div id=\"iframe\">IFrame</div><div id=\"node\">Node</div></body></html>"




