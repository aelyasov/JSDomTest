{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Genetic.ScoreJS where

import Genetic.DataJS

import Network.HTTP.Types
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Conduit (parseRequest, newManager, tlsManagerSettings, Request(..), RequestBody(..), httpLbs, responseBody, responseTimeoutMicro, HttpException(..))
import Control.Monad.IO.Class (liftIO)
import Control.Applicative
import Control.Monad
import Analysis.CFG.Data

-- import Analysis.CFG.Build
import Analysis.CFG.Fitness (computeFitness)
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graph (LEdge)
import Data.Graph.Inductive.Query.Dominators (dom)

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
import Safe
import System.Log.Logger (rootLoggerName, infoM, debugM, noticeM)


defaultResponseTimeout :: Int
defaultResponseTimeout = 60 * 10 ^ 6 -- one minute in microseconds

updateTargetPath :: Gr NLab ELab
                  -> GPath
                  -> [LEdge ELab]
                  -> [LEdge ELab]
                  -> ([LEdge ELab], [LEdge ELab]) 
updateTargetPath cfg path branches target = let
  ((targetFrom, _, _), rest) = if length target == 2 then (target!!0, target) else (target!!1, tail target)
  doms = reverse $ tail $ snd $ fromJust $ find (\(n, _) -> n == targetFrom) $ dom cfg 0
    
  findNewDomBranch :: Int -> Maybe (LEdge ELab) 
  findNewDomBranch d = find (\(n1, n2, _) -> d == n1 && n2 `notElem` path) branches
  
  findNewTarget :: [Int] -> ([LEdge ELab], [LEdge ELab])
  findNewTarget []     =  (rest, branches)
  findNewTarget (d:ds) = case findNewDomBranch d of
        Just br -> ((br:rest), Data.List.filter (\br1@(n1, n2, _) -> br1 /= br && (n1 /= d || n2 `notElem` path)) branches) 
        Nothing -> findNewTarget ds

  in  findNewTarget doms

fitnessScore :: Target -> [JSArg] -> IO (Maybe ScoredPath, Pool)
fitnessScore tg@(Target cfg targetPath)  jargs = do
  let logger = rootLoggerName
  debugM logger $ "Compute fitness score for the target path: " ++ (show targetPath)
  debugM logger $ "Compute fitness score for the JS arguments:\n" ++ (show jargs)
  man <- liftIO $ newManager tlsManagerSettings
  initReq <- parseRequest "http://localhost:7777/genetic"
  let req = initReq { method = "POST"
                    , responseTimeout = responseTimeoutMicro defaultResponseTimeout
                    , requestHeaders = [(CI.mk "Content-Type", "application/json;charset=UTF-8")]
                    , requestBody = RequestBodyLBS $ encode $ GAInput (jsargs2bstrs jargs)
                    }
      logger = rootLoggerName
  response <- (liftM responseBody $ httpLbs req man) `E.catch` \e -> putStrLn ("Caught " ++ show (e :: HttpException)) >> return "Fitness score calculation exception"

  (JSExecution trace_ distances_ loops_ enviroment_) <- return $ (fromMaybe (error $ "fitnessScore in response" ++ (show response)) . decode) $ response 

  noticeM logger $ "Execution trace: "    ++ show trace_
  infoM logger   $ "Branch distances: "   ++ show distances_
  infoM logger   $ "Loop iteration map: " ++ show loops_
  infoM logger   $ "New Enviroment: "     ++ show enviroment_

  let loopIterMap = map2IntMap loops_
      newCPool = ( Nothing,
                   Nothing,
                   Nothing,
                   ( Nothing
                   , Just $ getIdsJS enviroment_
                   , Just $ getNamesJS enviroment_
                   , Just $ getClassesJS enviroment_)
                 )
      newPool = Pool [] newCPool []
      
  fitnessVals <- mapM (\(from, to, _) -> computeFitness cfg loopIterMap to trace_ distances_ ) targetPath
 

  noticeM logger $ "Final Fitness value is equal to: " ++ (show fitnessVals)
  setCondBreakPoint
  
  return (Just $ ScoredPath fitnessVals trace_, newPool) 


-- mkTestCFG "./Genetic/safeAdd.js" >>= \g -> fitnessScore (Target g 9) [DomJS test_html, StringJS "iframe"]
-- test_fitnessScore = fitnessScore (Target G.empty 9) [DomJS test_html, StringJS "iframe"]

-- test_html = "<!DOCTYPE HTML>\n<html><head><title>Title</title><base href=\".\" target=\"_blank\"></head><body itemscope=\"\" itemtype=\"http://schema.org/WebPage\"><h1><a></a>A2A2</h1><h1></h1></body></html>"


-- | the function has to be encompassed by parencess to be evaluated by JavaScript
test_fun = "function safeAdd(frameid) {\n    console.log(this);\n    var iframe = document.createElement(\"iframe\");\n    var anchor = document.getElementById(\"node\");\n    var frame = document.getElementById(frameid);\n    iframe.setAttribute(\"id\", frameid);\n    if (frame) { \n        frame.parentNode.removeChild(frame); \n    } else {\n\tiframe.appendChild(anchor);\n    }\n}\n"

test_html = "<!DOCTYPE HTML><html><head></head><body><div id=\"iframe\">IFrame</div><div id=\"node\">Node</div></body></html>"




