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

updateTargetPath :: GPath -> [LEdge ELab] -> [(Int, LEdge ELab)] -> [LEdge ELab]
updateTargetPath execPath targetPath labBranches =
  let branches = Data.List.map snd $ trace ("labBranches: " ++ show labBranches) labBranches

      (prefix, suffix) = splitAt (length targetPath - 2) targetPath

      targetBranch@(tgFrom, tgTo, _) = head $ trace ("suffix: " ++ show suffix) suffix
      prevTargetBranch@(prTgFrom, prTgTo, _) = last prefix
      prefixExecPath = tailNote "Last branch is not found" $ snd $ break (prTgFrom==) execPath
      
      findBranchToNode brs nd = find (\(from, _, _) -> nd == from) brs 
      negatePrecedBranch (prevBranch, prevNode, _) =
        fromJust $ find ( \(from, to, _) ->
                            (from == prevBranch) &&
                            (to /= prevNode)) branches

      findPrecedBranch :: GPath -> [LEdge ELab]
      findPrecedBranch [] = targetPath
      findPrecedBranch (node:nodes) =
        case findBranchToNode branches node of
          Just br@(brFrom, brTo, _) | brFrom /= tgFrom -> prefix ++ [negatePrecedBranch br] ++ suffix
                                    | otherwise        -> targetPath
          Nothing                                      -> findPrecedBranch nodes                       
  in  if Data.List.null $ trace ("prefix: " ++ show prefix) prefix
      then findPrecedBranch execPath
      else findPrecedBranch $ trace ("prefixExecPath: " ++ show prefixExecPath) prefixExecPath


fitnessScore :: Target -> [JSArg] -> IO (Maybe ScoredPath, (JSSig, JSCPool))
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
      updatedJSCPool = ([], ( Nothing,
                              Nothing,
                              Nothing,
                              ( Nothing
                              , Just $ getIdsJS enviroment_
                              , Just $ getNamesJS enviroment_
                              , Just $ getClassesJS enviroment_)))
      
  fitnessVals <- mapM (\(from, to, _) -> computeFitness cfg loopIterMap to trace_ distances_ ) targetPath
 

  noticeM logger $ "Final Fitness value is equal to: " ++ (show fitnessVals)
  setCondBreakPoint
  
  return (Just $ ScoredPath fitnessVals trace_, updatedJSCPool) 


-- mkTestCFG "./Genetic/safeAdd.js" >>= \g -> fitnessScore (Target g 9) [DomJS test_html, StringJS "iframe"]
-- test_fitnessScore = fitnessScore (Target G.empty 9) [DomJS test_html, StringJS "iframe"]

-- test_html = "<!DOCTYPE HTML>\n<html><head><title>Title</title><base href=\".\" target=\"_blank\"></head><body itemscope=\"\" itemtype=\"http://schema.org/WebPage\"><h1><a></a>A2A2</h1><h1></h1></body></html>"


-- | the function has to be encompassed by parencess to be evaluated by JavaScript
test_fun = "function safeAdd(frameid) {\n    console.log(this);\n    var iframe = document.createElement(\"iframe\");\n    var anchor = document.getElementById(\"node\");\n    var frame = document.getElementById(frameid);\n    iframe.setAttribute(\"id\", frameid);\n    if (frame) { \n        frame.parentNode.removeChild(frame); \n    } else {\n\tiframe.appendChild(anchor);\n    }\n}\n"

test_html = "<!DOCTYPE HTML><html><head></head><body><div id=\"iframe\">IFrame</div><div id=\"node\">Node</div></body></html>"




