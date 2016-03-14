{-# LANGUAGE OverloadedStrings #-}

module Main where

-- | System
import System.Environment (getArgs) -- pass arguments to the program

-- | Parsing
import Language.ECMAScript3.Parser
import Language.ECMAScript3.Syntax
import qualified Language.ECMAScript3.PrettyPrint as JSP
import Data.Graph.Inductive.Dot (fglToDotString, showDot)
--import Data.Default.Class (def) -- create default value of the SourcePos datatype
import Data.Default
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Text as T
import Data.Text (Text)
import Data.Aeson
import Data.List

-- | Monads
import Control.Monad (msum, liftM)
import Control.Monad.State hiding (mapM)

-- | My imports
import Util.ParseJSSignature
import Analysis.CFG.Instrument (instrScript)
import Analysis.CFG.Build (enrichCollectedEdges, getAllBranches)
import Analysis.CFG.Label (assignUniqueIdsSt)
import Analysis.CFG.Transform (transfromJS)
import Analysis.Static

-- | Networking
import Network.HTTP.Conduit
import qualified Data.CaseInsensitive as CI


 -- | Genetics
import Genetic.Instance (Algorithm, runAlgorithm)
import Genetic.DataJS
import Genetic.ScoreJS

-- | Mutations
import Mutation.Dom.Operators

-- | Graphs
import Data.Graph.Inductive


-- | Debug
import Debug.Trace
import System.IO (stdout, Handle)
import System.Log.Logger (rootLoggerName, getRootLogger, setHandlers, updateGlobalLogger, Priority(..), debugM, infoM, noticeM, setLevel)
import System.Log.Handler.Simple (streamHandler, GenericHandler)
import System.Log.Handler (setFormatter)
import System.Log.Formatter
import Data.Configurator (load, Worth(..), require)
import System.Process (system)


readMainConfig :: IO (Algorithm, Priority)
readMainConfig = do
  config   <- load [ Required "jsdomtest.cfg"]
  algType  <- liftM read $ require config "algorithm.type"
  logLevel <- liftM read $ require config "logging.level"
  return (algType, logLevel)


setCommonFormatter x =
  let f = simpleLogFormatter "[$time $loggername $prio] $msg"
  in  setFormatter x f


-- | run main: :main "nodeCovertest.js"
main :: IO ()    
main = do
  (algType, logLevel) <- readMainConfig
  -- Logger Configuration
  myStreamHandler <- streamHandler stdout logLevel
  let myStreamHandler' = setCommonFormatter myStreamHandler
  let logger = rootLoggerName
  updateGlobalLogger logger (setLevel logLevel)
  updateGlobalLogger logger (setHandlers [myStreamHandler'])

  (jsFile:_) <- getArgs
  debugM logger $ "The following JS file is given for analysis: " ++ (show jsFile)
  jsFun <- liftM transfromJS $ parseFromFile jsFile
  jsSig <- parseJSSignature jsFile
  print jsSig
  let jsLabFun@(Script l jsLabStms) = fst $ flip runState 0 $ assignUniqueIdsSt  jsFun
      jsFunCFG      = uncurry mkGraph $ enrichCollectedEdges jsLabStms
      branches      = getAllBranches jsFunCFG
      constPool     = collectConstantInfoJS jsLabFun
      jsLabFunInstr = instrScript jsLabFun
  noticeM logger "Function given for analysis has the following CFG:\n"
  system $ "echo " ++ (show $ showDot $ fglToDotString jsFunCFG) ++ " | graph-easy --as_ascii"
  noticeM logger $ "Instrumented version of the analysed function:\n" ++ (show $ JSP.prettyPrint jsLabFunInstr)
  noticeM logger $ "The following branches have to be covered: " ++ (show branches)
  noticeM logger $ "Initial pool data: " ++ (show constPool)
  -- getLine
  request <- parseUrl "http://localhost:7777"
  man <- liftIO $ newManager tlsManagerSettings

  let reqInit = request { method = "POST"
                        , requestHeaders = [(CI.mk "Content-Type", "text/html;charset=UTF-8")]
                        , queryString = "init=true"
                        , requestBody = RequestBodyLBS $ encode (InitData (T.pack $ show $ JSP.prettyPrint jsLabFunInstr))
                        }
  initResp <- httpLbs reqInit man
  debugM logger $ show initResp 

  mapM_  (\(i, branch) -> killJSMutationGenetic algType man i (Target jsFunCFG branch)  (jsSig, constPool)) $ zip [1..] branches


killJSMutationGenetic :: Algorithm -> Manager -> Int -> Target -> (JSSig, JSCPool) -> IO ()
killJSMutationGenetic alg man mutN target pool = do
  let logger = rootLoggerName
  putStrLn $ replicate 70 '-'
  noticeM logger $ "Branch : #" ++ (show mutN) ++ " -> " ++ (show $ mutSrc target)
  noticeM logger $ "Initial pool data: " ++ (show pool)
  request <- parseUrl "http://localhost:7777"
  let reqMut = request { method = "POST"
                       , requestHeaders = [(CI.mk "Content-Type", "text/html;charset=UTF-8")]
                       , queryString = "mutation=true"
                       , requestBody = RequestBodyLBS $ encode (MutData (T.pack $ show mutN))
                       }
  mutResp <- httpLbs reqMut man
  debugM logger $ show mutResp
  jsArgs <- runAlgorithm alg target pool
  -- for the integration testing purpose runGenetic has been replaced with fitnessScore
  -- mkTestCFG "./Genetic/safeAdd.js" >>= \g -> fitnessScore (Target g 9) [DomJS test_html, StringJS "iframe"]
  -- let jsArgs = [DomJS test_html, StringJS "iframe"]
  noticeM logger $ "Best entity (GA): " ++ (show jsArgs)

  let reqExec = request { method = "POST"
                        , requestHeaders = [(CI.mk "Content-Type", "text/html;charset=UTF-8")]
                        , queryString = "execute=true"
                        , requestBody = RequestBodyLBS $ encode (ArgData $ jsargs2bstrs jsArgs)
                        }
  execResp <- httpLbs reqExec man
  debugM logger $ show execResp
  getLine
  return ()



-- | The generateJSMutations function given a function produces all possible mutations out of existing. It returns the mutated programs together with the source positions to which the mutation operators were applied.x
generateJSMutations :: JavaScript SourcePosLab -> [(Int, Mutation SourcePosLab)]
generateJSMutations js = zip [1..] $ concatMap (\m -> m js) domMutations


data InitData = InitData { jsFun :: Text }

instance ToJSON InitData where
    toJSON (InitData jsFun) = object [ "jsFun" .= jsFun ]

data MutData = MutData { mutN :: Text }

instance ToJSON MutData where
    toJSON (MutData mutN) = object [ "mutN" .= mutN ]


data ArgData = ArgData { jsArgs :: Text }

instance ToJSON ArgData where
    toJSON (ArgData jsArgs) = object [ "jsArgs" .= jsArgs ]


