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
import Analysis.CFG.Data (ELab, NLab, EnumLEdge)
import Analysis.Static

-- | Networking
import Network.HTTP.Conduit (parseRequest, newManager, tlsManagerSettings, Request(..), RequestBody(..), httpLbs, responseBody, responseTimeoutMicro, HttpException(..), Manager(..), Response(..))
import Network.HTTP.Types.Status (statusMessage)
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
import Util.Debug (setCondBreakPoint)
import System.IO (stdout, Handle)
import System.Log.Logger (rootLoggerName, getRootLogger, setHandlers, updateGlobalLogger, Priority(..), debugM, infoM, noticeM, setLevel, errorM, criticalM, removeAllHandlers)
import System.Log.Handler.Simple (streamHandler, GenericHandler(..))
import System.Log.Handler (setFormatter, close)
import System.Log.Formatter
import Data.Configurator (load, Worth(..), require, display)
import System.Process (system)
import System.IO (openFile, IOMode(..), hClose)
import System.Directory (createDirectoryIfMissing)
import Control.Exception (onException)


readMainConfig :: IO (Algorithm, Priority, Bool, Int, Int)
readMainConfig = do
  config   <- load [Required "jsdomtest.cfg"]
  -- display config 
  algType          <- liftM read $ require config "algorithm.type"
  logLevel         <- liftM read $ require config "logging.level"
  isBreakEnabled   <- require config "breakpoint.enabled"
  iterateTotal     <- require config "iterate.total"
  coverageBranches <- require config "coverage.branches"
  return (algType, logLevel, isBreakEnabled, iterateTotal, coverageBranches)


fileHandlerReadWrite :: FilePath -> Priority -> IO (GenericHandler Handle)
fileHandlerReadWrite fp pri = do
  -- createDirectoryIfMissing True fp
  h  <- openFile fp WriteMode
  sh <- streamHandler h pri
  return (sh{closeFunc = hClose})



parseInputAndSetupLogger :: Priority -> IO String
parseInputAndSetupLogger logLevel = do
  args <- getArgs
  (handler, inFile) <- case args of
    [jsFile]          -> liftM (\h -> (h, jsFile)) $ streamHandler stdout logLevel
    [jsFile, outFile] -> liftM (\h -> (h, jsFile)) $ fileHandlerReadWrite outFile logLevel 
    otherwise         -> error $ "Wrong number of input arguments: " ++ (show $ length args) 
  let formatter = simpleLogFormatter "[$time $loggername $prio] $msg"
      handler'  = setFormatter handler formatter
      logger    = rootLoggerName
  updateGlobalLogger logger $ setLevel logLevel
  updateGlobalLogger logger $ setHandlers [handler']
  return inFile


main :: IO ()    
main = main' `onException` removeAllHandlers


-- | run main: :main "nodeCovertest.js"
main' :: IO ()    
main' = do
  (algType, logLevel, isBreak, iterateTotal, coverageBranches) <- readMainConfig
  -- Parse input and set logger
  jsFile <- parseInputAndSetupLogger logLevel
  let logger = rootLoggerName
  criticalM logger $ "Test generation strategy: " ++ show algType
  criticalM logger $ "Number of performed generation runs: " ++ show iterateTotal 
  debugM logger $ "JavaScript file given for the analysis: " ++ show jsFile
  jsFileContent <- parseFromFile jsFile
  jsSig <- parseJSSignature jsFile  
  debugM logger $ "JS function signature: " ++ show jsSig
  let jsFun         = transfromJS jsFileContent               
      (Script l (jsLabFun':jsLabStms)) = fst $ flip runState 0 $ assignUniqueIdsSt jsFun
      jsLabFun      = Script l [jsLabFun'] 
      jsFunCFG      = uncurry mkGraph $ enrichCollectedEdges [jsLabFun']
      branches      = getAllBranches jsFunCFG
      labBranches   = zip [1..] branches
      cpool     = collectConstantInfoJS jsLabFun
      jsLabFunInstr@(Script _ [jsLabFunInstrStmt]) = instrScript jsLabFun
      jsLabFunInstrFile = Script l (jsLabFunInstrStmt:jsLabStms)
  noticeM logger "The function has the following CFG:\n"
  when isBreak $ void $ system $ "echo " ++ (show $ showDot $ fglToDotString jsFunCFG) ++ " | graph-easy --as_ascii" 
  noticeM logger $ "Instrumented version of the analysed function:\n" ++ (show $ JSP.prettyPrint jsLabFunInstr)
  noticeM logger $ "The following branches have to be covered: " ++ show branches
  noticeM logger $ "Initial constant pool data: " ++ show cpool

  setCondBreakPoint
  
  -- | Send initial data to the client
  noticeM logger $ "Initialise first request to send instrumented function under test:"
  request <- parseRequest "http://localhost:7777/init"
  man     <- liftIO $ newManager tlsManagerSettings
  let reqInit = request { method = "POST"
                        , requestHeaders = [(CI.mk "Content-Type", "text/html;charset=UTF-8")]
                        , requestBody = RequestBodyLBS
                                        $ encode
                                        $ InitData (T.pack $ show $ JSP.prettyPrint jsLabFunInstrFile)
                                        $ map (T.pack . show) jsSig
                        }
  initResp <- httpLbs reqInit man
  debugM logger $ prettyPrintResponse initResp
  mapM_ (askForBranchsToCover coverageBranches algType man jsFunCFG (Pool jsSig cpool branches) labBranches) [1..iterateTotal]


askForBranchsToCover :: Int -> Algorithm -> Manager -> Gr NLab ELab -> Pool -> EnumLEdge -> Int -> IO ()
askForBranchsToCover branchN algType man cfg pool labBranches iterN = do
  if (branchN /= 0)
    then do showAllBranches labBranches
            choice <- getLine
            let choiceN = read choice :: Int
                branchesToCover = if (null choice) || (choiceN == 0)
                                  then labBranches
                                  else [(labBranches!!(choiceN-1))]
            killJSMutationGeneticAll algType man cfg pool branchesToCover iterN
    else killJSMutationGeneticAll algType man cfg pool labBranches iterN


showAllBranches :: EnumLEdge -> IO ()
showAllBranches branches = do
  putStrLn "Choose the branch to cover:"
  putStrLn "0: all branches"
  mapM_ (\(brId, br) -> putStrLn $ (show brId) ++ ": " ++ (show br)) branches


prettyPrintResponse :: Response C.ByteString -> String
prettyPrintResponse response = "Status: " ++
                               (show $ statusMessage $ responseStatus response) ++
                               ", Body: " ++
                               (show $ responseBody response)


killJSMutationGeneticAll :: Algorithm -> Manager -> Gr NLab ELab -> Pool -> EnumLEdge -> Int -> IO ()
killJSMutationGeneticAll algType man cfg pool branchesToCover iterN =
  mapM_  (\(i, branch) ->
            killJSMutationGenetic algType man i (Target cfg [branch, (-1,-1,"exit")]) pool) branchesToCover
  where
    killJSMutationGenetic :: Algorithm -> Manager -> Int -> Target -> Pool -> IO ()
    killJSMutationGenetic alg man mutN target pool = do
      let logger = rootLoggerName
      criticalM logger $ replicate 70 '-'
      criticalM logger $
        "Started iteration #" ++
        (show iterN) ++
        " to cover branch: #" ++
        (show mutN) ++
        " -> " ++
        (show $ jsTargetPath target)
      noticeM logger $ "Initial pool data: " ++ (show pool)
      request <- parseRequest "http://localhost:7777/mutation"
      let reqMut = request { method = "POST"
                           , requestHeaders = [(CI.mk "Content-Type", "text/html;charset=UTF-8")]
                           , requestBody = RequestBodyLBS $ encode (MutData (T.pack $ show mutN))
                           }
      mutResp <- httpLbs reqMut man
      debugM logger $ prettyPrintResponse mutResp
      jsArgs <- runAlgorithm alg target pool
      -- for the integration testing purpose runGenetic has been replaced with fitnessScore
      -- mkTestCFG "./Genetic/safeAdd.js" >>= \g -> fitnessScore (Target g 9) [DomJS test_html, StringJS "iframe"]
      -- let jsArgs = [DomJS test_html, StringJS "iframe"]
      criticalM logger $ "Completed iteration #" ++ (show iterN) ++ " to cover branch: #" ++ (show mutN) ++ " -> " ++ (show $ jsTargetPath target)
      let reqExec = request { method = "POST"
                            , requestHeaders = [(CI.mk "Content-Type", "text/html;charset=UTF-8")]
                            , queryString = "execute=true"
                            , requestBody = RequestBodyLBS $ encode (ArgData $ jsargs2bstrs jsArgs)
                            }
      execResp <- httpLbs reqExec man
      debugM logger $ prettyPrintResponse execResp
      -- | End of test generation 
      setCondBreakPoint
      return ()



-- | The generateJSMutations function given a function produces all possible mutations out of existing.
-- | It returns the mutated programs together with the source positions to which the mutation operators were applied.
generateJSMutations :: JavaScript SourcePosLab -> [(Int, Mutation SourcePosLab)]
generateJSMutations js = zip [1..] $ concatMap (\m -> m js) domMutations


data InitData = InitData { jsFun :: Text , jsSig :: [Text]}

instance ToJSON InitData where
    toJSON (InitData jsFun jsSig) = object [ "jsFun" .= jsFun , "jsSig" .= jsSig]

data MutData = MutData { mutN :: Text }

instance ToJSON MutData where
    toJSON (MutData mutN) = object [ "mutN" .= mutN ]


data ArgData = ArgData { jsArgs :: Text }

instance ToJSON ArgData where
    toJSON (ArgData jsArgs) = object [ "jsArgs" .= jsArgs ]


