{-# LANGUAGE OverloadedStrings #-}

module MutationMain where

-- | System
import System.Environment (getArgs) -- pass arguments to the program

-- | Parsing
import Language.ECMAScript3.Parser
import Language.ECMAScript3.Syntax
import qualified Language.ECMAScript3.PrettyPrint as JSP
import Data.Default.Class (def) -- create default value of the SourcePos datatype
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
import Analysis.CFG.Build

-- | Networking
import Network.HTTP.Conduit
import qualified Data.CaseInsensitive as CI


 -- | Genetics
import Genetic.Instance
import Genetic.DataJS
import Genetic.ScoreJS

-- | Mutations
import Mutation.Dom.Operators

-- | Graphs
import Data.Graph.Inductive


-- | Debug
import Debug.Trace

-- | run main: :main "nodeCovertest.js"
main :: IO ()    
main = do
  (jsFile:_) <- getArgs
  putStrLn $ "The following JS file is given for analysis: " ++ (show jsFile)
  jsFun <- parseFromFile jsFile
  jsSig <- parseJSSignature jsFile
  print jsSig
  let jsLabFun@(Script l jsLabStms) = fst $ flip runState 0 $ assignUniqueIdsSt_  jsFun
      jsLabMutFuns = generateJSMutations jsLabFun
      jsFunCFG     = enrichCollectedEdges jsLabStms
      jsDefInts    = []
      jsDefStrings = []
      jsDefTags    = []
      jsDefIds     = []
      jsDefNames   = []
      jsDefClasses = []
      jsDefDoms    = (jsDefTags, jsDefIds, jsDefNames, jsDefClasses)
      jsDefPool    = (jsDefInts, jsDefStrings, jsDefDoms) :: JSCPool

  request <- parseUrl "http://localhost:8888"
  man <- liftIO $ newManager conduitManagerSettings

  let reqInit = request { method = "POST"
                        , requestHeaders = [(CI.mk "Content-Type", "text/html;charset=UTF-8")]
                        , queryString = "init=true"
                        , requestBody = RequestBodyLBS $ encode (InitData (T.pack $ show $ JSP.prettyPrint jsFun))
                        }
  initResp <- httpLbs reqInit man
  print $ responseBody initResp

  mapM_  (\(i, jsLabMutFun, jsMutLoc) -> 
              killJSMutationGenetic man 
                                    i
                                    (show $ JSP.prettyPrint $ (instrScript jsLabMutFun :: JavaScript SourcePosLab))
                                    (Target (uncurry mkGraph jsFunCFG) (snd jsMutLoc)) 
                                    (jsSig, jsDefPool)
         ) jsLabMutFuns



killJSMutationGenetic :: Manager -> Int -> String -> Target -> (JSSig, JSCPool) -> IO ()
killJSMutationGenetic man mutN jsMutFun target pool = do
  request <- parseUrl "http://localhost:8888"
  let reqMut = request { method = "POST"
                       , requestHeaders = [(CI.mk "Content-Type", "text/html;charset=UTF-8")]
                       , queryString = "mutation=true"
                       , requestBody = RequestBodyLBS $ encode (MutData (T.pack $ show mutN) (T.pack jsMutFun))
                       }
  mutResp <- httpLbs reqMut man
  print $ responseBody mutResp

  -- jsArgs <- runGenetic target pool
  -- for the integration testing purpose runGenetic has been replaced with fitnessScore
  mkTestCFG "./Genetic/safeAdd.js" >>= \g -> fitnessScore (Target g 9) [DomJS test_html, StringJS "iframe"]
  let jsArgs = [DomJS test_html, StringJS "iframe"]
  putStrLn $ "best entity (GA): " ++ (show jsArgs)

  let reqExec = request { method = "POST"
                        , requestHeaders = [(CI.mk "Content-Type", "text/html;charset=UTF-8")]
                        , queryString = "execute=true"
                        , requestBody = RequestBodyLBS $ encode (ArgData $ jsargs2bstrs jsArgs)
                        }
  execResp <- httpLbs reqExec man
  print    $ responseBody execResp         
  return ()



-- | The generateJSMutations function given a function produces all possible mutations out of existing. It returns the mutated programs together with the source positions to which the mutation operators were applied.x
generateJSMutations :: JavaScript SourcePosLab -> [(Int, JavaScript SourcePosLab, SourcePosLab)]
generateJSMutations js = zipWith (\x (y, z) -> (x, y, z)) [1..] $ concatMap (\m -> m js) domMutations


data InitData = InitData { jsFun :: Text }

instance ToJSON InitData where
    toJSON (InitData jsFun) = object [ "jsFun" .= jsFun ]

data MutData = MutData { mutN :: Text, jsMutFun :: Text }

instance ToJSON MutData where
    toJSON (MutData mutN jsMutFun) = object [ "mutN" .= mutN, "jsMutFun" .= jsMutFun ]


data ArgData = ArgData { jsArgs :: Text }

instance ToJSON ArgData where
    toJSON (ArgData jsArgs) = object [ "jsArgs" .= jsArgs ]


