{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Mock.ServerJS where

import Genetic.DataJS
import Servant 
import Network.Wai
import Network.Wai.Handler.Warp (run)
import System.Posix.Signals (sigINT, killProcess, installHandler, Handler (CatchOnce), signalProcess)
import System.Posix.Process (getProcessID)
import Control.Concurrent (forkIO, killThread, ThreadId)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.Map (Map)
import qualified Data.Map as Map
import Network.HTTP.Media ((//), (/:))


type JSTestApi = "genetic" :> ReqBody '[JSON] GAInput :> Post '[JSON] JSExecution

jsTestApi :: Proxy JSTestApi
jsTestApi = Proxy

server :: Server JSTestApi
server = geneticH where
  geneticH :: GAInput -> Servant.Handler JSExecution
  geneticH _ = do
    return $ JSExecution [3,2,1] [] Map.empty (JSEnviroment [] [] [] [] [])

mock :: Application
mock = serve jsTestApi server

mockPort = 7777

runMockServer :: IO ThreadId
runMockServer = do
  flag <- newEmptyMVar
  tid <- forkIO $ run mockPort mock
  putStrLn $ "ThreadID: " ++ (show tid)
  -- installHandler sigINT (CatchOnce $ do
  --                           putStrLn "Caught an interrupt"
  --                           killThread tid
  --                           putMVar flag ()
  --                       ) Nothing
  p <- getProcessID
  putStrLn $ "ProcessID: " ++ (show p)
  return tid
  -- putStrLn "Warp exited"
  -- takeMVar flag

stopMockServer :: ThreadId -> IO ()
stopMockServer = killThread
-- do
-- p <- getProcessID
-- putStrLn $ show p
-- signalProcess sigINT p
  
