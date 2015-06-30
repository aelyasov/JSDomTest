{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad    (msum, liftM)
import Control.Monad.Trans.Class
import Happstack.Server ( Method(GET, POST), dir, method, toResponse
                        , Browsing(EnableBrowsing), nullConf, ok, look
                        , serveDirectory, simpleHTTP, seeOther, path
                        )
import Html5.Arbitrary
-- import Language.JavaScript.Parser

import System.Environment (getArgs) -- pass arguments to the program
import Language.ECMAScript3.Parser
import Language.ECMAScript3.Syntax
import Language.ECMAScript3.PrettyPrint (prettyPrint)

import Data.Default.Class (def) -- create default value of the SourcePos datatype

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T 

import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy (ByteString)

import qualified Codec.Binary.UTF8.String as U

import Genetic.Instance
import GA (Entity(..), GAConfig(..), evolveVerbose)


import Debug.Trace

-- | run main: :main "nodeCovertest.js"
main :: IO ()
main = do 
  (jsFile:_) <- getArgs 
  putStrLn $ "The following JS file is given for analysis: " ++ (show jsFile)
  jsAST <- liftM unJavaScript $ parseFromFile jsFile
  let -- jsMutASTs = generateJSMutations jsAST
      jsMutASTs = [(jsAST, def)]
      -- jsSigAST  = parseJSSignature jsAST 
      jsSigAST  = [JS_Dom]
  jsTests <- mapM (genKillingArgs jsAST jsSigAST) jsMutASTs
  mapM (putStrLn . show) jsTests
  -- putStrLn $ show $ prettyPrint jsAST  
  return ()
-- main = simpleHTTP nullConf $ msum
--        [ do method POST
--             lift getNewHtml >>= ok . toResponse
--        ]


data ArgJS = JS_Int | JS_String | JS_Bool | JS_Dom
             deriving Show

type SignJS = [ArgJS]

type JSTestArgs = [String]

-- | The generateJSMutations function given a function produces all possible mutations out of existing. It returns mutated programs together with the source position to which the mutation operators were applied.
generateJSMutations :: [Statement SourcePos] -> [([Statement SourcePos], SourcePos)]
generateJSMutations = undefined


-- | The parseJSSignature function extracts the type signature of the given JS function from the comment.
parseJSSignature :: [Statement SourcePos] ->  SignJS
parseJSSignature = undefined

-- | The genKillingArgs function takes the original JS function, its mutated version and the signature, and it returns argument substitutions that exposes the mutation.
-- | TODO: we should somewhere store the intermediate values of the generated arguments to check if they can kill other mutations
genKillingArgs :: [Statement SourcePos] -> SignJS -> ([Statement SourcePos], SourcePos) -> IO ([Statement SourcePos], SourcePos, JSTestArgs) 
genKillingArgs jsFun jsSig (jsMFun, mpos) = do
  simpleHTTP nullConf $ msum
                 [ do dir "rungenetic" $ 
                          trace "rungenetic" $ do method GET
                                                  score <- look "fitscore"
                                                  ok $ toResponse (score :: String)
                 -- $ ok $ toResponse ("rungenetic" :: ByteString)
                              -- $ trace "rungenetic" 
                              --       $ do method POST
                              --            score <- look "score"
                              --            ok $ toResponse ("rungenetic" :: ByteString)
                        -- ok (B.pack $ U.encode $ show score)
                 , do method POST 
                      -- ok ("Hello, World!" :: String)       
                      ok $ trace "all" $ toResponse $ encode (MAInput (showDoc jsFun)
                                                                   (T.pack $ show jsSig)
                                                                   (showDoc jsMFun)
                                                                   (T.pack $ show mpos))
                 ]
  return (jsMFun, mpos, [])
      where
        showDoc = T.pack . show . prettyPrint





data MAInput = MAInput 
    { funJS    :: Text
    , funSigJS :: Text
    , mfunJS   :: Text
    , mposJS   :: Text
    } deriving Show


instance ToJSON MAInput where
    -- to invoke the 'encode' function apply this flag ":set -XOverloadedStrings"
    toJSON (MAInput funJS funSigJS mfunJS mposJS) = 
        object [ "funJS"    .= funJS
               , "funSigJS" .= funSigJS
               , "mfunJS"   .= mfunJS
               , "mposJS"   .= mposJS
               ]

