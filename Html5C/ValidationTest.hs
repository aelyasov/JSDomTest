{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, StandaloneDeriving #-}

module Html5C.ValidationTest where

import Data.Maybe (fromMaybe)

import Html5C.Validation (checkErrorsInResponse)
import Html5C.Arbitrary
import Html5C.Tags
import Html5C.QuickCheck.Gen
import Html5C.Attributes

import Network.HTTP.Types
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Conduit (parseRequest, newManager, tlsManagerSettings, Request(..), RequestBody(..), httpLbs, responseBody, responseTimeoutMicro)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State
import Control.Monad

import Text.HTML.DOM (parseLT)
import Text.Blaze 
import Text.Blaze.Html5 (Html)
import Text.Blaze.Internal
import Text.Blaze.Html.Renderer.Utf8
import qualified Text.Blaze.Html.Renderer.Pretty as PP
import qualified Text.Blaze.Html.Renderer.String as PS
import qualified Text.Blaze.Html.Renderer.Text as PT
import Text.XML.Pretty (prettyHtmlByteString)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as C
import Data.ByteString.Lazy (ByteString)
-- import Data.ByteString.Lazy.Internal

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Genetic.DataJS
import Debug.Trace
import System.Log.Logger (rootLoggerName, infoM, debugM, noticeM)

-- import Text.HTML.DOM


runTests :: IO ()
-- main = quickCheckWith (stdArgs {maxSuccess = 1000}) prop_ValidHtml
runTests = verboseCheckWith (stdArgs {maxSuccess = 10}) prop_ValidHtml


runTestsWithTags :: JSTags -> IO ()
runTestsWithTags tags = verboseCheckWith (stdArgs {maxSuccess = 10}) property
-- runTestsWithTags tags = quickCheckWith (stdArgs {maxSuccess = 100}) property
  where property = monadicIO $ do
          h <- run $ genValidHtml (Nothing, Nothing, Nothing, (tags, Nothing, Nothing, Nothing))
          assert True


prop_ValidHtml :: Html -> Property
prop_ValidHtml html = monadicIO $ (run $ validatorCheck html) >>= assert'


genValidHtml :: JSCPool -> IO ByteString
genValidHtml env@(_, _, strs, (tags, ids, names, classes)) = do
  let state = defaultState{ htmlTags    = fromMaybe [TAG_DIV] tags
                          , htmlNames   = names
                          , htmlIds     = ids
                          , htmlClasses = classes}
      logger = rootLoggerName
  -- debugM logger $ "First step of html generation (no ids):\n"  
  --   ++ (prettyHtmlByteString $ renderHtml htmlNoIds)
  -- debugM logger $ "Second step of html generation (with ids):\n"
  --   ++ (prettyHtmlByteString html)
  docPlain       <- liftM (parseLT . PT.renderHtml) $ generate $ evalStateT htmlGenState state
  docWithIds     <- assignIdsToDocumentRandomly strs (htmlIds state) docPlain
  docWithClasses <- assignClassesToDocumentRandomly strs (htmlClasses state) docWithIds
  html <- return $ (renderHtml . toMarkup) docWithClasses
  response  <- askValidator  html
  case response  of
    Just _  -> do -- debugM logger "Generated html document is invalid"
                  genValidHtml env
    Nothing -> return html
  

validatorCheck :: Html -> IO (Maybe Text)
validatorCheck = askValidator . renderHtml

defaultResponseTimeout :: Int
defaultResponseTimeout = 60 * 10 ^ 6 -- one minute in microseconds

askValidator :: ByteString -> IO (Maybe Text)
askValidator html_str = do
  man <- liftIO $ newManager tlsManagerSettings
  -- initReq <- parseRequest "http://html5.validator.nu" 
  initReq <- parseRequest "http://localhost:8888"
  let req = initReq { method = "POST"
                    , responseTimeout = responseTimeoutMicro defaultResponseTimeout
                    , requestHeaders = [(CI.mk "Content-Type", "text/html;charset=UTF-8")]
                    , queryString = "laxtype=yes&parser=html5&out=json"
                    , requestBody = RequestBodyLBS  html_str
                    }
  response <- httpLbs req man
  return $ checkErrorsInResponse $ responseBody response


instance Show Html where
  show = PP.renderHtml    
    

assert' :: Monad m => Maybe Text -> PropertyM m ()
assert' Nothing = return ()
assert' (Just str) = fail $ T.unpack str

test_genValidHtml = genValidHtml (Nothing, Nothing, Nothing, (Nothing, Nothing, Nothing, Nothing))

-- | parseLBS $ C.pack test_html
test_html = "<!DOCTYPE HTML>\n<html><head><title>Title</title><base href=\".\" target=\"_blank\"></head><body itemscope=\"\" itemtype=\"http://schema.org/WebPage\"><h1><a></a>A2A2</h1><h1></h1></body></html>"
