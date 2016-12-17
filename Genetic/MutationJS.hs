{-# LANGUAGE OverloadedStrings #-}

module Genetic.MutationJS where

import Data.ByteString.Lazy (ByteString)
import Text.XML.Pretty (prettyHtmlByteString, prettyDocument, bytestring2document, bytestring2LabeledDocument)
import Text.XML.Label (deleteNodeInDocumentByLabel, removeAttributeFromDocument)
import Text.XML.Statistics (depthDocument, sizeDocument)
import Text.XML (Document(..))
import Text.XML.Convert (document2bytestring)
import System.Log.Logger (rootLoggerName, infoM, debugM, noticeM)
-- import System.Random (mkStdGen, StdGen, randomR)
import System.Random
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Text.Blaze.Html.Renderer.Utf8 as BR
import Text.Blaze.Html (toHtml)
import Html5C.ValidationTest (askValidator)
import Html5C.Attributes (assignIdsToDocumentRandomly, assignClassesToDocumentRandomly)
import Genetic.DataJS (JSCPool, JSArg(..), getJSInts, getJSStrings, getJSDoms, getJSIds, getJSClasses)
import Control.Monad (liftM)
import Genetic.RandomJS (genRandomInt, genRandomString, genRandomDom)
import Analysis.Static (removeDuplicates)
import Test.QuickCheck.Gen (elements, generate)
import Util.Debug (setCondBreakPoint)


data MutationType = DropSubtree
                  | NewRandom
                  | ReassignIds
                  | ReassignClasses
                  deriving (Show)

mutateHtml :: [MutationType] -> StdGen -> JSCPool -> ByteString -> IO ByteString
mutateHtml mutations gen pool html = do
  mutN <- randomRIO (0, (length mutations) - 1)
  mutateHtml' (mutations!!mutN) gen pool html
    where
      mutateHtml' :: MutationType -> StdGen -> JSCPool -> ByteString -> IO ByteString
      mutateHtml' mutType gen pool html = case mutType of
        DropSubtree     -> mutateHtml_dropSubtree gen html
        NewRandom       -> mutateHtml_newRandom pool
        ReassignIds     -> mutateHtml_reassignIds pool html
        ReassignClasses -> mutateHtml_reassignClasses pool html

    
mutateHtml_dropSubtree :: StdGen -> ByteString -> IO ByteString
mutateHtml_dropSubtree gen html = do
  let logger = rootLoggerName
  debugM logger $ "Mutation: drop subtree\n" ++ (prettyHtmlByteString html)
  let parent = bytestring2LabeledDocument html
  if (snd parent <= 4)
    then do setCondBreakPoint
            return html
    else mutateIterate gen parent


mutateHtml_newRandom :: JSCPool -> IO ByteString
mutateHtml_newRandom pool = do
  let logger  = rootLoggerName
      setPool = removeDuplicates pool
  debugM logger $ "Constant pool data: " ++ (show setPool)
  debugM rootLoggerName $ "Mutation: generate new html:"
  genRandomDom $ getJSDoms setPool


mutateHtml_reassignIds :: JSCPool -> ByteString -> IO ByteString
mutateHtml_reassignIds pool html = do
  let logger  = rootLoggerName
      setPool = removeDuplicates pool
      tagIds  = getJSIds setPool
      plain_html = removeAttributeFromDocument "id" $ bytestring2document html
  debugM rootLoggerName $ "Mutate element:\n" ++ (prettyHtmlByteString html)    
  debugM logger $ "Constant pool data: " ++ (show setPool)
  new_html <- liftM document2bytestring $ assignIdsToDocumentRandomly tagIds plain_html
  debugM rootLoggerName $ "Mutation result: re-assign ids:\n" ++ (prettyHtmlByteString new_html)
  setCondBreakPoint
  return new_html


mutateHtml_reassignClasses :: JSCPool -> ByteString -> IO ByteString
mutateHtml_reassignClasses pool html = do
  let logger     = rootLoggerName
      setPool    = removeDuplicates pool
      tagClasses = getJSClasses setPool
      plain_html = removeAttributeFromDocument "class" $ bytestring2document html
  debugM rootLoggerName $ "Mutate element:\n" ++ (prettyHtmlByteString html)    
  debugM logger $ "Constant pool data: " ++ (show setPool)
  new_html <- liftM document2bytestring $ assignClassesToDocumentRandomly tagClasses plain_html
  debugM rootLoggerName $ "Mutation result: re-assign classes:\n" ++ (prettyHtmlByteString new_html)
  setCondBreakPoint
  return new_html
  

mutateIterate :: StdGen -> (Document, Int) -> IO ByteString  
mutateIterate gen doc@(fromDoc, docDepth) = do
  let logger = rootLoggerName
      (fromLabel, gen1) = randomR (4, docDepth - 1) gen
      mutatedDocument = mutateDocument fromLabel fromDoc
      mutatedDocumentNoLabel = fmap (removeAttributeFromDocument "label") mutatedDocument
  debugM logger $ "Mutation is applied at the node #" ++ (show fromLabel) ++ " out of " ++ (show docDepth)  
  case mutatedDocumentNoLabel of
   Just document -> do let html   = toHtml document
                           result = BR.renderHtml html 
                       response <- askValidator result
                       case response of
                        Just _  -> do debugM logger $ "The Mutant is inconsistent html:\n" ++ (renderHtml html)
                                      mutateIterate gen1 doc
                        Nothing -> do setCondBreakPoint
                                      debugM logger $ "Mutation result:\n" ++ (renderHtml html)
                                      return result              
   Nothing       -> mutateIterate gen1 doc


mutateDocument :: Int -> Document -> Maybe Document      
mutateDocument = deleteNodeInDocumentByLabel


mutateJSInt :: Int -> JSCPool -> IO JSArg
mutateJSInt int pool = do
  r <- genRandomInt $ getJSInts pool
  let ints = map (\f -> f int) [(+1), (+(-1))]
  int' <- generate $ elements (r:ints)
  debugM rootLoggerName $ "Mutation of the integer value: " ++ (show int) ++ " replaced by: " ++ (show int')
  return $ IntJS int'

-- mutateJSInt :: JSCPool -> IO JSArg
-- mutateJSInt pool = do
--   i <- genRandomInt $ getJSInts pool
--   debugM rootLoggerName $ "Mutate by generating a new int arg" ++ (show i)
--   return $ IntJS i 

mutateJSString :: JSCPool -> IO JSArg
mutateJSString pool = do
  s <- genRandomString $ getJSStrings pool
  debugM rootLoggerName $ "Mutate by generating a new str arg" ++ (show s)
  return $ StringJS s


-- | TEST: mutateHtml (mkStdGen 5) thtml
thtml :: ByteString
thtml = "<!DOCTYPE HTML><html><head><title>Title1</title></head><body><h1><a></a>Foo</h1><h1></h1></body></html>"

thtml1 :: ByteString
thtml1 = "<!DOCTYPE HTML><html><head><title>Title</title></head><body><div><div></div><div></div></div><div><div></div><div><div></div></div></div><div></div></body></html>"

thtml2 :: ByteString
thtml2 = "<!DOCTYPE HTML>"
