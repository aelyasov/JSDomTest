{-# LANGUAGE OverloadedStrings #-}

module Genetic.MutationJS (mutateJSArg) where

import Data.ByteString.Lazy (ByteString)
import Text.XML.Pretty (prettyHtmlByteString, prettyDocument, bytestring2document, bytestring2LabeledDocument)
import Text.XML.Label (deleteNodeInDocumentByLabel, removeAttributeFromDocument)
import Text.XML.Statistics (depthDocument, sizeDocument)
import Text.XML (Document(..))
import Text.XML.Convert (document2bytestring)
import System.Log.Logger (rootLoggerName, infoM, debugM, noticeM, criticalM)
-- import System.Random (mkStdGen, StdGen, randomR)
import System.Random
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Text.Blaze.Html.Renderer.Utf8 as BR
import Text.Blaze.Html (toHtml)
import Html5C.ValidationTest (askValidator)
import Html5C.Attributes (assignIdsToDocumentRandomly, assignClassesToDocumentRandomly)
import Genetic.DataJS (JSCPool, JSArg(..), getJSInts, getJSFloats, getJSStrings, getJSDoms, getJSIds, getJSClasses, JSType(..))
import Control.Monad (liftM)
import Genetic.RandomJS (genRandomInt, genRandomFloat, genRandomString, genRandomDom, genRandomArray, genRandomVal)
import Analysis.Static (removeDuplicates)
import Test.QuickCheck.Gen (elements, generate, frequency)
import Util.Debug (setCondBreakPoint)
import Analysis.CFG.Util (replaceElemInList)
import Safe (atNote)
import Debug.Trace


mutateJSArg :: JSArg -> JSType -> StdGen -> JSCPool -> IO JSArg
mutateJSArg jsArg jsType gen pool =
  case (jsArg, jsType) of
    (DomJS dom,  _) ->  mutateHtml [ReassignIds, ReassignClasses, NewRandom] gen pool dom
    -- DropSubtree,  
    (IntJS int,  _)     -> mutateJSInt int pool
    (FloatJS float,  _) -> mutateJSFloat float pool
    (StringJS str, _)   -> mutateJSString gen str pool
    (BoolJS b,   _)     -> mutateJSBool b
    (ArrayJS arr, JS_ARRAY typ) -> mutateJSArray gen arr typ pool
    mtype       -> error $ "mutation of type " ++ (show mtype)  ++ " isn't defined"


data MutationType = DropSubtree
                  | NewRandom
                  | ReassignIds
                  | ReassignClasses
                  deriving (Show)


mutateHtml :: [MutationType] -> StdGen -> JSCPool -> ByteString -> IO JSArg
mutateHtml mutations gen pool html = do
  mutN <- randomRIO (0, (length mutations) - 1)
  liftM DomJS $ mutateHtml' (mutations!!mutN) gen pool html
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
  liftM getDomJS $ genRandomDom setPool


mutateHtml_reassignIds :: JSCPool -> ByteString -> IO ByteString
mutateHtml_reassignIds pool html = do
  let logger  = rootLoggerName
      setPool = removeDuplicates pool
      tagIds  = getJSIds setPool
      strs    = getJSStrings setPool
      plain_html = removeAttributeFromDocument "id" $ bytestring2document html
  debugM rootLoggerName $ "Mutate element:\n" ++ (prettyHtmlByteString html)    
  debugM logger $ "Constant pool data: " ++ (show setPool)
  new_html <- liftM document2bytestring $ assignIdsToDocumentRandomly strs tagIds plain_html
  debugM rootLoggerName $ "Mutation result: re-assign ids:\n" ++ (prettyHtmlByteString new_html)
  setCondBreakPoint
  return new_html


mutateHtml_reassignClasses :: JSCPool -> ByteString -> IO ByteString
mutateHtml_reassignClasses pool html = do
  let logger     = rootLoggerName
      setPool    = removeDuplicates pool
      tagClasses = getJSClasses setPool
      strs       = getJSStrings setPool
      plain_html = removeAttributeFromDocument "class" $ bytestring2document html
  debugM rootLoggerName $ "Mutate element:\n" ++ (prettyHtmlByteString html)    
  debugM logger $ "Constant pool data: " ++ (show setPool)
  new_html <- liftM document2bytestring $ assignClassesToDocumentRandomly strs tagClasses plain_html
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
  randInt <- liftM getIntJS $ genRandomInt $ getJSInts pool
  let mutInts = map (\f -> f int) [(+1), (+(-1))]
  result <- generate $ frequency $ zip [4,1,1] $ map return (randInt:mutInts)
  debugM rootLoggerName $ "Mutation of the integer value: " ++ (show int) ++ " replaced by: " ++ (show result)
  return $ IntJS result


mutateJSFloat :: Float -> JSCPool -> IO JSArg
mutateJSFloat float pool = do
  r <- liftM getFloatJS $ genRandomFloat $ getJSFloats pool
  let floats = map (\f -> f float) [(+10.0), (+(-10.0)), (+1.0), (+(-1.0)), (+0.1), (+(-0.1)), fromIntegral . round]
  float' <- generate $ elements (r:floats)
  debugM rootLoggerName $ "Mutation of the float value: " ++ (show float) ++ " replaced by: " ++ (show float')
  return $ FloatJS float'  


mutateJSBool :: Bool -> IO JSArg
mutateJSBool b = do
  let b' = not b
  debugM rootLoggerName $ "Mutate boolean value: " ++ (show b) ++ " replaced by: " ++ (show b')
  return $ BoolJS b'  


mutateJSString :: StdGen -> String -> JSCPool -> IO JSArg
mutateJSString gen str pool =
  if null str
  then genRandomString Nothing 
  else do
    let (mutId, gen') = randomR (0, length str - 1) gen
        chr = atNote "mutateJSString" str mutId
        mutChar = if even mutId
                  then if chr `elem` ['z', '9', 'Z'] then pred chr else succ chr
                  else if chr `elem` ['a', '0', 'A'] then succ chr else pred chr
        mutStr = replaceElemInList mutId (Just mutChar) str
        delStr = replaceElemInList mutId Nothing str
    randStr <- liftM getStringJS $ genRandomString $ getJSStrings pool    
    mresult <- generate $ frequency [(4, return mutStr), (1, return randStr), (1, return delStr)]  
    debugM rootLoggerName $ "Mutation of string: " ++ show str ++ " at an index " ++ (show mutId)   
    debugM rootLoggerName $ "Mutated string:  " ++ show mresult    
    return $ StringJS mresult


mutateJSArray :: StdGen -> [JSArg] -> JSType -> JSCPool -> IO JSArg
mutateJSArray gen args typ pool =
  if null args
  then genRandomArray pool typ
  else do
    let (mutId, gen') = randomR (0, length args - 1) gen
        arg = atNote "mutateJSArray" args mutId
    debugM rootLoggerName $ "Mutation of an array: " ++ show args ++ " at an index " ++ (show mutId)
    mutArg <- mutateJSArg arg typ gen' pool
    let mresult = replaceElemInList mutId (Just mutArg) args
    debugM rootLoggerName $ "Mutated array:  " ++ show mresult
    return $ ArrayJS mresult
  

-- | TEST: mutateHtml (mkStdGen 5) thtml
thtml :: ByteString
thtml = "<!DOCTYPE HTML><html><head><title>Title1</title></head><body><h1><a></a>Foo</h1><h1></h1></body></html>"

thtml1 :: ByteString
thtml1 = "<!DOCTYPE HTML><html><head><title>Title</title></head><body><div><div></div><div></div></div><div><div></div><div><div></div></div></div><div></div></body></html>"

thtml2 :: ByteString
thtml2 = "<!DOCTYPE HTML>"
