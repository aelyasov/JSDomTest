{-# LANGUAGE OverloadedStrings #-}

module Genetic.CrossoverJS  where

import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy (ByteString)
import Text.XML.Label (findElementInDocumentByLabel, insertElementInDocumentByLabel, removeAttributeFromDocument)
import Text.XML.Pretty (prettyHtmlByteString, prettyDocument, bytestring2LabeledDocument)
import System.Log.Logger (rootLoggerName, infoM, debugM)
import Text.XML (Document(..))
import System.Random (StdGen, randomR, mkStdGen)
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import qualified Text.Blaze.Html.Renderer.Utf8 as BR
import Text.Blaze.Html (toHtml)
import Html5C.ValidationTest (askValidator)
import Genetic.DataJS
import Control.Monad (liftM)
import System.Random (randomRIO, random)
import Debug.Trace
import Util.Debug (setCondBreakPoint)
import Data.Maybe


crossAllArgs :: StdGen -> [(JSArg, JSArg)] -> IO [JSArg]
crossAllArgs gen [] = return []
crossAllArgs gen (pairJsArg:pairJsArgs) = do 
  let (a, gen') = random gen :: (Int, StdGen)
  d <- crossoverJSArgs gen pairJsArg
  pairJsArgs' <- crossAllArgs gen' pairJsArgs
  return (d:pairJsArgs')

crossoverJSArgs :: StdGen -> (JSArg, JSArg) -> IO JSArg
crossoverJSArgs gen pairJsArgs = case pairJsArgs of
  (BoolJS b1, BoolJS b2)       -> crossoverBoolJS b1 b2 
  (DomJS d1, DomJS d2)         -> crossoverDomJS gen d1 d2
  (IntJS i1, IntJS i2)         -> crossoverIntJS i1 i2
  (FloatJS f1, FloatJS f2)     -> crossoverFloatJS f1 f2
  (StringJS s1, StringJS s2)   -> crossoverStringJS s1 s2
  (ArrayJS arr1, ArrayJS arr2) -> crossoverArrayJS arr1 arr2
  otherwise -> error $ "crossover for elements of type " ++ (show pairJsArgs) ++ " isn't defined"


crossoverBoolJS :: Bool -> Bool -> IO JSArg
crossoverBoolJS b1 b2 = do
  result <- liftM ([b1,b2]!!) $ randomRIO (0, 1)
  debugM rootLoggerName $ "Crossing over: " ++ (show b1) ++ " and " ++ (show b2) ++ " results in " ++ (show result)
  setCondBreakPoint
  return $ BoolJS result

  
crossoverArrayJS :: [JSArg] -> [JSArg] -> IO JSArg
crossoverArrayJS arr1 arr2 = do
  crossPoint1 <- randomRIO (0, length arr1)
  crossPoint2 <- randomRIO (0, length arr2)
  let crossArr1 = take crossPoint1 arr1 ++ drop crossPoint2 arr2
      crossArr2 = take crossPoint2 arr2 ++ drop crossPoint1 arr1
  result <- liftM ([crossArr1, crossArr2]!!) $ randomRIO (0, 1)
  debugM rootLoggerName $ "Crossing over: " ++ (show arr1) ++ " and " ++ (show arr2) ++ " results in " ++ (show result)
  setCondBreakPoint
  return $ ArrayJS result
  

crossoverStringJS :: String -> String -> IO JSArg
crossoverStringJS str1 str2 = do
  -- result <- liftM ([str1,str2]!!) $ randomRIO (0, 1)
  -- debugM rootLoggerName $ "Crossing over: " ++ (show str1) ++ " and " ++ (show str2) ++ " results in " ++ (show result)
  -- setCondBreakPoint
  -- return $ StringJS result

  crossPoint1 <- randomRIO (0, length str1)
  crossPoint2 <- randomRIO (0, length str2)
  let crossStr1 = take crossPoint1 str1 ++ drop crossPoint2 str2
      crossStr2 = take crossPoint2 str2 ++ drop crossPoint1 str1
  result <- liftM ([crossStr1, crossStr2]!!) $ randomRIO (0, 1)
  debugM rootLoggerName $ "Crossing over: " ++ (show str1) ++ " and " ++ (show str2) ++ " results in " ++ (show result)
  setCondBreakPoint
  return $ StringJS result


crossoverIntJS :: Int -> Int -> IO JSArg
crossoverIntJS i1 i2 = do
  result <- liftM ([i1,i2]!!) $ randomRIO (0, 1)
  debugM rootLoggerName $ "Crossing over: " ++ (show i1) ++ " and " ++ (show i2) ++ " results in " ++ (show result)
  setCondBreakPoint
  return $ IntJS result


crossoverFloatJS :: Float -> Float -> IO JSArg
crossoverFloatJS f1 f2 = do
  result <- liftM ([f1,f2]!!) $ randomRIO (0, 1)
  debugM rootLoggerName $ "Crossing over: " ++ (show f1) ++ " and " ++ (show f2) ++ " results in " ++ (show result)
  setCondBreakPoint
  return $ FloatJS result  
  

crossoverDomJS :: StdGen -> ByteString -> ByteString -> IO JSArg
crossoverDomJS gen html1 html2 = do
  let logger = rootLoggerName
  debugM logger $ "Crossing over two html document:\n"
    ++ (prettyHtmlByteString html1)
    ++ "\n" ++ "with" ++ "\n"
    ++ (prettyHtmlByteString html2)
  let parent1 = bytestring2LabeledDocument html1
      parent2 = bytestring2LabeledDocument html2
  if (snd parent1 <= 4)
    then do debugM logger $ "Crossover result:\n" ++ (prettyHtmlByteString html2)
            setCondBreakPoint
            return $ DomJS html2      
    else if (snd parent2 <= 4)
         then do debugM logger $ "Crossover result:\n" ++ (prettyHtmlByteString html1)
                 setCondBreakPoint
                 return $ DomJS html1
         else do (crossedHtml, status) <- crossoverIterate 10 gen parent1 parent2
                 if status
                   then return $ DomJS crossedHtml
                   else do debugM logger $ "Crossover limit is exhausted"
                           setCondBreakPoint
                           liftM (DomJS . ([html1, html2]!!)) $ randomRIO (0, 1)



crossoverIterate :: Int -> StdGen -> (Document, Int) -> (Document, Int) -> IO (ByteString, Bool) 
crossoverIterate climit gen doc1@(fromDoc, docDepth1) doc2@(whereDoc, docDepth2) = do
  let logger = rootLoggerName
      (fromLabel, gen1) = randomR (4, docDepth1 - 1) gen
      (whereLabel, gen2) = randomR (4, docDepth2 - 1) gen1
      crossoveredDocument = crossoverDocuments fromLabel whereLabel fromDoc whereDoc
      crossoveredDocumentNoLabel = fmap (removeAttributeFromDocument "label") crossoveredDocument
  debugM logger $ "Crossover is applied at the node #" ++
    (show fromLabel) ++ " (" ++ (show docDepth1) ++ ") " ++
    " and #" ++
    (show whereLabel) ++  " (" ++ (show docDepth2) ++ ") "        
  case crossoveredDocumentNoLabel of
    Just doc -> do let html   = toHtml doc
                       result = BR.renderHtml html 
                   response <- askValidator result
                   if (climit == 0)
                     then return (result, False)
                     else case response of
                            Just _  -> do debugM logger $ "The Offsprint is inconsistent html"
                                          crossoverIterate (climit-1) gen2 doc1 doc2
                            Nothing -> do debugM logger $ "Crossover result:\n" ++ (renderHtml html)
                                          setCondBreakPoint
                                          return (result, True)              
    Nothing       -> crossoverIterate (climit-1) gen2 doc1 doc2


crossoverDocuments :: Int -> Int -> Document -> Document -> Maybe Document      
crossoverDocuments labFrom labWhere docFrom docWhere = do
  elementInsert <- findElementInDocumentByLabel labFrom docFrom
  insertElementInDocumentByLabel labWhere elementInsert docWhere


-- | TEST: crossoverDomJS (mkStdGen 5) (C.pack thtml) (C.pack thtml)
thtml1 :: ByteString
thtml1 = "<!DOCTYPE HTML><html><head><title>Title1</title></head><body><h1><a></a>Foo</h1><h1></h1></body></html>"

thtml2 :: ByteString
thtml2 = "<!DOCTYPE HTML><html><head><title>Title1</title></head><body><h1></h1></body></html>"

thtml3 :: ByteString
thtml3 = "<!DOCTYPE HTML><html><head><title>Title2</title></head><body><h2></h2></body></html>"

test4 :: ByteString
test4 = "<!DOCTYPE HTML>\n<html><head><title>Test Title</title></head><body itemscope=\"\" itemtype=\"http://schema.org/WebPage\">SPAN2<div id=\"square_0_0\"><div id=\"square_1_0\"><span>PHRASING CONTENT</span><span></span></div><span></span><span id=\"square_0_1\"><span>SPAN2<span>PHRASING CONTENT</span><span>PHRASING CONTENT</span></span><span id=\"square_1_1\">PHRASING CONTENT</span>SPAN2</span></div>SPAN2</body></html>"

