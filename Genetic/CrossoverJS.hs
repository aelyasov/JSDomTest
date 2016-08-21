{-# LANGUAGE OverloadedStrings #-}

module Genetic.CrossoverJS where

import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy (ByteString)
import Text.XML.Label (findElementInDocumentByLabel, insertElementInDocumentByLabel, removeLabelsFromDocument)
import Text.XML.Pretty (prettyHtmlByteString, prettyDocument, bytestring2document)
import System.Log.Logger (rootLoggerName, infoM, debugM)
import Text.XML (Document(..))
import System.Random (StdGen, randomR, mkStdGen)
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import qualified Text.Blaze.Html.Renderer.Utf8 as BR
import Text.Blaze.Html (toHtml)
import Html5C.ValidationTest (askValidator)

import Debug.Trace


crossoverHTML :: StdGen -> ByteString -> ByteString -> IO ByteString
crossoverHTML gen html1 html2 = do
  let logger = rootLoggerName
  debugM logger $ "Crossing over two html document:\n"
    ++ (prettyHtmlByteString html1)
    ++ "\n" ++ "with" ++ "\n"
    ++ (prettyHtmlByteString html2)
  let parent1 = bytestring2document html1
      parent2 = bytestring2document html2
  if (snd parent1 <= 4)
    then do
    getLine
    return html2      
    else if (snd parent2 <= 4)
         then return html1
         else crossoverIterate gen parent1 parent2


crossoverIterate :: StdGen -> (Document, Int) -> (Document, Int) -> IO ByteString 
crossoverIterate gen doc1@(fromDoc, docDepth1) doc2@(whereDoc, docDepth2) = do
  let logger = rootLoggerName
      (fromLabel, gen1) = randomR (4, docDepth1 - 1) gen
      (whereLabel, gen2) = randomR (4, docDepth2 - 1) gen1
      crossoveredDocument = crossoverDocuments fromLabel whereLabel fromDoc whereDoc
      crossoveredDocumentNoLabel = fmap removeLabelsFromDocument crossoveredDocument
  debugM logger $ "Crossover is applied at the node #" ++
    (show fromLabel) ++ " (" ++ (show docDepth1) ++ ") " ++
    " and #" ++
    (show whereLabel) ++  " (" ++ (show docDepth2) ++ ") "        
  case crossoveredDocumentNoLabel of
   Just document -> do let html   = toHtml document
                           result = BR.renderHtml html 
                       response <- askValidator result
                       case response of
                        Just _  -> do debugM logger $ "The Offsprint is inconsistent html"
                                      crossoverIterate gen2 doc1 doc2
                        Nothing -> do debugM logger $ "Crossover result:\n" ++ (renderHtml html)
                                      return result              
   Nothing       -> crossoverIterate gen2 doc1 doc2


crossoverDocuments :: Int -> Int -> Document -> Document -> Maybe Document      
crossoverDocuments labFrom labWhere docFrom docWhere = do
  elementInsert <- findElementInDocumentByLabel labFrom docFrom
  insertElementInDocumentByLabel labWhere elementInsert docWhere


-- | TEST: crossoverHTML (mkStdGen 5) (C.pack thtml) (C.pack thtml)
thtml1 :: ByteString
thtml1 = "<!DOCTYPE HTML><html><head><title>Title1</title></head><body><h1><a></a>Foo</h1><h1></h1></body></html>"

thtml2 :: ByteString
thtml2 = "<!DOCTYPE HTML><html><head><title>Title1</title></head><body><h1></h1></body></html>"

thtml3 :: ByteString
thtml3 = "<!DOCTYPE HTML><html><head><title>Title2</title></head><body><h2></h2></body></html>"
