{-# LANGUAGE OverloadedStrings #-}

module Genetic.MutationJS where

import Data.ByteString.Lazy (ByteString)
import Text.XML.Pretty (prettyHtmlByteString, prettyDocument, bytestring2document)
import Text.XML.Label (removeLabelsFromDocument, deleteNodeInDocumentByLabel)
import Text.XML (Document(..))
import System.Log.Logger (rootLoggerName, infoM)
-- import System.Random (mkStdGen, StdGen, randomR)
import System.Random
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import qualified Text.Blaze.Html.Renderer.Utf8 as BR
import Text.Blaze.Html (toHtml)
import Html5C.ValidationTest (askValidator)


mutateHtml :: StdGen -> ByteString -> IO ByteString
mutateHtml gen html = do
  let logger = rootLoggerName
  infoM logger $ "Mutate the html document:\n" ++ (prettyHtmlByteString html)
  let parent = bytestring2document html
  if (snd parent <= 4)
    then return html
    else mutateIterate gen parent


mutateIterate :: StdGen -> (Document, Int) -> IO ByteString  
mutateIterate gen doc@(fromDoc, docDepth) = do
  let logger = rootLoggerName
      (fromLabel, gen1) = randomR (4, docDepth - 1) gen
      mutatedDocument = mutateDocument fromLabel fromDoc
      mutatedDocumentNoLabel = fmap removeLabelsFromDocument mutatedDocument
  infoM logger $ "Mutation is applied at the node #" ++ (show fromLabel) ++ " out of " ++ (show docDepth)  
  case mutatedDocumentNoLabel of
   Just document -> do let html   = toHtml document
                           result = BR.renderHtml html 
                       response <- askValidator result
                       case response of
                        Just _  -> do infoM logger $ "The Mutant is inconsistent html:\n" ++ (renderHtml html)
                                      mutateIterate gen1 doc
                        Nothing -> do infoM logger $ "Mutation result:\n" ++ (renderHtml html)
                                      return result              
   Nothing       -> mutateIterate gen1 doc


mutateDocument :: Int -> Document -> Maybe Document      
mutateDocument = deleteNodeInDocumentByLabel

-- | TEST: mutateHtml (mkStdGen 5) thtml
thtml :: ByteString
thtml = "<!DOCTYPE HTML><html><head><title>Title1</title></head><body><h1><a></a>Foo</h1><h1></h1></body></html>"

thtml1 :: ByteString
thtml1 = "<!DOCTYPE HTML><html><head><title>Title</title></head><body><div><div></div><div></div></div><div><div></div><div><div></div></div></div><div></div></body></html>"
