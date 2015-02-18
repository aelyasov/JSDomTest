{-# LANGUAGE OverloadedStrings #-}

module Genetic.CrossoverJS where

import Text.XmlHtml
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Text as T
import Blaze.ByteString.Builder
import qualified Data.Map.Lazy as Map
import Control.Monad.State
import System.Random
import Data.Either
import Html5C.ValidationTest
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy (ByteString)
-- import qualified Data.ByteString as B
-- import Data.ByteString (ByteString)
import Debug.Trace
import Data.Maybe
import Data.List

crossoverHTML :: StdGen -> ByteString -> ByteString -> IO ByteString
crossoverHTML g html1 html2 = do
  let doc1@(HtmlDocument _ _ nodes1) = string2HtmlDoc html1 
      doc2@(HtmlDocument _ _ nodes2) = string2HtmlDoc html2
      parent1 = labelHtmlDocument nodes1
      parent2 = labelHtmlDocument nodes2
      
      crossoverIter :: StdGen -> ([Node], Int) -> ([Node], Int) -> IO ByteString
      crossoverIter gen par1@(nodeL1, docDepth1) par2@(nodeL2, docDepth2) = do 
        let (randDepth1, gen1) = randomR (0 :: Int, docDepth1-1) gen
            (randDepth2, gen2) = randomR (0 :: Int, docDepth2-1) gen1
            crossNodes = removeNodeLabels $ crossoverNodes (trace (show randDepth1) randDepth1) (trace (show randDepth2) randDepth2) nodeL1 nodeL2
            docOffspring = doc1{docContent = crossNodes}
        response <- askValidator $ toLazyByteString $ render docOffspring
        -- print response
        case response  of
          Just _  -> do print "Offsprint is inconsistent html"
                        crossoverIter gen2 par1 par2
          Nothing -> return $ toLazyByteString $ render docOffspring
  crossoverIter g parent1 parent2


-- | The crossoverDocuments function assumes that the documents are labeled
crossoverNodes :: Int -> Int -> [Node] -> [Node] -> [Node]
crossoverNodes cutp1 cutp2 par1 par2 = case findNodeAt cutp1 par1 of
                                        Just subpar -> insertNodeAt cutp2 subpar par2
                                        Nothing     -> error "crossoverDocuments: can't fint subtree"                                          
    where
      findNodeAt :: Int -> [Node] -> Maybe Node
      findNodeAt i []           = Nothing
      findNodeAt i (node:nodes) = 
          case node of
            Element tag atts nds | checkAttr i atts -> Just node
                                 | otherwise        -> case findNodeAt i nds of
                                                         Just nd -> Just nd
                                                         Nothing -> findNodeAt i nodes       
            otherwise                               -> findNodeAt i nodes 


      checkAttr i = isJust . find (\(attr, val) -> attr == "_lab_" && (T.unpack val) == show i)

      insertNodeAt :: Int -> Node -> [Node] -> [Node]
      insertNodeAt i subnode []           = []
      insertNodeAt i subnode (node:nodes) = 
          case node of
            Element tag atts nds | checkAttr i atts -> subnode:nodes
                                 | otherwise        -> (Element tag atts (insertNodeAt i subnode nds)):(insertNodeAt i subnode nodes)
            otherwise                               -> node:(insertNodeAt i subnode nodes)
              

                     
string2HtmlDoc :: ByteString -> Document
string2HtmlDoc html = case parseHTML "" (B.toStrict html) of
                        Right doc -> doc
                        Left _    -> error "string doesn't contain an html document"


-- | The function removeDocumentLabels traverses the node tree erasing all lables attached to the tree                       
removeNodeLabels :: [Node] -> [Node]
removeNodeLabels [] = []
removeNodeLabels (node:nodes) = case node of
                                  Element tag atts nds -> Element tag (removeLab atts) (removeNodeLabels nds):removeNodeLabels nodes
                                  nd                   -> nd:removeNodeLabels nodes
    where
      removeLab = filter (\(l, _) -> l /= "_lab_")


labelHtmlDocument :: [Node] -> ([Node], Int)
labelHtmlDocument conts = runState (mapM labelHtmlNode conts) 0 


labelHtmlNode :: Node -> State Int Node
labelHtmlNode (Element tag attrs childs) = do 
  lab <- get
  put $ lab + 1
  nodes <- mapM labelHtmlNode childs
  let attrs' = ("_lab_", T.pack $ show lab):attrs
  return $ Element tag attrs' nodes
labelHtmlNode node = return node -- TODO: consider assigning labels to the other types of nodes


-- | H.parseLBS $ C.pack test_html
-- | TEST: crossoverHTML (mkStdGen 5) (C.pack thtml) (C.pack thtml)
thtml = "<!DOCTYPE HTML>\n<html><head><title>Title</title><base href=\".\" target=\"_blank\"></head><body itemscope=\"\" itemtype=\"http://schema.org/WebPage\"><h1><a></a>A2A2</h1><h1></h1></body></html>"


divhtml = "<!DOCTYPE HTML>\n<html><head><title>Title</title><base href=\".\" target=\"_blank\"><style></style><style></style><noscript></noscript></head><body itemscope=\"\" itemtype=\"http://schema.org/WebPage\"><div><div></div><div><div>PHRASING CONTENT</div></div></div><div>DIV2</div><div></div></body></html>"
