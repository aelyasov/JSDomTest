{-# LANGUAGE RankNTypes, OverloadedStrings #-}

module Html5C.Attributes
  ( assignIdsToDocumentRandomly
  , assignClassesToDocumentRandomly)
where

import Text.XML
import Text.HTML.DOM (parseLT)
import Data.Map.Lazy (insert, delete, (!))
import Text.XML.Label (labelXMLDocument, labelXMLElement, assignAttributeToElement, removeAttributeFromElementEverywhere)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Lazy as L
import System.Random
import Data.List hiding (delete, insert)
import Data.Function (on)
import Text.Blaze hiding  ((!))
import Text.Blaze.Html.Renderer.Utf8
import qualified Text.Blaze.Html.Renderer.Pretty as PP
import Data.ByteString.Lazy (ByteString)
import Control.Monad.State (State, evalState, get, put)
import SYB.Data.Generics.Schemes (everywhereM')
import Data.Generics.Aliases (extM)
import Debug.Trace
import Control.Monad (liftM, foldM)

assignIdsToDocumentRandomly :: [String] -> Document -> IO Document
assignIdsToDocumentRandomly = assignAttributesToDocumentRandomly "id"


assignClassesToDocumentRandomly :: [String] -> Document -> IO Document
assignClassesToDocumentRandomly = assignAttributesToDocumentRandomly "class"


assignAttributesToDocumentRandomly :: String -> [String] -> Document -> IO Document   
assignAttributesToDocumentRandomly attrName attrValues (Document prologue (Element html html_attrs [header, NodeElement body]) epilogue) = do
  gen <- newStdGen
  let (labeledElement, maxLabel) = labelXMLElement body
      randomLabeles              = take (maxLabel - 1) $ nub $ randomRs (1, maxLabel - 1) gen
      labsAndValues              = sortBy (compare `on` fst) $ zip randomLabeles attrValues
      elementNewValues          = foldl (\el (lab, val) -> assignAttributeToElement lab attrName val el) labeledElement labsAndValues
      elementNewValuesNoLabels  = removeAttributeFromElementEverywhere "label" elementNewValues
  return $ Document prologue (Element html html_attrs [header, NodeElement elementNewValuesNoLabels]) epilogue

      
test_html :: L.Text
test_html = "<!DOCTYPE HTML><html><head><title>Title</title></head><body><h1><a></a></h1><h1><a><h2></h2></a></h1></body></html>"

test1 :: L.Text
test1 = "<!DOCTYPE HTML><html><head><title>Title</title></head><body><div></div></body></html>"

-- test_span :: L.Text
test_span = "<!DOCTYPE HTML><html><head><title>Title</title></head><body><span></span></body></html>"



label_test = (assignIdsToDocumentRandomly ["foo", "bar"] $ parseLT test_html) >>= putStr . PP.renderHtml . toMarkup

