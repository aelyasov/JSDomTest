{-# LANGUAGE RankNTypes, OverloadedStrings #-}

module Html5C.Attributes where

import Text.XML
import Text.HTML.DOM (parseLT)
import Data.Map.Lazy (insert, delete, (!))
import Text.XML.Label (labelXMLElement)
import qualified Data.Text as T 
import qualified Data.Text.Lazy as L
import System.Random
import Data.List hiding (delete, insert)
import Text.Blaze hiding  ((!))
import Text.Blaze.Html.Renderer.Utf8
import qualified Text.Blaze.Html.Renderer.Pretty as PP
import Data.ByteString.Lazy (ByteString)
import Control.Monad.State (State, evalState, get, put)
import SYB.Data.Generics.Schemes (everywhereM')
import Data.Generics.Aliases (extM)
import Debug.Trace


assignIds2HtmlRandomly :: [String] -> L.Text -> IO ByteString
assignIds2HtmlRandomly ids html = do doc <- assignIds2DocumentRandomly ids $ parseLT html
                                     let markup = toMarkup doc
                                     putStr $ PP.renderHtml markup
                                     return $ renderHtml markup


assignIds2DocumentRandomly :: [String] -> Document -> IO Document   
assignIds2DocumentRandomly strs (Document prologue (Element html html_attrs [header, NodeElement body]) epilogue) =
  elementWithIds >>= \body_ -> return $ (Document prologue (Element html html_attrs [header, NodeElement body_]) epilogue)
  where
    limit = length strs
    (labeledElement, maxLabel) = labelXMLElement body
    randomLabeles  = newStdGen     >>= return . sort . take limit .  nub . if (maxLabel == 1) then const [] else randomRs (1, maxLabel - 1)
    labsAndIds     = randomLabeles >>= return . flip zip strs
    elementWithIds = labsAndIds    >>= return . evalState (everywhereM' (return `extM` assignNewIds) labeledElement)

    assignNewIds :: Element -> State [(Int, String)] Element
    assignNewIds el = do
      state <- get
      let attrs = elementAttributes el    
          (attrsWithId, stateNew) = if ((not $ null state) && (attrs ! "label" == (T.pack $ show $ fst $ head state)))
                                    then let ((_, id):state') = state
                                         in  (insert (Name "id" Nothing Nothing) (T.pack id) attrs, state')
                                    else (attrs, state)
          attrsWithoutLabel = delete "label" attrsWithId                   
      put stateNew
      return el{elementAttributes = attrsWithoutLabel}
      
test_html :: L.Text
test_html = "<!DOCTYPE HTML><html><head><title>Title</title></head><body><h1><a></a></h1><h1></h1></body></html>"


-- label_test = (assignIds2DocumentRandomly ["foo", "bar"] $ parseLT test_html) >>= putStr . renderHtml . toMarkup

