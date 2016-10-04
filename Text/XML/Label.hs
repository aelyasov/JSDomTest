{-# LANGUAGE OverloadedStrings #-}

module Text.XML.Label ( labelXMLElement
                      , labelXMLDocument
                      , findElementInDocumentByLabel
                      , insertElementInDocumentByLabel
                      , deleteNodeInDocumentByLabel
                      , removeAttributeFromDocument
                      ) where

import Data.Generics (extM, everywhere', everything, somewhere, orElse, mkQ, mkMp, mkT, gsize)
import SYB.Data.Generics.Schemes (everywhereM')
import Text.XML (Element(..), Document (..), Name(..), Node(..))
import Control.Monad.State (State, runState, get, put)
import Data.Map.Lazy (insert, lookup, delete, empty)
import Data.Text (pack, Text)
import Prelude hiding (lookup)

labelXMLElement :: Element -> (Element, Int)
labelXMLElement el = runState (labelXMLElementState el) 0


labelXMLDocument :: Document -> (Document, Int)
labelXMLDocument doc = runState (labelXMLDocumentState doc) 0


labelXMLDocumentState :: Document -> State Int Document
labelXMLDocumentState = everywhereM' (return `extM` labelElement)

labelXMLElementState :: Element -> State Int Element
labelXMLElementState = everywhereM' (return `extM` labelElement)


labelElement :: Element -> State Int Element
labelElement el@(Element name attrs nodes) = do
  i <- get
  let attrs' = insert (Name "label" Nothing Nothing) (pack $ show i) attrs
  put (i+1)
  return el{elementAttributes = attrs'}


findElementInDocumentByLabel :: Int -> Document -> Maybe Element
findElementInDocumentByLabel lab = everything orElse (Nothing `mkQ` findElementByLabel lab)


findElementByLabel :: Int -> Element -> Maybe Element
findElementByLabel lab element =
  let attrs = elementAttributes element
  in  case lookup (Name "label" Nothing Nothing) attrs of
       Just l | l ==  (pack $ show lab) -> Just element
              | otherwise               -> Nothing
       Nothing                          -> Nothing


insertElementInDocumentByLabel :: Int -> Element -> Document -> Maybe Document
insertElementInDocumentByLabel lab element = somewhere (mkMp (replaceElementByLabel lab element))


replaceElementByLabel :: Int -> Element -> Element -> Maybe Element
replaceElementByLabel lab element elementToReplace = 
  let attrs = elementAttributes elementToReplace
  in  case lookup (Name "label" Nothing Nothing) attrs of
       Just l | l ==  (pack $ show lab) -> Just element
              | otherwise               -> Nothing
       Nothing                          -> Nothing


deleteNodeInDocumentByLabel :: Int -> Document -> Maybe Document
deleteNodeInDocumentByLabel lab = somewhere (mkMp (deleteNodeByLabel lab))
-- deleteNodeInDocumentByLabel lab doc = insertElementInDocumentByLabel lab (Element "br" empty []) doc  

deleteNodeByLabel :: Int -> Node -> Maybe Node
deleteNodeByLabel lab (NodeElement el) =
  case lookup (Name "label" Nothing Nothing) (elementAttributes el) of
   Just l | l ==  (pack $ show lab) -> Just $ NodeComment "skip"
          | otherwise               -> Nothing
   Nothing                          -> Nothing
deleteNodeByLabel _ node = Nothing


removeAttributeFromDocument :: Text -> Document -> Document
removeAttributeFromDocument attrName doc = everywhere' (mkT (removeAttributeFromElement attrName)) doc


removeAttributeFromElement :: Text -> Element -> Element
removeAttributeFromElement attrName element =
  let attrs = elementAttributes element
      attrsWithoutLabel = (Name attrName Nothing Nothing) `delete` attrs
  in  element{elementAttributes = attrsWithoutLabel}




