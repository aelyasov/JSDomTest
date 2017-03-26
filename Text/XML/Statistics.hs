{-# LANGUAGE OverloadedStrings #-}

module Text.XML.Statistics ( depthDocument
                           , sizeDocument
                           , Statistics(..)
                           , Size(..)
                           , Depth(..)
                           ) where

import Safe (maximumDef)
import Text.XML (Element(..), Document(..), Node(..))
import Genetic.DataJS (JSArg(..))
import Text.HTML.DOM (parseLBS)
import Data.List (intercalate)

-- ---------------------------------------------------------------
--                  Depth and Size of a Document
-- ---------------------------------------------------------------

depthDocument :: Document -> Int
depthDocument (Document _ element _) = depthElement element

depthElement :: Element -> Int
depthElement (Element _ _ nodes) = 1 + (maximumDef 0 $ map depthNode nodes) 

depthNode :: Node -> Int
depthNode (NodeElement element) = depthElement element
depthNode (NodeInstruction _)   = 1
depthNode (NodeContent _)       = 1
depthNode (NodeComment _)       = 1


sizeDocument :: Document -> Int
sizeDocument (Document _ element _) = sizeElement element

sizeElement :: Element -> Int
sizeElement (Element _ _ nodes) = 1 + (sum $ map sizeNode nodes)

sizeNode :: Node -> Int
sizeNode (NodeElement element) = sizeElement element
sizeNode (NodeInstruction _)   = 1
sizeNode (NodeContent _)       = 1
sizeNode (NodeComment _)       = 1

class Size a where
  size :: a -> Int

instance Size JSArg where
  size (IntJS i)    = 1
  size (StringJS s) = 1
  size (BoolJS b)   = 1
  size (DomJS s)    = let doc = parseLBS s in sizeDocument doc
  size (ArrayJS a)  = length a 

instance Size a => Size [a] where
  size xs = product $ map size xs

class Depth a where
  depth :: a -> Int

instance Depth JSArg where
  depth (IntJS i)    = 1
  depth (StringJS s) = 1
  depth (BoolJS b)   = 1
  depth (DomJS s)    = let doc = parseLBS s in depthDocument doc
  depth (ArrayJS a)  = 1 

instance Depth a => Depth [a] where
  depth xs = product $ map depth xs  


class Statistics a where
  showStatistics :: a -> String

instance Statistics a => Statistics [a] where
  showStatistics as = intercalate "\n" $ map showStatistics as 

instance Statistics JSArg where
  showStatistics (IntJS i)    = show i
  showStatistics (StringJS s) = show s
  showStatistics (BoolJS b)   = show b
  showStatistics (DomJS s)    = let doc = parseLBS s
                                in "depth: " ++
                                   (show $ depthDocument doc)
                                   ++ " size: " ++
                                   (show $ sizeDocument doc)
  showStatistics (ArrayJS arr) = show arr
  showStatistics jsarg = error $ (show jsarg) ++ " can't compute statistics"
