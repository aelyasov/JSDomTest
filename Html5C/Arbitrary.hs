{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, StandaloneDeriving #-}

module Html5C.Arbitrary where

import Control.Monad.Trans.State

import Html5C.Tags
import Html5C.Context

import Config.Default
import Config.Data

import Html5C.Generators
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Html5C.QuickCheck.Gen
import Test.QuickCheck
import qualified Text.Blaze.Html.Renderer.Pretty as PP
import qualified Text.Blaze.Html.Renderer.Utf8  as Utf

import Util.Stack
import Text.Blaze.Html5

import Debug.Trace


instance Arbitrary Html where
  arbitrary = evalStateT htmlGenState defaultState


htmlGenState :: GenHtmlState
htmlGenState = do st <- get
                  let depth = getDepth st
                  resizeState depth arbHtmlHTML
                      

-- htmlGenState :: HtmlRandomGenConfig -> GenHtmlState
-- htmlGenState cfg = let depth     = maxDepth cfg
--                        tagsFreq  = tagsFrequency cfg
--                        tags      = tagsKnown cfg
--                        names     = namesKnown cfg
--                        ids       = idsKnown cfg
--                        classes   = classesKnown cfg
--                        mode      = genMode cfg
--                    in  resizeState depth 
--                            $ sizedState 
--                            $ arbHtmlHTML 
--                            $ push (CLikely tags) empty 

                  
-- getNewHtml :: IO String
-- getNewHtml = return "<html></html>"
-- getNewHtml = generate arbitrary >>= return . PP.renderHtml
getNewHtml = generate arbitrary >>= return . Utf.renderHtml
