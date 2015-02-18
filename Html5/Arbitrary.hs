{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Html5.Arbitrary where

import Control.Monad.Trans.State

import Html5.Generators
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Html5.QuickCheck.Gen
import qualified Text.Blaze.Html.Renderer.Pretty as PP
import qualified Text.Blaze.Html.Renderer.Utf8  as Utf

import Util.Stack
import Text.Blaze.Html5

instance Arbitrary Html where
    arbitrary = evalStateT htmlGenState True

htmlGenState :: GenState HasMain Html
htmlGenState = resizeState 10 $ sizedState $ arbHtmlHTML empty 
                  
-- getNewHtml :: IO String
-- getNewHtml = return "<html></html>"
-- getNewHtml = generate arbitrary >>= return . PP.renderHtml
getNewHtml = generate arbitrary >>= return . Utf.renderHtml
