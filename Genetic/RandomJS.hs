{-# LANGUAGE OverloadedStrings #-}

module Genetic.RandomJS where

import Genetic.DataJS
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Text.XML.Pretty (prettyHtmlByteString)
import Control.Monad
import Html5C.ValidationTest
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text, pack)
import System.Log.Logger (rootLoggerName, infoM, debugM, noticeM)
import System.Random
import Control.Monad.Trans.Class
import Util.Debug (setCondBreakPoint)
import Data.Char (isHexDigit)
import Data.Maybe (isNothing, fromJust)


genRandomVal :: JSCPool -> JSType -> IO JSArg
genRandomVal pool tp = do
  debugM rootLoggerName  $ "Generating random value of type: " ++ (show tp)
  debugM rootLoggerName $ "Constant pool data: " ++ (show pool)
  genRandomVal' pool tp


genRandomVal' :: JSCPool -> JSType -> IO JSArg
genRandomVal' (ints, _, _)    JS_INT            = liftM IntJS    $ genRandomInt ints
genRandomVal' (_, strings, _) JS_STRING         = liftM StringJS $ genRandomString strings
genRandomVal' _               JS_BOOL           = liftM BoolJS   $ genRandomBool
genRandomVal' pool            JS_DOM            = liftM DomJS    $ genRandomDom pool
genRandomVal' pool            (JS_ARRAY jsType) = liftM ArrayJS  $ genRandomArray pool jsType

  
genRandomArray :: JSCPool -> JSType -> IO [JSArg]
genRandomArray pool jsType =
  do arraySize <- randomRIO (1, 5)
     debugM rootLoggerName $ "Generating random array of length: " ++ (show arraySize)
     randomArray <- mapM (const (genRandomVal' pool jsType)) [1..(arraySize :: Int)]
     debugM rootLoggerName $ show randomArray
     setCondBreakPoint
     return randomArray
  

-- | generate random integer value out of given diaposon
-- | TODO: can be extended to generate sometimes an arbitrary integer value
genRandomInt :: JSInts -> IO Int
genRandomInt ints = do
  randomInt <- generate $ if isNothing ints
                          then choose (-10, 10)
                          else frequency [(4, choose (-10, 10)), (1, elements $ fromJust ints)]
  debugM rootLoggerName $ "Generate random integer: " ++  show randomInt
  setCondBreakPoint
  return randomInt
                            

-- | generate random string value out of given diaposon
-- | TODO: can be extended to generate sometimes an arbitrary string value
genRandomString :: JSStrings -> IO String
genRandomString strs = do
  stringSize <- randomRIO (1, 5)
  debugM rootLoggerName $ "Generating random string of length: " ++ (show stringSize)
  randomStr <- generate $ if isNothing strs
                          then genFixedString stringSize
                          else frequency [(4, genFixedString stringSize), (1, elements $ fromJust strs)]
  debugM rootLoggerName $ show randomStr
  setCondBreakPoint
  return randomStr
    where
      genFixedString size = vectorOf size $ suchThat arbitrary isHexDigit


-- | generate random boolean value out of given diaposon
genRandomBool :: IO Bool
genRandomBool = do
  randomBool <- generate $ arbitrary
  debugM rootLoggerName $ show randomBool
  setCondBreakPoint
  return randomBool


genRandomDom :: JSCPool -> IO ByteString
genRandomDom pool = do
  html <- genValidHtml pool 
  debugM rootLoggerName $ prettyHtmlByteString html
  setCondBreakPoint
  return html
 
