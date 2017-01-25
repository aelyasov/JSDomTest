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


genRandomVal :: JSCPool -> JSType -> IO JSArg
genRandomVal pool tp = do
  debugM rootLoggerName  $ "Generating random value of type: " ++ (show tp)
  debugM rootLoggerName $ "Constant pool data: " ++ (show pool)
  debugM rootLoggerName $ "Generated random value is:"
  genRandomVal' pool tp


genRandomVal' :: JSCPool -> JSType -> IO JSArg
genRandomVal' (ints, _, _)    JS_INT    = liftM IntJS    $ genRandomInt ints
genRandomVal' (_, strings, _) JS_STRING = liftM StringJS $ genRandomString strings
genRandomVal' _               JS_BOOL   = liftM BoolJS   $ genRandomBool
genRandomVal' (_, _, doms)    JS_DOM    = liftM DomJS    $ genRandomDom doms
genRandomVal' pool            (JSArray arType) =
  do arrSize <- randomRIO (0, 10)
     arr <- mapM (const (genRandomVal' pool arType)) [1..(arrSize :: Int)]
     return $ ArrayJS arr


-- | generate random integer value out of given diaposon
-- | TODO: can be extended to generate sometimes an arbitrary integer value
genRandomInt :: JSInts -> IO Int
genRandomInt ints = do
  randomInt <- generate $ case ints of
                            [] -> arbitrary
                            _  -> oneof [arbitrary, elements ints]
  debugM rootLoggerName (show randomInt)
  return randomInt
                            

-- | generate random string value out of given diaposon
-- | TODO: can be extended to generate sometimes an arbitrary string value
genRandomString :: JSStrings -> IO String
genRandomString strs = do
  gstr <- arbitraryMy
  generate $ case strs of
               [] -> return gstr
               _  -> oneof [elements strs]
  where
    arbitraryMy :: IO String
    arbitraryMy = do g <- newStdGen
                     return $ take 10 $ randomRs ('a','z') g


-- | generate random boolean value out of given diaposon
genRandomBool :: IO Bool
genRandomBool = generate $ arbitrary


genRandomDom :: JSDoms -> IO ByteString
genRandomDom doms = do
  html <- genValidHtml doms 
  debugM rootLoggerName (prettyHtmlByteString html)
  setCondBreakPoint
  return html
 
