module Genetic.RandomJS where

import Genetic.DataJS
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Control.Monad
import Html5C.ValidationTest
import Data.ByteString.Lazy (ByteString)
import System.Log.Logger (rootLoggerName, infoM)
import System.Random
import Control.Monad.Trans.Class


genRandomVal :: JSCPool -> JSType -> IO JSArg
genRandomVal pool tp = do
  infoM rootLoggerName $ "Generating random value for a type: " ++ (show tp)
  genRandomVal' pool tp

genRandomVal' :: JSCPool -> JSType -> IO JSArg
genRandomVal' (ints, _, _)    JS_INT    = liftM IntJS    $ genRandomInt ints
genRandomVal' (_, strings, _) JS_STRING = liftM StringJS $ genRandomString strings
genRandomVal' _               JS_BOOL   = liftM BoolJS   $ genRandomBool
genRandomVal' (_, _, doms)    JS_DOM    = liftM DomJS    $ genRandomDom doms

-- | generate random integer value out of given diaposon
-- | TODO: can be extended to generate sometimes an arbitrary integer value
genRandomInt :: JSInts -> IO Int
genRandomInt ints = generate $ case ints of
                                [] -> arbitrary
                                _  -> oneof [arbitrary, elements ints]

-- | generate random string value out of given diaposon
-- | TODO: can be extended to generate sometimes an arbitrary string value
genRandomString :: JSStrings -> IO String
genRandomString strs = do gstr <- arbitraryMy
                          generate $ case strs of
                            [] -> return gstr
                            _  -> oneof [return gstr, elements strs]
  where
    arbitraryMy :: IO String
    arbitraryMy = do g <- newStdGen
                     return $ take 10 $ randomRs ('a','z') g


-- | generate random boolean value out of given diaposon
genRandomBool :: IO Bool
genRandomBool = generate $ arbitrary


genRandomDom :: JSDoms -> IO ByteString
genRandomDom = genValidHtml

