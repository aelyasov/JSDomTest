{-# LANGUAGE FlexibleContexts #-}

module Util.ParseJSSignature where

import Language.JavaScript.Parser
import Data.Generics

import qualified Text.Parsec.Prim as P
import Text.Parsec.Combinator
import Text.Parsec.Char
import Data.Functor.Identity

import Data.Maybe
import Data.Either

import Genetic.DataJS

data ArgJS = JS_Int | JS_String | JS_Bool | JS_Dom | JS_Float
             deriving Show


parseJSSignature :: FilePath -> IO JSSig
parseJSSignature jsF = do
  jsNode <- parseFile jsF
  let sigCom = fromJust $ findSignatureComment jsNode  
      types  = either (const []) id $ P.parse pSigCom "" sigCom
  return $ foldr (\x xs -> maybe xs (\a -> a:xs)  (str2argJS x)) [] types

  
findSignatureComment :: JSNode -> Maybe String
findSignatureComment = everything orElse (Nothing `mkQ` findTypeComment)


findTypeComment :: CommentAnnotation -> Maybe String
findTypeComment (CommentA _ str) | take 3 str == "/*t" = Just str
                                 | otherwise           = Nothing 
findTypeComment _ = Nothing


pSigCom :: P.Stream s Identity Char => P.Parsec s () [String]
pSigCom = manyTill (choice [ string "/*t"
                           , string "int"
                           , string "bool"
                           , string "string"
                           , string "float"
                           , string "dom"
                           , string " "
                           , string ":"
                           ]
                   ) (string "*/") 


str2argJS :: String -> Maybe JSType
str2argJS str = case str of
                  "int"    -> Just JS_INT
                  "bool"   -> Just JS_BOOL
                  "string" -> Just JS_STRING
                  "dom"    -> Just JS_DOM
                  "float"  -> Just JS_FLOAT
                  _        -> Nothing


