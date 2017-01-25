{-# LANGUAGE FlexibleContexts #-}

module Util.ParseJSSignature where

import Language.JavaScript.Parser
import Data.Generics

import Data.Functor.Identity

import Data.Maybe
import Data.Either
import Data.Char (isSpace)
import Data.List (isPrefixOf)

import Genetic.DataJS

parseJSSignature :: FilePath -> IO JSSig
parseJSSignature jsF = do
  jsNode <- parseFile jsF
  let sigComment = fromJust $ findSignatureComment jsNode
      sig = map str2JSType $ splitBy ':' $ removeCommentAnn $ stripSpaces sigComment
  return sig

  
findSignatureComment :: JSNode -> Maybe String
findSignatureComment = everything orElse (Nothing `mkQ` findTypeComment)


findTypeComment :: CommentAnnotation -> Maybe String
findTypeComment (CommentA _ str) | take 3 str == "/*t" = Just str
                                 | otherwise           = Nothing 
findTypeComment _ = Nothing

str2JSType :: String -> JSType
str2JSType str | "bool"   `isPrefixOf` str = JS_BOOL
               | "int"    `isPrefixOf` str = JS_INT
               | "float"  `isPrefixOf` str = JS_FLOAT
               | "string" `isPrefixOf` str = JS_STRING
               | "dom"    `isPrefixOf` str = JS_DOM
               | "["      `isPrefixOf` str = JSArray (str2JSType $ tail str)
               | otherwise = error $ str ++ " type isn't defined"

splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy y = foldr (\x (xs:xss) -> if x == y then []:(xs:xss) else (x:xs):xss) [[]]

removeCommentAnn :: String -> String
removeCommentAnn str = let strNoOpenComment = drop 3 str
                       in  take (length strNoOpenComment - 2) strNoOpenComment
                           
stripSpaces :: String -> String
stripSpaces = foldr (\s rest -> if isSpace s then rest else s:rest) [] 
