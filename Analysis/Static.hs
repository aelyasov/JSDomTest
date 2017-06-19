{-# LANGUAGE OverloadedStrings #-}

module Analysis.Static where

import Data.Generics.Aliases (mkQ)
import Data.Generics.Schemes (everything)
import Genetic.DataJS (JSCPool)
import Language.ECMAScript3.Syntax
import Language.ECMAScript3.Parser
import Data.Monoid
import Html5C.Tags 
import Data.List (nub)
import Mutation.Dom.Operators
import Debug.Trace
import Control.Monad (liftM)


collectStaticRefs :: JavaScript SourcePosLab -> JSCPool
collectStaticRefs = everything mappend (mempty `mkQ` foldExpr)

foldExpr :: Expression SourcePosLab -> JSCPool
foldExpr (CallExpr _ (DotRef _ _ (Id _ fun)) [StringLit _ arg]) =
  case fun of
   "createElement"          -> (mempty, mempty, (Just [str2HtmlTag arg], mempty, mempty, mempty)) 
   "getElementById"         -> (mempty, mempty, (mempty,            Just [arg],  mempty, mempty))
   "getElementsByClassName" -> (mempty, mempty, (mempty,            mempty, mempty, Just [arg]))
   "getElementsByName"      -> (mempty, mempty, (mempty,            mempty, Just [arg],  mempty))
   "getElementsByTagName"   -> (mempty, mempty, (Just [str2HtmlTag  arg], mempty, mempty, mempty))
   _                        -> mempty
foldExpr (DotRef _ _ (Id _ fun)) | fun == "insertRow" = (mempty, mempty, (Just [TAG_TABLE], mempty, mempty, mempty))
                                 | otherwise          = mempty
foldExpr (StringLit _ str) = (mempty, Just [str], (mempty, mempty, mempty, mempty))
foldExpr (IntLit _ int)    = (Just [int], mempty, (mempty, mempty, mempty, mempty))   
foldExpr (InfixExpr _ infixOp leftExpr rightExpr)
  | infixOp `elem` [OpEq, OpNEq] = extractClassName leftExpr rightExpr <> extractClassName rightExpr leftExpr
foldExpr _ = mempty


extractClassName :: Expression SourcePosLab -> Expression SourcePosLab -> JSCPool
extractClassName expr (StringLit _ className)
  | isExprEndsWithClassName expr = (mempty, mempty, (mempty, mempty, mempty, Just [className]))
  | otherwise                    = mempty
extractClassName _ _ = mempty

      
isExprEndsWithClassName :: Expression SourcePosLab -> Bool    
isExprEndsWithClassName (DotRef _ _ (Id _ "className")) = True
isExprEndsWithClassName _                               = False

                                                          
-- | TODO: implement integer and string collection
collectConstantInfoJS :: JavaScript SourcePosLab -> JSCPool
collectConstantInfoJS = removeDuplicates . collectStaticRefs


removeDuplicates :: JSCPool -> JSCPool
removeDuplicates (ints, strings, (tags, ids, names, classes)) = ( liftM nub ints
                                                                , liftM nub strings
                                                                , ( liftM nub tags
                                                                  , liftM nub ids
                                                                  , liftM nub names
                                                                  , liftM nub classes))

