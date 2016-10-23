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


collectStaticRefs :: JavaScript SourcePosLab -> JSCPool
collectStaticRefs = everything mappend (mempty `mkQ` foldExpr)

foldExpr :: Expression SourcePosLab -> JSCPool
foldExpr (CallExpr _ (DotRef _ _ (Id _ fun)) [StringLit _ arg]) =
  case fun of
   "createElement"          -> (mempty, mempty, ([str2HtmlTag arg], mempty, mempty, mempty)) 
   "getElementById"         -> (mempty, mempty, (mempty,            [arg],  mempty, mempty))
   "getElementsByClassName" -> (mempty, mempty, (mempty,            mempty, mempty, [arg]))
   "getElementsByName"      -> (mempty, mempty, (mempty,            mempty, [arg],  mempty))
   "getElementsByTagName"   -> (mempty, mempty, ([str2HtmlTag arg], mempty, mempty, mempty))
   _                        -> mempty
foldExpr (DotRef _ _ (Id _ fun)) | fun == "insertRow" = (mempty, mempty, ([TAG_TABLE], mempty, mempty, mempty))
                                 | otherwise          = mempty
foldExpr (StringLit _ str) = (mempty, [str], (mempty, mempty, mempty, mempty))
foldExpr (IntLit _ int)    = ([int], mempty, (mempty, mempty, mempty, mempty))   
foldExpr _ = mempty


-- | TODO: implement integer and string collection
collectConstantInfoJS :: JavaScript SourcePosLab -> JSCPool
collectConstantInfoJS = removeDuplicates . collectStaticRefs

removeDuplicates :: JSCPool -> JSCPool
removeDuplicates (ints, strings, (tags, ids, names, classes)) = (nub ints, nub strings, (nub (TAG_DIV:tags), nub ids, nub names, nub classes))
-- removeDuplicates (ints, strings, (tags, ids, names, classes)) = (nub ints, nub strings, (nub (tags), nub ids, nub names, nub classes))
