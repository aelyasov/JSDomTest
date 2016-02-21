module Analysis.Static where

import Data.Generics.Aliases
import Data.Generics.Schemes
import Genetic.DataJS
import Language.ECMAScript3.Syntax
import Language.ECMAScript3.Parser
import Data.Monoid
import Html5C.Tags
import Data.List (nub)
-- import Data.Data
import Mutation.Dom.Operators

import Debug.Trace


collectStaticRefs :: JavaScript SourcePosLab -> JSDoms
collectStaticRefs = everything mappend (mempty `mkQ` foldExpr)

foldExpr :: Expression SourcePosLab -> JSDoms
foldExpr (CallExpr _ (DotRef _ _ (Id _ fun)) [StringLit _ arg]) =
  case fun of
   "createElement"          -> ([str2HtmlTag arg], mempty, mempty, mempty) 
   "getElementById"         -> (mempty,            [arg],  mempty, mempty)
   "getElementsByClassName" -> (mempty,            mempty, mempty, [arg])
   "getElementsByName"      -> (mempty,            mempty, [arg],  mempty)
   "getElementsByTagName"   -> ([str2HtmlTag arg], mempty, mempty, mempty)
   _                        -> mempty
foldExpr _ = mempty


-- | TODO: implement integer and string collection
collectConstantInfoJS :: JavaScript SourcePosLab -> JSCPool
collectConstantInfoJS js = ([], [], removeDuplicates $ collectStaticRefs js)

removeDuplicates :: JSDoms -> JSDoms
removeDuplicates (tags, ids, names, classes) = (nub tags, nub ids, nub names, nub classes)
