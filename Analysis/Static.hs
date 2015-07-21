module Analysis.Static where

import Data.Generics.Aliases
import Data.Generics.Schemes
import Genetic.DataJS
import Language.ECMAScript3.Syntax
import Language.ECMAScript3.Parser
import Data.Monoid
import Html5C.Tags

collectStaticRefs :: JavaScript SourcePos -> JSDoms
collectStaticRefs = everything mappend (mempty `mkQ` foldExpr)

foldExpr :: Expression SourcePos -> JSDoms
foldExpr (CallExpr _ (DotRef _ _ (Id _ fun)) [StringLit _ arg]) =
  case fun of
   "getElementById"         -> (mempty,            [arg],  mempty, mempty)
   "getElementsByClassName" -> (mempty,            mempty, mempty, [arg])
   "getElementsByName"      -> (mempty,            mempty, [arg],  mempty)
   "getElementsByTagName"   -> ([str2HtmlTag arg], mempty, mempty, mempty)
   _                        -> mempty
foldExpr _ = mempty

