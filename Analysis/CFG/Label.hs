module Analysis.CFG.Label where

import Control.Monad.State hiding (mapM)
import Data.Generics.Aliases (extM)
import SYB.Data.Generics.Schemes (everywhereM')
import Language.ECMAScript3.Syntax (JavaScript(..), Statement(..), Expression(..), CatchClause(..), CaseClause(..), SourcePos)
import Language.ECMAScript3.Syntax.Annotations (reannotate)
import Analysis.CFG.Data (SLab)


assignUniqueIdsSt :: JavaScript SourcePos -> State SLab (JavaScript (SourcePos, SLab))
assignUniqueIdsSt tree  = everywhereM' (return `extM` labelStatement `extM` labelCaseClause `extM` labelCatchClause `extM` labelExpression) treeAnn
    where 
      treeAnn :: JavaScript (SourcePos, SLab)
      treeAnn = reannotate (\a -> (a, 0)) tree


labelExpression :: Expression (SourcePos, SLab) -> State SLab (Expression (SourcePos, SLab))
labelExpression ex = do i <- get
                        let ex' = fmap (\(a, _) -> (a, i)) ex
                        return ex'


labelCatchClause :: CatchClause (SourcePos, SLab) -> State SLab (CatchClause (SourcePos, SLab))
labelCatchClause cc = do i <- get
                         put (i+1)
                         i' <- get
                         let cc' = fmap (\(a, _) -> (a, i')) cc
                         return cc'


-- | It should be possible to embed this function inside everywhereM 
labelCaseClause :: CaseClause (SourcePos, SLab) -> State SLab (CaseClause (SourcePos, SLab))
labelCaseClause cc = do i <- get
                        put (i+1)
                        i' <- get
                        let cc' = fmap (\(a, _) -> (a, i')) cc
                        return cc'


labelStatement :: Statement (SourcePos, SLab) -> State SLab (Statement (SourcePos, SLab))
labelStatement st = do i <- get
                       put (i+1)
                       i' <- get
                       let st' = fmap (\(a, _) -> (a, i')) st     
                       return st' -- trace ("\nst: \n" ++ (show st) ++ "\nst': \n" ++ (show st'))
