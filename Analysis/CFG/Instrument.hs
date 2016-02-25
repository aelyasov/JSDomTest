module Analysis.CFG.Instrument (instrScript) where

import Language.ECMAScript3.Syntax (JavaScript(..), Statement(..), Expression(..), CatchClause(..), CaseClause(..), SourcePos, PrefixOp(..))
import Analysis.CFG.Data (SLab)
import Analysis.CFG.Util (getStmtLab, traceStmt, traceVar, getStmtLab, traceBranchDistance, getCCLab)
import Data.Default (def)
import Debug.Trace (trace)


instrScript :: JavaScript (SourcePos, SLab) -> JavaScript (SourcePos, SLab)
instrScript (Script l sts) = Script l $ instrStms sts 

instrStms :: [Statement (SourcePos, SLab)] -> [Statement (SourcePos, SLab)]
instrStms = instrStatements


instrStatements :: [Statement (SourcePos, SLab)] -> [Statement (SourcePos, SLab)]
instrStatements = concatMap instrStatement

instrStatement :: Statement (SourcePos, SLab) -> [Statement (SourcePos, SLab)]
instrStatement st@(BlockStmt    l sts)      = let blockLab = getStmtLab st
                                                  logStmt  = traceStmt traceVar blockLab
                                                  sts'     = instrStatements sts
                                              in  [logStmt, BlockStmt l sts'] 
instrStatement st@(EmptyStmt    l)            = [traceStmt traceVar $ getStmtLab st, st]
instrStatement st@(ExprStmt     l ex)         = [traceStmt traceVar $ getStmtLab st, st]
instrStatement st@(IfStmt l ex thenBl elseBl) = let BlockStmt l1 sts1 = thenBl
                                                    BlockStmt l2 sts2 = elseBl
                                                    branchLab         = getStmtLab st
                                                    logStmt           = traceStmt traceVar branchLab
                                                    thenBrDist        = traceBranchDistance branchLab (PrefixExpr def PrefixLNot ex)
                                                    elseBrDist        = traceBranchDistance branchLab ex
                                                    sts1'             = instrStatements sts1 
                                                    sts2'             = instrStatements sts2 
                                                    thenBl'           = BlockStmt l1 (thenBrDist:sts1')
                                                    elseBl'           = BlockStmt l2 (elseBrDist:sts2')
                                                in  [logStmt, IfStmt l ex thenBl' elseBl']
instrStatement st@(IfSingleStmt l ex thenBl) = let BlockStmt l1 sts1 = thenBl
                                                   ifLab             = getStmtLab st
                                                   blockLab          = getStmtLab thenBl
                                                   ifLogStmt         = traceStmt traceVar ifLab
                                                   blLogStmt         = traceStmt traceVar blockLab
                                                   thenBrDist        = traceBranchDistance ifLab (PrefixExpr def PrefixLNot ex)
                                                   sts1'             = instrStatements sts1
                                                   thenBl'           = BlockStmt l1 (blLogStmt:thenBrDist:sts1')
                                               in  [ifLogStmt, IfSingleStmt l ex thenBl']
instrStatement st@(SwitchStmt   l ex ccs)   = let ccs' = map instrumentCaseClause ccs
                                              in  [SwitchStmt   l ex ccs']           
instrStatement st@(WhileStmt    l ex whileBl) = let BlockStmt l1 sts1 = whileBl
                                                    branchLab         = getStmtLab st
                                                    logStmt           = traceStmt traceVar branchLab
                                                    sts1'             = instrStatements sts1 
                                                    whileBl'          = BlockStmt l1 (logStmt:sts1')
                                                in  [WhileStmt    l ex whileBl', logStmt]                                                   
instrStatement st@(DoWhileStmt  l dowhileBl ex)    = let BlockStmt l1 sts1 = dowhileBl
                                                         branchLab         = getStmtLab st
                                                         logStmt           = traceStmt traceVar branchLab
                                                         sts1'             = instrStatements sts1 
                                                         dowhileBl'        = BlockStmt l1 (logStmt:sts1')
                                                     in  [DoWhileStmt    l dowhileBl' ex, logStmt]                                                   
instrStatement st@(BreakStmt    l id)       = [st]
instrStatement st@(ContinueStmt l id)       = [st]
instrStatement st@(LabelledStmt l id labBl) = let BlockStmt l1 sts1 = labBl
                                                  sts1'   = instrStatements sts1
                                                  labBl' = BlockStmt l1 sts1' 
                                              in  [LabelledStmt l id labBl']
instrStatement st@(ForInStmt    l v ex forBl) = let BlockStmt l1 sts1 = forBl
                                                    branchLab         = getStmtLab st
                                                    logStmt           = traceStmt traceVar branchLab
                                                    sts1'             = instrStatements sts1 
                                                    forBl'          = BlockStmt l1 sts1'
                                                in  [logStmt, ForInStmt    l v ex forBl'] 
instrStatement st@(ForStmt    l i t inc forBl) = let BlockStmt l1 sts1 = forBl
                                                     branchLab         = getStmtLab st
                                                     logStmt           = traceStmt traceVar branchLab
                                                     sts1'             = instrStatements sts1 
                                                     forBl'            = BlockStmt l1 sts1'
                                                 in  [logStmt, ForStmt    l i t inc forBl']   
instrStatement st@(TryStmt      l tryBl catchBl finallyBl) = let BlockStmt l1 sts1 = tryBl
                                                                 sts1'     = instrStatements sts1 
                                                                 tryBl'     = BlockStmt l1 sts1'
                                                                 catchBl'   = maybe Nothing (Just . instrumentCatchClause) catchBl       
                                                                 finallyBl' = maybe Nothing (Just . head . instrStatement) finallyBl
                                                             in  [TryStmt l tryBl' catchBl' finallyBl']
instrStatement st@(ThrowStmt    l ex)       = [st]
instrStatement st@(ReturnStmt   l ex)       = [traceStmt traceVar $ getStmtLab st, st]
instrStatement st@(WithStmt     l ex st1)    = [WithStmt     l ex (head $ instrStatement st1)]
instrStatement st@(VarDeclStmt  l vs)       = [traceStmt traceVar $ getStmtLab st, st]
instrStatement st@(FunctionStmt l id ids sts) = [FunctionStmt l id ids $ (instrStatements sts ++ [traceStmt traceVar (-1)])]


instrumentCatchClause :: CatchClause (SourcePos, SLab) -> CatchClause (SourcePos, SLab)
instrumentCatchClause cc@(CatchClause l v catchBl) = let BlockStmt l1 sts1 = catchBl
                                                         sts1'   = instrStatements sts1
                                                         catchBl' = BlockStmt l1 sts1'
                                                     in  CatchClause l v catchBl'    
 

instrumentCaseClause :: CaseClause (SourcePos, SLab) -> CaseClause (SourcePos, SLab)
instrumentCaseClause cc@(CaseClause l ex sts) = let clauseLab = getCCLab cc
                                                    logStmt   = traceStmt traceVar clauseLab
                                                    sts'      = instrStatements sts
                                                in  CaseClause l ex (logStmt:sts')
instrumentCaseClause cc@(CaseDefault l sts)   = let clauseLab = getCCLab cc
                                                    logStmt   = traceStmt traceVar clauseLab
                                                    sts'      = instrStatements sts
                                                in  CaseDefault l (logStmt:sts')
                                                    

