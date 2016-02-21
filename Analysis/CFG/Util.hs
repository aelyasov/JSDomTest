module Analysis.CFG.Util where

import Safe (headMay)
import Data.Maybe (maybe, fromJust)
import Data.List (find)
import Language.ECMAScript3.Syntax (Statement(..), Expression(..), CaseClause(..), Id(..), SourcePos, InfixOp(..), PrefixOp(..))
import qualified Language.ECMAScript3.PrettyPrint as JSP
import qualified Text.PrettyPrint.Leijen as PPL
import Analysis.CFG.Data (SLab)
import Data.Default (Default, def)


getNextStLab :: SLab -> [Statement (SourcePos, SLab)] -> SLab
getNextStLab defL sts = maybe defL (getStmtLab . upwrapDoWhileStmt) $ headMay sts
    where upwrapDoWhileStmt :: Statement (SourcePos, SLab) -> Statement (SourcePos, SLab)
          upwrapDoWhileStmt (DoWhileStmt _ st _) = st
          upwrapDoWhileStmt st = st


getStmtLab :: Statement (SourcePos, SLab) -> SLab
getStmtLab = snd . getStmtData


getCCLab :: CaseClause (SourcePos, SLab) -> SLab
getCCLab = snd . getCCData


getCCData :: CaseClause (SourcePos, SLab) -> (SourcePos, SLab)
getCCData (CaseClause  cd _ _) = cd 
getCCData (CaseDefault cd _)   = cd


-- | TODO: define this function generically
getStmtData :: Statement (SourcePos, SLab) -> (SourcePos, SLab)
getStmtData (BlockStmt    sd _ )      = sd
getStmtData (EmptyStmt    sd)         = sd
getStmtData (ExprStmt     sd _)       = sd
getStmtData (IfStmt       sd _ _ _)   = sd
getStmtData (IfSingleStmt sd _ _)     = sd
getStmtData (SwitchStmt   sd _ _)     = sd
getStmtData (WhileStmt    sd _ _)     = sd
getStmtData (DoWhileStmt  sd _ _)     = sd
getStmtData (BreakStmt    sd _)       = sd
getStmtData (ContinueStmt sd _)       = sd
getStmtData (LabelledStmt sd _ _)     = sd
getStmtData (ForInStmt    sd _ _ _)   = sd
getStmtData (ForStmt      sd _ _ _ _) = sd
getStmtData (TryStmt      sd _ _ _)   = sd
getStmtData (ThrowStmt    sd _)       = sd
getStmtData (ReturnStmt   sd _)       = sd
getStmtData (WithStmt     sd _ _)     = sd
getStmtData (VarDeclStmt  sd _)       = sd
getStmtData (FunctionStmt sd _ _ _)   = sd


prettyNodeCFG :: SLab -> String -> Maybe (Expression a) -> String
prettyNodeCFG l info expr = show l ++ ": " ++ info ++  maybe "" (show . JSP.prettyPrint) expr

prettyPrintMaybe :: JSP.Pretty a => Maybe a -> PPL.Doc
prettyPrintMaybe Nothing = PPL.empty
prettyPrintMaybe (Just x) = JSP.prettyPrint x

findIdLab :: [(SLab, String)] -> String -> SLab
findIdLab tbl lab = fst $ fromJust $ find (\(i, l) -> l == lab) tbl

traceStmt :: Default a => String -> Int -> Statement a
traceStmt varName lab = ExprStmt def (CallExpr def (DotRef def (VarRef def (Id def varName)) (Id def "push")) [IntLit def lab])

traceBranchDistance :: Default a => SLab -> Expression a -> Statement a
traceBranchDistance lab ex = ExprStmt def (CallExpr def (DotRef def (VarRef def (Id def branchDistance)) (Id def "push")) [ArrayLit def [IntLit def lab, expr2objFun ex]])

importVar       = "instrument."
traceVar        = "_trace_"
branchDistance  = "_branchDistance_"
failureConst    = "_K_"
distanceFun     = importVar ++ "abs"
distanceZero    = importVar ++ "absZero"
distanceNegZero = importVar ++ "absNegZero"

expr2objFun :: Default a => Expression a -> Expression a
expr2objFun expr@(InfixExpr l inOp ex1 ex2) = 
    let minExpr a b = CallExpr def (DotRef def (VarRef def (Id def "Math")) (Id def "min")) [a, b]
        absExpr a b = CallExpr def (VarRef def (Id def distanceFun)) [a, b] 
    in  case inOp of
          OpLT   -> InfixExpr def OpAdd (absExpr ex1 ex2) (VarRef def (Id def failureConst))
          OpLEq  -> absExpr ex1 ex2
          OpGT   -> InfixExpr def OpAdd (absExpr ex1 ex2) (VarRef def (Id def failureConst))
          OpGEq  -> absExpr ex1 ex2
          OpEq   -> absExpr ex1 ex2
          OpNEq  -> VarRef def (Id def failureConst)
          OpLAnd -> InfixExpr l OpAdd (expr2objFun ex1) (expr2objFun ex2) 
          OpLOr  -> minExpr (expr2objFun ex1)  (expr2objFun ex2)
          OpMul  -> expr
          OpDiv  -> expr
          OpMod  -> expr
          OpSub  -> expr
          OpAdd  -> expr
          -- OpBAnd
          -- OpBXor
          -- OpBOr
          -- OpZfRShift
          -- OpSpRShift
          -- OpLShift
          -- OpStrictNEq
          -- OpStrictEq 
          -- OpIn
          -- OpInstanceof
-- expr2objFun (RegexpLit l str b1 b2)        = undefined 
-- expr2objFun (BracketRef l ex1 ex2)         = undefined
-- expr2objFun (NewExpr l ex exs)             = undefined
expr2objFun ex@(StringLit l str)           = expr2objFun (InfixExpr def OpNEq ex (StringLit def ""))
expr2objFun ex@(NumLit l val)              = expr2objFun (InfixExpr def OpNEq ex (NumLit def 0))
expr2objFun ex@(IntLit l val)              = expr2objFun (InfixExpr def OpNEq ex (IntLit def 0))
expr2objFun ex@(BoolLit l val)             = expr2objFun (InfixExpr def OpNEq ex (BoolLit def False))
expr2objFun (NullLit l)                    = VarRef def (Id def failureConst)
expr2objFun (ArrayLit l exs)               = VarRef def (Id def failureConst)
expr2objFun (ObjectLit l exs)              = VarRef def (Id def failureConst)
expr2objFun (ThisRef l)                    = VarRef def (Id def failureConst)
expr2objFun ex@(VarRef l id)               = CallExpr def (VarRef def (Id def distanceZero)) [ex]
expr2objFun ex@(DotRef l ex1 id)           = CallExpr def (VarRef def (Id def distanceZero)) [ex]
expr2objFun (PrefixExpr l prOp ex)         = 
    case prOp of
      PrefixLNot -> case ex of
                      BoolLit _ _               -> expr2objFun $ propogateNeg ex
                      PrefixExpr _ PrefixLNot _ -> expr2objFun $ propogateNeg ex
                      InfixExpr _ _ _ _         -> expr2objFun $ propogateNeg ex
                      _                         -> CallExpr def (VarRef def (Id def distanceNegZero)) [ex] 
-- expr2objFun (UnaryAssignExpr l unAsOp val) = undefined
-- expr2objFun (CondExpr l ex1 ex2 ex3)       = undefined
-- expr2objFun (AssignExpr l asOp val ex)     = undefined
-- expr2objFun (ListExpr l exs)               = undefined
-- expr2objFun (CallExpr l  ex exs)           = undefined
-- expr2objFun (FuncExpr l fName fArgs sts)   = undefined
expr2objFun ex = ex


propogateNeg :: Default a => Expression a -> Expression a
propogateNeg (BoolLit l val)            = BoolLit l (not val)
propogateNeg ex@(PrefixExpr l prOp exs) = 
    case prOp of
      PrefixLNot -> exs
      _          -> PrefixExpr def PrefixLNot ex
propogateNeg (InfixExpr l inOp ex1 ex2) = 
    case inOp of      
      OpLT   -> InfixExpr l OpGEq ex1 ex2
      OpLEq  -> InfixExpr l OpGT  ex1 ex2
      OpGT   -> InfixExpr l OpLEq ex1 ex2
      OpGEq  -> InfixExpr l OpLT  ex1 ex2
      OpEq   -> InfixExpr l OpNEq ex1 ex2
      OpNEq  -> InfixExpr l OpEq  ex1 ex2
      OpLAnd -> InfixExpr l OpLOr  (propogateNeg ex1) (propogateNeg ex2)
      OpLOr  -> InfixExpr l OpLAnd (propogateNeg ex1) (propogateNeg ex2)
propogateNeg ex = PrefixExpr def PrefixLNot ex
-- propogateNeg ex@(NullLit l)             = PrefixExpr def PrefixLNot ex
-- propogateNeg ex@(ArrayLit l exs)        = PrefixExpr def PrefixLNot ex
-- propogateNeg ex@(ObjectLit l exs)       = PrefixExpr def PrefixLNot ex
-- propogateNeg ex@(ThisRef l)                    = PrefixExpr def PrefixLNot ex
-- propogateNeg ex@(VarRef l id)                  = PrefixExpr def PrefixLNot ex
-- propogateNeg ex@(StringLit l str)              = PrefixExpr def PrefixLNot ex    
-- propogateNeg ex@(RegexpLit l str b1 b2)        = PrefixExpr def PrefixLNot ex
-- propogateNeg ex@(NumLit l val)                 = PrefixExpr def PrefixLNot ex
-- propogateNeg ex@(IntLit l val)                 = PrefixExpr def PrefixLNot ex           
-- propogateNeg ex@(UnaryAssignExpr l unAsOp val) = PrefixExpr def PrefixLNot ex
-- propogateNeg ex@(CondExpr l ex1 ex2 ex3)       = PrefixExpr def PrefixLNot ex
-- propogateNeg ex@(AssignExpr l asOp val e2)     = PrefixExpr def PrefixLNot ex
-- propogateNeg ex@(ListExpr l exs)               = PrefixExpr def PrefixLNot ex
-- propogateNeg ex@(CallExpr l  exs1 exs2)        = PrefixExpr def PrefixLNot ex
-- propogateNeg ex@(FuncExpr l fName fArgs sts)   = PrefixExpr def PrefixLNot ex
