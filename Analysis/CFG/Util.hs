module Analysis.CFG.Util where

import Safe (headMay, headNote)
import Data.Maybe (maybe, fromJust)
import Data.List (find, groupBy, nub)
import qualified Data.IntMap as IntMap
import Language.ECMAScript3.Syntax 
import qualified Language.ECMAScript3.PrettyPrint as JSP
import qualified Text.PrettyPrint.Leijen as PPL
import Analysis.CFG.Data (SLab)
import Data.Default (Default, def)
import Data.Graph.Analysis.Algorithms.Commons (cyclesIn')
import Data.Graph.Inductive.PatriciaTree
import Analysis.CFG.Data
import Data.Function (on)


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
traceBranchDistance lab ex = ExprStmt def (CallExpr def (DotRef def (VarRef def (Id def branchDistance)) (Id def "push")) [ObjectLit def [(PropId def (Id def "label"), IntLit def lab), (PropId def (Id def "distance"), expr2objFun ex)]])

-- ExprStmt def (CallExpr def (DotRef def (VarRef def (Id def branchDistance)) (Id def "push")) [ArrayLit def [IntLit def lab, expr2objFun ex]])


traceLoopMap :: Default a => SLab -> Expression a -> Statement a
traceLoopMap loopLabel loopExpr = ExprStmt def (AssignExpr def OpAssign (LBracket def (VarRef def (Id def loopMap)) (IntLit def loopLabel)) (expr2objFun loopExpr))


importVar       = "instrument."
traceVar        = "trace"
branchDistance  = "branchDistance"
loopMap         = "loopMap" 
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


evalLoopTree :: LoopTree -> Int
evalLoopTree (Node iter head body) = iter * (1 + (sum $ map evalLoopTree body))
evalLoopTree (Leaf _) = 1


updateLoopIterMap ::  LoopIterationMap -> GPath -> LoopIterationMap
updateLoopIterMap iterMap path = foldr (IntMap.adjust (flip (-) 1)) iterMap path


estimatePath :: GPath -> LoopMaxSizeMap -> Int
estimatePath path loopMap = sum $ map (\n -> 1 + IntMap.findWithDefault 0 n loopMap) path


list2LoopTree ::  LoopIterationMap -> GPath -> LoopTree
list2LoopTree _ [] = error "list2LoopTree: loop can't be empty"
list2LoopTree iterMap (head:body) =
  case (IntMap.lookup head iterMap) of
   Just iterN -> Node iterN head (map Leaf body)
   Nothing    -> error "list2LoopTree: there is no info about max iteration number for given loop"


buildLoopMaxSizeMap :: Gr NLab ELab -> LoopIterationMap -> LoopMaxSizeMap
buildLoopMaxSizeMap gr iterMap =
  let allLoopTrees = findAllLoopTreesInGraph gr iterMap
  in  foldr (\loops loopMap ->
              let loopHead = getLoopHead $ head loops
                  maxLoopPath = maximum $ map evalLoopTree loops  
              in  IntMap.insert loopHead maxLoopPath loopMap
            ) IntMap.empty allLoopTrees 


findAllLoopTreesInGraph :: Gr NLab ELab -> LoopIterationMap -> [[LoopTree]]
findAllLoopTreesInGraph gr iterMap =
  let allLoops          = cyclesIn' gr
      groupAllLoops     = groupBy ((==) `on` head) $ reverse allLoops
      groupAllLoopTrees = map (map $ list2LoopTree iterMap) groupAllLoops
  in  insertAllLoopsInAllLoops groupAllLoopTrees

insertOneLoopTreeInAnother :: LoopTree -> LoopTree -> (LoopTree, Bool)
insertOneLoopTreeInAnother oneTree (Node iter headAnother body) =
  let (body', flags) = unzip $ map (insertOneLoopTreeInAnother oneTree) body
  in  (Node iter headAnother body', or flags)
insertOneLoopTreeInAnother oneTree (Leaf node) | getLoopHead oneTree == node = (oneTree, True)
                                               | otherwise                   = (Leaf node, False)

insertOneLoopInLoops :: LoopTree -> [LoopTree] -> ([LoopTree], Bool)
insertOneLoopInLoops loop loops =
  let (loops', flags) = unzip $ map (insertOneLoopTreeInAnother loop) loops
  in  (loops', or flags)
     

insertLoopsInLoops :: [LoopTree] -> [LoopTree] -> ([LoopTree], Bool)       
insertLoopsInLoops loops1 loops2 =
  let (loops', flags) = unzip $ map (flip insertOneLoopInLoops loops2) loops1
  in  (nub $ concat loops', or flags)


insertLoopsInAllLoops :: [LoopTree] -> [[LoopTree]] -> [[LoopTree]]
insertLoopsInAllLoops _ [] = []
insertLoopsInAllLoops loops1 (loops2:allLoops) =
  let (loops, flag) = insertLoopsInLoops loops1 loops2
  in  if  flag
      then loops:allLoops
      else loops2:insertLoopsInAllLoops loops1 allLoops


insertAllLoopsInAllLoops :: [[LoopTree]] -> [[LoopTree]]
insertAllLoopsInAllLoops []      = []
insertAllLoopsInAllLoops (loops:allLoops) = loops : insertAllLoopsInAllLoops (insertLoopsInAllLoops loops allLoops)


findLoopIteration :: Int -> LoopIterationMap -> Int
findLoopIteration i mp  = IntMap.findWithDefault 1 i mp

buildLoopSizeMap :: Gr NLab ELab -> LoopSizeMap
buildLoopSizeMap gr =
  let cycles      = cyclesIn' gr
      cycleGroups = groupBy ((==) `on` head) cycles
      foldCycleGroup cycleGr resultMap =
        let groupLead = head $ head cycleGr
            maxLength = maximum $ map length cycleGr
        in  IntMap.insert groupLead maxLength resultMap    
  in  foldr foldCycleGroup IntMap.empty cycleGroups


mkLoopTransitiveClosure :: [[GPath]] -> [[GPath]]
mkLoopTransitiveClosure [loop]       = [loop]
mkLoopTransitiveClosure (loop:loops) = mkLoopTransitiveClosure (injectLoopsInGroups loop loops)


injectLoopsInPath :: [GPath] -> GPath -> ([GPath], Bool) 
injectLoopsInPath loops path =
  let leadElem = headNote "injectLoopsInPath: no loops to inject"
               $ headNote "injectLoopsInPath: first loop has no elements"
               loops
      (pref, suff) = break (==leadElem) path
  in  if (null suff)
      then ([path], False)
      else (map (\l -> pref ++ l ++ tail suff) loops, True)


injectLoopsInGroup :: [GPath] -> [GPath] -> ([GPath], Bool)
injectLoopsInGroup loops group =
  let (group' , cond) = unzip $ map (injectLoopsInPath loops) group
  in  (concat group', or cond)


injectLoopsInGroups :: [GPath] -> [[GPath]] -> [[GPath]]       
injectLoopsInGroups _ [] = []
injectLoopsInGroups loops (group:groups) =
  let (group', cond) =  injectLoopsInGroup loops group
  in  if cond
      then group':groups
      else group:injectLoopsInGroups loops groups

