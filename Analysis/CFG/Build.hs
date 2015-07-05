{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeOperators, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, StandaloneDeriving, DeriveGeneric, DeriveDataTypeable #-}

module Analysis.CFG.Build where

import Language.ECMAScript3.Parser (parseFromFile)
import Language.ECMAScript3.Analysis.LabelSet
import Language.ECMAScript3.Syntax
import Language.ECMAScript3.Syntax.Annotations
import qualified Language.ECMAScript3.PrettyPrint as JSP
import qualified Text.PrettyPrint.Leijen as PPL

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Arrow

import Data.Traversable
import Control.Applicative
-- import Control.Arrow
import Control.Monad.State hiding (mapM)
import Prelude hiding (mapM)

import Data.Typeable
import Data.Data
import Data.Default.Class

import Data.Generics
import GHC.Generics 

import Data.Maybe 
import Data.List
import Data.Function
import Safe

import Data.Graph.Inductive
-- import Data.Graph.Inductive.PatriciaTree
-- import Data.Graph.Inductive.Graph
-- import Data.Graph.Inductive.Query.Monad
import Data.Graph.Inductive.Dot -- graph vesualisation with dot tool

import System.Process

-- import Data.Generics.Uniplate.Operations

import Debug.Trace
import Safe

-- testLabel :: JavaScript a -> JavaScript a
testLabel js =
    let jsa = reannotate (\a -> (a, Set.empty))
    in  annotateLabelSets snd (\labs (a, ls) -> (a, labs `Set.union` ls)) $ jsa js

testLabel1 :: JavaScript SourcePos -> JavaScript (SourcePos, Set Label)
testLabel1 = annotateLabelSets snd (\labs -> second $ Set.union labs) . reannotate (\a -> (a, Set.empty))


type SLab = Int

assignUniqueIdsSt :: Int -> JavaScript SourcePos -> (JavaScript (SourcePos, SLab), SLab) 
assignUniqueIdsSt first tree =
  (returnA *** \i -> i-1) $ runState (mapM f tree) first
  where f :: (Typeable a, Data a, Show a) => a -> State Int (a, Int)
        f x = do i <- get
                 -- case (cast x) :: Maybe (Statement SourcePos) of
                 --   Just (x' :: Statement SourcePos) -> do trace (show $ typeOf x) $ put (i+1)
                 --                                          return (x, i)
                 --   Nothing -> trace (show x) $ return (x, i)
                 if (show $ typeOf x) == "Statement SourcePos"
                 then do trace (show $ typeOf x) $ put (i+1)
                         return (x, i)
                 else trace (show $ dataTypeOf x) $ return (x, i)

-- assignUniqueIdsSt :: Int -> JavaScript SourcePos -> (JavaScript SourcePos,  Int) 
-- assignUniqueIdsSt first tree = runState (transformBiM fkjkk tree) first
--      where
--         f :: (Data a, Typeable a) => a -> State Int a
--         f a = do i <- get
--                  case cast a of
--                    Just (x :: Statement SourcePos) -> do trace "then" $ put (i+1)
--                                                          return a
--                    Nothing -> trace "else" $ return a 
--   (returnA *** \i -> i-1) $


assignUniqueIdsSt_ :: JavaScript SourcePos -> State SLab (JavaScript (SourcePos, SLab))
-- assignUniqueIdsSt_ tree  = everywhereM' (mkM labelStatement) treeAnn
assignUniqueIdsSt_ tree  = everywhereM' (return `extM` labelStatement `extM` labelCaseClause `extM` labelCatchClause `extM` labelExpression) treeAnn
    where 
      treeAnn :: JavaScript (SourcePos, SLab)
      treeAnn = reannotate (\a -> (a, 0)) tree

-- `extM` labelExpression

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


tt :: JavaScript SourcePos -> JavaScript (SourcePos, Int)
tt = reannotate (\a -> (a, 0))


-- | Monadic variation on everywhere
everywhereM' :: (Monad m) => GenericM m -> GenericM m
everywhereM' f x = do x' <- f x
                      gmapM (everywhereM' f) x'


-- ==============================================================================
-- applications of applyOnce where replaced by fmap
-- left here as a use-case of generic deriving
-- ==============================================================================

-- deriving instance GHC.Generics.Generic1 (Statement)
-- deriving instance GHC.Generics.Generic1 (CaseClause)
-- deriving instance GHC.Generics.Generic1 (CatchClause)
-- deriving instance GHC.Generics.Generic1 (Expression)


-- applyOnce :: (Generic1 f, ApplyOnce' (Rep1 f)) => (a -> a) -> f a -> f a
-- applyOnce f = to1 . applyOnce' f . from1

-- applyOnce :: Functor f => (a -> a) -> f a -> f a
-- applyOnce = fmap
                      
-- class ApplyOnce' f where
--   applyOnce' :: (a -> a) -> f a -> f a


-- instance (ApplyOnce' f, ApplyOnce' g) => ApplyOnce' (f :+: g) where
--   applyOnce' f (L1 x) = L1 $ applyOnce' f x
--   applyOnce' f (R1 x) = R1 $ applyOnce' f x


-- instance ApplyOnce' f => ApplyOnce' (f :*: g) where
--   applyOnce' f (x :*: y) = applyOnce' f x :*: y


-- instance ApplyOnce' f => ApplyOnce' (f :.: g) where
--   applyOnce' f (Comp1 x) = Comp1 (applyOnce' f x)


-- -- instance ApplyOnce' Par1 where
-- --   applyOnce' f (Par1 x) = Par1 (f x)


-- instance ApplyOnce' (K1 i c) where
--   applyOnce' f (K1 x) = K1 (applyOnce' f x)


-- instance ApplyOnce' f => ApplyOnce' (M1 i c f) where
--   applyOnce' f (M1 x) = M1 (applyOnce' f x)


-- instance ApplyOnce' U1 where
--   applyOnce' _ U1 = U1
-- ==============================================================================
            

-- | there are following types of CFG nodes: 
-- |  e = entry
-- |  x = exit 
-- |  b = branch
-- |  l = loop
-- |  p = expression
-- |  etc.
type NLab = String
type ELab = String
type CFG = Gr NLab ELab


enrichCollectedEdges :: [Statement (SourcePos, SLab)] -> ([LNode NLab], [LEdge ELab])
enrichCollectedEdges stms = 
    let (nodes', edges') = collectEdges stms (0,-1,[]) (-1)
    in  ((0, "entry"):(-1, "exit"):nodes', (0, 1, ""):edges')


-- | the function collectEdges takes the list of statements, pair of labels (block entry and exit), end label and returns edges together with the nodes of the CFG graph
collectEdges :: [Statement (SourcePos, SLab)] -> (Int, Int, [(SLab, String)]) -> Int ->  ([LNode NLab], [LEdge ELab])
collectEdges [] _ _ = ([], []) -- FIX: the label is not necessary "x"
collectEdges (st:sts) labs@(loopInNd, loopOutNd, stLabs) endNd = 
    let curNd  = getStmtLab st
        sucNd  = getNextStLab endNd sts
        edgeIn = (curNd, sucNd, "")
        (nodes, edges) = collectEdges sts labs endNd
    in  case st of
          BlockStmt _ sts1 -> let blockNd = (curNd, show curNd ++ ": " ++ "block")
                                  (blockNds, blockEds) = collectEdges sts1 labs sucNd
                                  blockInNd = getNextStLab sucNd sts1
                              in  (blockNd:(blockNds ++ nodes), (curNd, blockInNd, ""):(blockEds ++ edges))
          EmptyStmt _      -> let emptyNd = (curNd, "empty")
                              in (emptyNd:nodes, edgeIn:edges)
          ExprStmt _ expr     -> let exprNd = (curNd, prettyNodeCFG curNd "" expr)
                                 in (exprNd:nodes, edgeIn:edges)
          IfStmt _ expr sts1 sts2 -> let ifNd     = (curNd, (show curNd) ++ ": if " ++ (show $ PPL.tupled [JSP.prettyPrint expr]))
                                         thenInNd = getStmtLab sts1
                                         elseInNd = getStmtLab sts2
                                         (thenNds, thenEds) = collectEdges [sts1] labs sucNd
                                         (elseNds, elseEds) = collectEdges [sts2] labs sucNd
                                     in (ifNd:(thenNds ++ elseNds ++ nodes), (curNd, thenInNd, "then"):(curNd, elseInNd, "else"):(thenEds ++ elseEds ++ edges))
          IfSingleStmt _ expr sts1 -> let ifNd     = (curNd, (show curNd) ++ ": if " ++ (show $ PPL.tupled [JSP.prettyPrint expr]))
                                          thenInNd = getStmtLab sts1
                                          (thenNds, thenEds) = collectEdges [sts1] labs sucNd
                                      in  (ifNd:(thenNds ++ nodes), edgeIn:(curNd, thenInNd, "then"):(thenEds ++ edges))
          SwitchStmt _ expr ccs -> let switchNd = (curNd, (show curNd) ++ ": switch " ++ (show $ PPL.tupled [JSP.prettyPrint expr]))
                                       caseInNd = maybe endNd getCCLab $ headMay ccs
                                       (caseNds, caseEds) = collectEdges_CaseClause  ccs (curNd, sucNd, stLabs)
                                   in  (switchNd:(caseNds ++ nodes), (caseEds ++ edges))
          WhileStmt _ expr sts1 -> let whileNd   = (curNd, (show curNd) ++ ": while " ++ (show $ PPL.tupled [JSP.prettyPrint expr]))
                                       whileInNd = getStmtLab sts1
                                       whileInEd = (curNd, whileInNd, "inWhile")
                                       whileEndEd = (curNd, sucNd, "outWhile")
                                       (whileNds, whileEds) = collectEdges [sts1] (curNd, sucNd, stLabs) curNd
                                                              -- sucNd
                                   in  (whileNd:(whileNds ++ nodes), whileInEd:whileEndEd:(whileEds ++ edges))
          DoWhileStmt _ sts1 expr -> let dowhileNd    = (curNd, (show curNd) ++ ": dowhile " ++ (show $ PPL.tupled [JSP.prettyPrint expr]))
                                         dowhileInNd  = getStmtLab sts1
                                         dowhileOutEd  = (curNd, sucNd, "outDoWhile")
                                         dowhileLoopEd = (curNd, dowhileInNd, "inDoWhile")
                                         (dowhileNds, dowhileEds) = collectEdges [sts1] (curNd, sucNd, stLabs) curNd
                                     in  (dowhileNd:(dowhileNds ++ nodes),  dowhileLoopEd:dowhileOutEd:(dowhileEds ++ edges))
          BreakStmt _ id        -> let breakNd = (curNd, show curNd ++ ": " ++ "break")
                                       jumpNd = maybe loopOutNd (findIdLab stLabs . unId) id
                                   in  (breakNd:nodes, (curNd, jumpNd, "jump"):edges)
          ContinueStmt _ id     -> let contNd = (curNd, show curNd ++ ": " ++ "continue")
                                       jumpNd = maybe loopInNd (findIdLab stLabs . unId) id
                                   in  (contNd:nodes, (curNd, jumpNd, "jump"):edges)
          LabelledStmt _ id sts1 -> let labelNd = (curNd, show curNd ++ ": label " ++ (show $ JSP.prettyPrint id))
                                        (labelNds, labelEds) = collectEdges [sts1] (loopInNd, loopOutNd, (curNd, unId id):stLabs) sucNd
                                        labelInNd = getStmtLab sts1
                                    in  (labelNd:(labelNds ++ nodes), (curNd, trace ("labelInNd: " ++ show labelInNd) labelInNd, ""):(labelEds ++ edges))
          ForInStmt _ var colls sts1 -> let forinNd   = (curNd, show curNd ++ ": for " ++ (show $ JSP.prettyPrint var) ++ " in " ++ (show $ JSP.prettyPrint colls))
                                            forinInNd = getStmtLab sts1
                                            forinInEd = (curNd, forinInNd, "inFor")
                                            forinEndEd = (curNd, sucNd, "outFor")
                                            (forinNds, forinEds) = collectEdges [sts1] (curNd, sucNd, stLabs) curNd
                                        in  (forinNd:(forinNds ++ nodes), forinInEd:forinEndEd:(forinEds ++ edges))
          ForStmt _ init test incr sts1 -> let forNd   = (curNd, show curNd ++ ": for (" ++ (show $ JSP.prettyPrint init) ++ "; " ++ (show $ prettyPrintMaybe test) ++ "; " ++ (show $ prettyPrintMaybe incr) ++ ")")
                                               forInNd = getStmtLab sts1
                                               forInEd = (curNd, forInNd, "inFor")
                                               forEndEd = (curNd, sucNd, "outFor")
                                               (forNds, forEds) = collectEdges [sts1] (curNd, sucNd, stLabs) curNd
                                           in  (forNd:(forNds ++ nodes), forInEd:forEndEd:(forEds ++ edges)) 
          ReturnStmt _ _       -> let returnNd = (curNd, "return") -- TODO: edge from return to the consecutive node 
                                      exitEd   = (curNd, -1, "toExit")
                                  in (returnNd:nodes, exitEd:edges)
          VarDeclStmt _ vdecls     -> let varDeclNd = (curNd, (show curNd) ++ ": " ++ (show $ PPL.hcat $ PPL.punctuate PPL.comma $ map JSP.prettyPrint vdecls))
                                      in  (varDeclNd:nodes, edgeIn:edges)
          FunctionStmt _ fn args sts1 -> let funcDeclNd = (curNd, (show curNd) ++ ": function " ++ (show $ JSP.prettyPrint fn PPL.<> (PPL.tupled $ map JSP.prettyPrint args)))
                                             (funcDeclNds, funcDeclEds) = collectEdges sts1 labs sucNd
                                             funcDeclInNd = getNextStLab sucNd sts1
                                         in if curNd == 1
                                            then (funcDeclNd:(funcDeclNds ++ nodes), (curNd, funcDeclInNd, ""):(funcDeclEds ++ edges))
                                            else (funcDeclNd:nodes, edgeIn:edges)



prettyPrintMaybe :: JSP.Pretty a => Maybe a -> PPL.Doc
prettyPrintMaybe Nothing = PPL.empty
prettyPrintMaybe (Just x) = JSP.prettyPrint x


findIdLab :: [(SLab, String)] -> String -> SLab
findIdLab tbl lab = fst $ fromJust $ find (\(i, l) -> l == lab) tbl


prettyNodeCFG :: Int -> String -> Expression a -> String
prettyNodeCFG l info expr = show l ++ ": " ++ info ++  (show $ JSP.prettyPrint expr)


collectEdges_CaseClause :: [CaseClause (SourcePos, SLab)] -> (Int, Int, [(SLab, String)]) -> ([LNode NLab], [LEdge ELab])
collectEdges_CaseClause [] _ = ([], [])
collectEdges_CaseClause (cc:ccs) labs@(switchNd, endNd, stLabs) = 
    let curCC          = getCCLab cc
        caseInEd       = (switchNd, curCC, "toCase") 
        (nodes, edges) = collectEdges_CaseClause ccs labs     
    in case cc of
         CaseClause _ expr sts -> let ccNd       = (curCC, (show curCC) ++ ": case " ++ (show $ JSP.prettyPrint expr))
                                      sucCaseNd  = maybe endNd getCCLab $ headMay ccs
                                      caseInNd   = getNextStLab endNd sts
                                      caseInStEd = (curCC, caseInNd, "inCase")
                                      (caseNds, caseEds) = collectEdges sts labs sucCaseNd
                                  in  (ccNd:(caseNds ++ nodes), caseInEd:caseInStEd:(caseEds ++ edges))                 
         CaseDefault _ sts  -> let ccNd       = (curCC, "default")
                                   sucCaseNd  = maybe endNd getCCLab $ headMay ccs
                                   caseInNd   = getNextStLab endNd sts
                                   caseInStEd = (curCC, caseInNd, "fallCase")
                                   (caseNds, caseEds) = collectEdges sts labs sucCaseNd
                               in  (ccNd:(caseNds ++ nodes), caseInEd:caseInStEd:(caseEds ++ edges))


ppCFG :: FilePath -> IO ()
ppCFG fp = do
  stms <- liftM (unJavaScript . fst . flip runState 0 . assignUniqueIdsSt_) $ parseFromFile fp
  print stms
  let coll@(nodes', edges') = collectEdges stms (0,-1,[]) (-1)
      nodes = (0, "entry"):(-1, "exit"):nodes'
      edges = (0, 1, ""):edges'
      graph = mkGraph (trace (show nodes) nodes) (trace (show edges) edges) :: Gr NLab ELab
      dot = showDot (fglToDotString graph)
  -- showDot (fglToDot graph)
  prettyPrint graph
  writeFile "file.dot" dot
  system "dot -Tpng -ofile.png file.dot"
  return ()


mkTestCFG :: FilePath -> IO (Gr NLab ELab)
mkTestCFG fp = do
  stms <- liftM (unJavaScript . fst . flip runState 0 . assignUniqueIdsSt_) $ parseFromFile fp
  -- print stms
  let coll@(nodes', edges') = collectEdges stms (0,-1,[]) (-1)
      nodes = (0, "entry"):(-1, "exit"):nodes'
      edges = (0, 1, ""):edges'
      graph = mkGraph (trace (show nodes) nodes) (trace (show edges) edges) :: Gr NLab ELab
  return graph


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
    -- where getSLab :: (SourcePos, SLab) -> SLab
    --       getSLab  = snd
                     

getNextStLab :: SLab -> [Statement (SourcePos, SLab)] -> SLab
getNextStLab defL sts = maybe defL (getStmtLab . upwrapDoWhileStmt) $ headMay sts
    where upwrapDoWhileStmt :: Statement (SourcePos, SLab) -> Statement (SourcePos, SLab)
          upwrapDoWhileStmt (DoWhileStmt _ st _) = st
          upwrapDoWhileStmt st = st


importVar :: String
importVar       = "instrument"
failureConst    = importVar ++ "." ++ "_K_"
traceVar        = importVar ++ "." ++ "_trace_"
branchDistance  = importVar ++ "." ++ "_branchDistance_"
distanceFun     = importVar ++ "." ++ "abs"
distanceZero    = importVar ++ "." ++ "absZero"
distanceNegZero = importVar ++ "." ++ "absNegZero"



importDecl :: Default a => String -> Statement a
importDecl var = VarDeclStmt def [VarDecl def (Id def var) (Just (CallExpr def (VarRef def (Id def "require")) [StringLit def "./js/instrumentLib.js"]))]


declTraceVar :: Default a => String -> Statement a 
declTraceVar varName = VarDeclStmt def [VarDecl def (Id def varName) (Just (ArrayLit def [IntLit def 1]))]


declFailureConst ::  Default a => String -> Statement a
declFailureConst varName = VarDeclStmt def [VarDecl def (Id def varName) (Just (IntLit def 0))]


declBrDistVar :: Default a => String -> Statement a 
declBrDistVar varName = VarDeclStmt def [VarDecl def (Id def varName) (Just (ArrayLit def []))]


traceStmt :: Default a => String -> Int -> Statement a
traceStmt varName lab = ExprStmt def (CallExpr def (DotRef def (VarRef def (Id def varName)) (Id def "push")) [IntLit def lab])


instrCaseClause :: CaseClause (SourcePos, SLab) -> [CaseClause (SourcePos, SLab)]
instrCaseClause cc@(CaseClause l ex sts) = let clauseLab = getCCLab cc
                                               logStmt = traceStmt traceVar clauseLab
                                           in  [CaseClause l ex (logStmt:sts)]
instrCaseClause cc@(CaseDefault l sts)   = let clauseLab = getCCLab cc
                                               logStmt = traceStmt traceVar clauseLab
                                           in  [CaseDefault l (logStmt:sts)]


instrStmt :: Statement (SourcePos, SLab) -> [Statement (SourcePos, SLab)]
instrStmt st@(BlockStmt l sts) = let blockLab = getStmtLab st
                                     logStmt  = traceStmt traceVar blockLab
                                 in  [BlockStmt l (logStmt:sts)]
instrStmt st@(IfStmt l ex thenBl elseBl) = let BlockStmt l1 sts1 = thenBl
                                               BlockStmt l2 sts2 = elseBl
                                               branchLab  = getStmtLab st
                                               logStmt    = traceStmt traceVar branchLab
                                               thenBrDist = traceBranchDistance branchLab (PrefixExpr def PrefixLNot ex)
                                               elseBrDist = traceBranchDistance branchLab ex
                                               thenBl' = BlockStmt l1 (logStmt:thenBrDist:sts1)
                                               elseBl' = BlockStmt l2 (logStmt:elseBrDist:sts2)
                                           in  [IfStmt l ex thenBl' elseBl']
instrStmt st@(IfSingleStmt l ex thenBl) = let BlockStmt l1 sts1 = thenBl
                                              branchLab  = getStmtLab st
                                              logStmt    = traceStmt traceVar branchLab
                                              thenBrDist = traceBranchDistance branchLab (PrefixExpr def PrefixLNot ex)
                                              thenBl' = BlockStmt l1 (logStmt:thenBrDist:sts1)
                                          in  [IfSingleStmt l ex thenBl', logStmt]
instrStmt st = [st]


traceBranchDistance :: Default a => SLab -> Expression a -> Statement a
traceBranchDistance lab ex = ExprStmt def (CallExpr def (DotRef def (VarRef def (Id def branchDistance)) (Id def "push")) [ArrayLit def [IntLit def lab, expr2objFun ex]])  


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

   

instrStatements :: [Statement (SourcePos, SLab)] -> [Statement (SourcePos, SLab)]
instrStatements = concatMap instrStatement


instrStatement :: Statement (SourcePos, SLab) -> [Statement (SourcePos, SLab)]
instrStatement st@(BlockStmt    l sts)      = let blockLab = getStmtLab st
                                                  logStmt  = traceStmt traceVar blockLab
                                                  sts'     = instrStatements sts
                                              in  [BlockStmt l (logStmt:sts')] 
instrStatement st@(EmptyStmt    l)            = [st]
instrStatement st@(ExprStmt     l ex)         = [st]
instrStatement st@(IfStmt l ex thenBl elseBl) = let BlockStmt l1 sts1 = thenBl
                                                    BlockStmt l2 sts2 = elseBl
                                                    branchLab         = getStmtLab st
                                                    logStmt           = traceStmt traceVar branchLab
                                                    thenBrDist        = traceBranchDistance branchLab (PrefixExpr def PrefixLNot ex)
                                                    elseBrDist        = traceBranchDistance branchLab ex
                                                    sts1'             = instrStatements sts1 
                                                    sts2'             = instrStatements sts2 
                                                    thenBl'           = BlockStmt l1 (logStmt:thenBrDist:sts1')
                                                    elseBl'           = BlockStmt l2 (logStmt:elseBrDist:sts2')
                                                in  [IfStmt l ex thenBl' elseBl']
instrStatement st@(IfSingleStmt l ex thenBl) = let BlockStmt l1 sts1 = thenBl
                                                   branchLab         = getStmtLab st
                                                   logStmt           = traceStmt traceVar branchLab
                                                   thenBrDist        = traceBranchDistance branchLab (PrefixExpr def PrefixLNot ex)
                                                   thenBl'           = BlockStmt l1 (logStmt:thenBrDist:sts1)
                                               in  [IfSingleStmt l ex thenBl', logStmt]
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
                                                    forBl'          = BlockStmt l1 (logStmt:sts1')
                                                in  [ForInStmt    l v ex forBl', logStmt] 
instrStatement st@(ForStmt    l i t inc forBl) = let BlockStmt l1 sts1 = forBl
                                                     branchLab         = getStmtLab st
                                                     logStmt           = traceStmt traceVar branchLab
                                                     sts1'             = instrStatements sts1 
                                                     forBl'          = BlockStmt l1 (logStmt:sts1')
                                                 in  [ForStmt    l i t inc forBl', logStmt]   
instrStatement st@(TryStmt      l tryBl catchBl finallyBl) = let BlockStmt l1 sts1 = tryBl
                                                                 sts1'     = instrStatements sts1 
                                                                 tryBl'     = BlockStmt l1 sts1'
                                                                 catchBl'   = maybe Nothing (Just . instrumentCatchClause) catchBl       
                                                                 finallyBl' = maybe Nothing (Just . head . instrStatement) finallyBl
                                                             in  [TryStmt l tryBl' catchBl' finallyBl']
instrStatement st@(ThrowStmt    l ex)       = [st]
instrStatement st@(ReturnStmt   l ex)       = [st]
instrStatement st@(WithStmt     l ex st1)    = [WithStmt     l ex (head $ instrStatement st1)]
instrStatement st@(VarDeclStmt  l vs)       = [st]
instrStatement st@(FunctionStmt l id ids sts) = [FunctionStmt l id ids $ instrStatements sts]


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


instrStms :: [Statement (SourcePos, SLab)] -> [Statement (SourcePos, SLab)]
-- instrStms = everything (++) (instrU `extQ` instrStmt) 
instrStms = instrStatements


instrScript :: JavaScript (SourcePos, SLab) -> JavaScript (SourcePos, SLab)
instrScript (Script l sts) = Script l $ instrStms sts


instrU :: Data a => a -> [Statement (SourcePos, SLab)]
instrU _ = []

-- id `extQ` instrStmt `extQ` instrCaseClause

intrumentTest :: FilePath -> IO ()
intrumentTest fp = do 
  stms <- liftM (unJavaScript . fst . flip runState 0 . assignUniqueIdsSt_) $ parseFromFile fp
  print stms 
  let stmtInstr = instrStatements stms
  print stmtInstr
  print $ JSP.prettyPrint (stmtInstr :: [Statement (SourcePos, SLab)])   


instance Default (SourcePos, SLab) where
  def = (def :: SourcePos, 0)


-- VarDeclStmt def [VarDecl def (Id def "_trace_") (Just (ArrayLit def []))]
-- ExprStmt def (CallExpr def (DotRef def (VarRef def (Id def "_trace_")) (Id def "push")) [IntLit def 1])
-- ExprStmt def (CallExpr def (DotRef def (VarRef def (Id def "console")) (Id def "log")) [StringLit def "test"])


-- | The findSourceBranch function is invoked with the argument representing path equal to the revirsed actual path
findSourceBranch :: Gr NLab ELab -> SLab -> [SLab] -> [[SLab]]
findSourceBranch gr tL run = 
    let rgr             = grev gr
        pathsFromTarget = filter ((0==) . last) $ pathTree $ match tL rgr
        fstRunIntersect r p = let (pref, suf) =  break (`elem`r)  p
                              in  if suf == [0] then Nothing else Just ((head suf):reverse pref)
    in nub $ mapMaybe (fstRunIntersect run) pathsFromTarget


compApproachLevel :: Gr NLab ELab -> SLab -> [SLab] -> (Int, SLab)
compApproachLevel gr tL path = (foldr (\x xs -> if (isBranch x) && (isCritical x) then xs+1 else xs) 0 path - 1, trace ("path: " ++ show path) $ path `at` 0)
    where
      isBranch   n = let out = outdeg gr n in out `notElem` [0,1]
      isCritical n = let paths = filter (((-1)==) . last) $ pathTree $ match n gr 
                     in  or $ map (tL`notElem`) paths


approachLevel :: Gr NLab ELab -> SLab -> [SLab] -> (Int, SLab)
approachLevel gr tL run = minimumBy (compare `on` fst) $ map (compApproachLevel gr tL) $ findSourceBranch gr tL run


-- | Find all possible paths from this given node, avoiding loops,
--   cycles, etc.
pathTree             :: (DynGraph g) => Decomp g a b -> [NGroup]
pathTree (Nothing,_) = []
pathTree (Just ct,g)
    | isEmpty g = []
    | null sucs = [[n]]
    | otherwise = (:) [n] . map (n:) . concatMap (subPathTree g') $ sucs
    where
      n = node' ct
      sucs = suc' ct
      -- Avoid infinite loops by not letting it continue any further
      ct' = makeLeaf ct
      g' = ct' & g
      subPathTree gr n' = pathTree $ match n' gr


-- | Remove all outgoing edges
makeLeaf           :: Context a b -> Context a b
makeLeaf (p,n,a,_) = (p', n, a, [])
    where
      -- Ensure there isn't an edge (n,n)
      p' = filter (\(_,n') -> n' /= n) p


type NGroup = [Node]


testGr :: Gr NLab ELab
testGr = mkGraph grNodes grEdges  
grNodes = [(0,"entry"),(-1,"exit"),(1,"funcdecl"),(2,"2: if true"),(3,"3: block"),(4,"4: n++"),(5,"5: if true"),(6,"6: block"),(7,"7: n++"),(8,"8: block"),(9,"9: n++")]
grEdges = [(0,1,""),(1,2,""),(2,3,""),(2,5,""),(3,4,""),(4,-1,""),(5,6,""),(5,8,""),(6,7,""),(7,-1,""),(8,9,""),(9,-1,"")]


-- | Monadic variation on everywhere
everywhereM_ :: GenericM [] -> GenericM []
everywhereM_ f x = do x' <- gmapM (everywhereM_ f) x
                      f x'
