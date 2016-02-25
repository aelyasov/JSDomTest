module Analysis.CFG.Build where

import Data.Graph.Inductive.Graph (LNode, LEdge, mkGraph, match, outdeg, nodeRange, out)
import Data.Graph.Inductive.PatriciaTree (Gr(..))
import Data.Graph.Inductive.Dot (fglToDotString, showDot)
import Language.ECMAScript3.Syntax (Statement(..), CaseClause(..), SourcePos, unJavaScript, unId)
import Language.ECMAScript3.Parser (parseFromFile)
import Analysis.CFG.Data (SLab, NLab, ELab)
import Analysis.CFG.Util (getStmtLab, getNextStLab, getCCLab, prettyNodeCFG, prettyPrintMaybe, findIdLab)
import qualified Language.ECMAScript3.PrettyPrint as JSP
import qualified Text.PrettyPrint.Leijen as PPL
import Safe (headMay)
import Data.Maybe (maybe)
import Control.Monad (liftM)
import Control.Monad.State (runState)
import Analysis.CFG.Label (assignUniqueIdsSt)
import System.Process (system)
import System.FilePath.Posix (takeDirectory, (</>))
import Debug.Trace (trace)



getAllBranches :: Gr NLab ELab -> [LEdge ELab]
getAllBranches gr = foldr (\nd edges -> if (outdeg gr nd > 1) then (out gr nd) ++ edges else edges ) [] [minNode .. maxNode]
  where
    (minNode, maxNode) = nodeRange gr

ppCFG :: FilePath -> IO ()
ppCFG fp = do
  stms <- liftM (unJavaScript . fst . flip runState 0 . assignUniqueIdsSt) $ parseFromFile fp
  print stms
  let coll@(nodes', edges') = collectEdges stms (0,-1,[]) (-1)
      nodes   = (0, "0: entry"):(-1, "-1: exit"):nodes'
      edges   = (0, 1, ""):edges'
      graph   = mkGraph nodes edges :: Gr NLab ELab
      dot     = showDot (fglToDotString graph)
      dir     = takeDirectory fp
      dotFile = dir </> "output.dot"
      pngFile = dir </> "output.png"
  -- showDot (fglToDot graph)
  print dir
  -- prettyPrint graph
  writeFile dotFile dot
  system $ "dot -Tpng -o" ++ pngFile ++ " " ++ dotFile
  return ()


mkTestCFG :: FilePath -> IO (Gr NLab ELab)
mkTestCFG fp = do
  stms <- liftM (unJavaScript . fst . flip runState 0 . assignUniqueIdsSt) $ parseFromFile fp
  -- print stms
  let coll@(nodes', edges') = collectEdges stms (0,-1,[]) (-1)
      nodes = (0, "0: entry"):(-1, "-1: exit"):nodes'
      edges = (0, 1, ""):edges'
      graph = mkGraph nodes edges :: Gr NLab ELab
  return graph

enrichCollectedEdges :: [Statement (SourcePos, SLab)] -> ([LNode NLab], [LEdge ELab])
enrichCollectedEdges stms = 
    let (nodes', edges') = collectEdges stms (0,-1,[]) (-1)
    in  ((0, "0: entry"):(-1, "-1: exit"):nodes', (0, 1, ""):edges')


-- | the function collectEdges takes the list of statements, pair of labels (block entry and exit), end label and returns edges together with the nodes of the CFG graph
collectEdges :: [Statement (SourcePos, SLab)] -> (Int, Int, [(SLab, String)]) -> Int ->  ([LNode NLab], [LEdge ELab])
collectEdges [] _ _ = ([], []) -- FIX: the label is not necessary "x"
collectEdges (st:sts) labs@(loopInNd, loopOutNd, stLabs) endNd = 
    let curNd  = getStmtLab st
        sucNd  = getNextStLab endNd sts
        edgeIn = (curNd, sucNd, "")
        (nodes, edges) = collectEdges sts labs endNd
    in  case st of
          BlockStmt _ sts1 -> let blockNd = (curNd, prettyNodeCFG curNd "block" Nothing)
                                  (blockNds, blockEds) = collectEdges sts1 labs sucNd
                                  blockInNd = getNextStLab sucNd sts1
                              in  (blockNd:(blockNds ++ nodes), (curNd, blockInNd, ""):(blockEds ++ edges))
          EmptyStmt _      -> let emptyNd = (curNd, prettyNodeCFG curNd "empty" Nothing)
                              in (emptyNd:nodes, edgeIn:edges)
          ExprStmt _ expr     -> let exprNd = (curNd, prettyNodeCFG curNd "" (Just expr))
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
          ReturnStmt _ expr       -> let returnNd = (curNd, prettyNodeCFG curNd "return " expr) -- TODO: edge from return to the consecutive node 
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
