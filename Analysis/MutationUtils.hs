module Analysis.MutationUtils where

import Language.ECMAScript3.Parser
import Language.ECMAScript3.Syntax
import Language.ECMAScript3.Analysis.LexicalEnvironment

import Control.Monad

-- import Data.Set (Set)
-- import qualified Data.Set as Set

collectVarDecl :: [Statement SourcePos] -> [SourcePos]
collectVarDecl = foldr (\x xs -> stmtVarDecl x ++ xs) []


stmtVarDecl :: Statement SourcePos -> [SourcePos]
stmtVarDecl s = case s of
  BlockStmt _ ss -> concatMap stmtVarDecl ss
  EmptyStmt _ -> []
  ExprStmt _ e -> exprVarDecl e
  IfStmt _ e s1 s2 -> exprVarDecl e ++ stmtVarDecl s1 ++ stmtVarDecl s2
  IfSingleStmt _ e s -> exprVarDecl e ++ stmtVarDecl s
  SwitchStmt _ e cases -> exprVarDecl e ++ concatMap caseClauseVarDecl cases
  WhileStmt _ e s -> exprVarDecl e ++ stmtVarDecl s
  DoWhileStmt _ s e -> stmtVarDecl s ++ exprVarDecl e
  BreakStmt _ _ -> []
  ContinueStmt _ _ -> []
  LabelledStmt _ _ s -> stmtVarDecl s
  ForInStmt _ _ e s -> exprVarDecl e ++ stmtVarDecl s
  ForStmt _ _ _ _ s -> stmtVarDecl s -- we don't consider counter declaration
  TryStmt _ s mcatch ms -> stmtVarDecl s ++ maybe [] catchClauseVarDecl mcatch ++ maybe [] stmtVarDecl ms
  ThrowStmt _ e -> exprVarDecl e
  ReturnStmt _ me -> maybe [] exprVarDecl me
  WithStmt _ e s -> exprVarDecl e ++ stmtVarDecl s
  VarDeclStmt _ decls -> concatMap varDecl decls
  FunctionStmt _ _ _ ss -> concatMap stmtVarDecl ss

-- | we only return top-level variable declarations
-- | variables can also be declared inside functional expressions
exprVarDecl :: Expression SourcePos -> [SourcePos]
exprVarDecl e = []

varDecl :: VarDecl SourcePos -> [SourcePos]
varDecl (VarDecl _ id Nothing)  = declVarDecl id
varDecl (VarDecl _ id (Just e)) = declVarDecl id ++ exprVarDecl e

forInitVarDecl :: ForInit SourcePos -> [SourcePos]
forInitVarDecl fi = case fi of
  NoInit     -> []
  VarInit ds -> concatMap varDecl ds
  ExprInit e -> exprVarDecl e 


catchClauseVarDecl :: CatchClause SourcePos -> [SourcePos]
catchClauseVarDecl (CatchClause _ _ s) = stmtVarDecl s


declVarDecl :: Id SourcePos -> [SourcePos]
declVarDecl (Id p v) = [p]

-- collectIntSet :: [Statement SourcePos] -> Set Int
-- collectIntSet = Set.unions . map stmtInt

-- collectStringSet :: [Statement SourcePos] -> Set String
-- collectStringSet = undefined

-- stmtInt :: Statement SourcePos -> Set Int
-- stmtInt s = case s of
--   BlockStmt _ ss -> Set.unions $ map stmtInt ss
--   EmptyStmt _ -> empty
--   ExprStmt _ e -> exprInt e
--   IfStmt _ e s1 s2 -> Set.unions [expr e, stmt s1, stmt s2]
--   IfSingleStmt _ e s -> Set.unions [expr e, stmt s]
--   SwitchStmt _ e cases -> Set.unions [expr e, unions $ map caseClause cases]
--   WhileStmt _ e s -> Set.unions [expr e, stmt s]
--   DoWhileStmt _ s e -> Set.unions [stmt s, expr e]
--   BreakStmt _ _ -> empty
--   ContinueStmt _ _ -> empty
--   LabelledStmt _ _ s -> stmt s
--   ForInStmt _ fii e s -> Set.unions [forInInit fii, expr e, stmt s]
--   ForStmt _ fi  me1 me2 s -> 
--     Set.unions [forInit fi, maybe empty expr me1, maybe empty expr me2, stmt s]
--   TryStmt _ s mcatch ms ->
--     Set.unions [stmt s, maybe empty catchClause mcatch, maybe empty stmt ms]
--   ThrowStmt _ e -> expr e
--   ReturnStmt _ me -> maybe empty expr me
--   WithStmt _ e s -> Set.unions [expr e, stmt s]
--   VarDeclStmt _ decls -> Set.unions $ map varDecl decls
--   FunctionStmt _ fnId args ss ->
--     Set.unions [decl fnId, nest $ Set.unions [Set.unions $ map decl args,
--                                       Set.unions $ map stmt ss]]


caseClauseVarDecl :: CaseClause SourcePos -> [SourcePos]
caseClauseVarDecl cc = case cc of
  CaseClause _ _ ss -> concatMap stmtVarDecl ss
  CaseDefault _ ss  -> concatMap stmtVarDecl ss
