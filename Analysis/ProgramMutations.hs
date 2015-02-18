{-# LANGUAGE StandaloneDeriving #-}

module Analysis.ProgramMutations where

import Language.ECMAScript3.Parser
import Language.ECMAScript3.Syntax
import Language.ECMAScript3.Analysis.LexicalEnvironment

import qualified Data.Foldable as F

import Control.Monad
import Data.Maybe
import qualified Data.Map as M
import Data.Map (Map)

test :: String -> IO (JavaScript SourcePos)
test = parseFromFile

-- ==================================================
-- | MUT_VAR_CH this mutation only changes the values of primitive variables

data JSMutation = MUT_VAR_CH    -- change the value assigned to the variable
                | MUT_VAR_RM    -- remove variable declaration/initialization
                | MUT_VAR_CH_TP -- change the variable type by converting number to string
                | MUT_VAR_RP_AOP
                | MUT_FUN_SW_ARG
                | MUT_FUN_RM_ARG


apply_VAR_RM :: [Statement SourcePos] -> SourcePos -> [Statement SourcePos]
apply_VAR_RM stms pos = foldr (\st sts -> ) [] stms


genJSMutationsN :: JavaScript SourcePos -> Int -> [JavaScript SourcePos]
genJSMutationsN (Script pos sts) = undefined


deriving instance Show EnvTree

collectVars :: [Statement SourcePos] -> [(String, SourcePos)]
collectVars = concatMap stmt 

stmt :: Statement SourcePos -> [(String, SourcePos)]
stmt s = case s of
  BlockStmt _ ss -> concatMap stmt ss
  EmptyStmt _ -> []
  ExprStmt _ e -> expr e
  IfStmt _ _ s1 s2 -> stmt s1 ++ stmt s2
  IfSingleStmt _ _ s -> stmt s
  SwitchStmt _ _ cases -> concatMap caseClause cases
  WhileStmt _ _ s -> stmt s
  DoWhileStmt _ s _ -> stmt s
  BreakStmt _ _ -> []
  ContinueStmt _ _ -> []
  LabelledStmt _ _ s -> stmt s
  ForInStmt _ _ _ s -> stmt s
  ForStmt _ _  _ _ s -> stmt s
  TryStmt _ s mcatch ms -> stmt s ++ maybe [] catchClause mcatch ++ maybe [] stmt ms
  ThrowStmt _ e -> expr e
  ReturnStmt _ me -> maybe [] expr me
  WithStmt _ _ s -> stmt s
  VarDeclStmt _ decls -> map varDecl decls
  FunctionStmt _  _ _ ss -> concatMap stmt ss


caseClause :: CaseClause SourcePos -> [(String, SourcePos)]
caseClause cc = case cc of
  CaseClause _ _ ss -> concatMap stmt ss
  CaseDefault _ ss  -> concatMap stmt ss

catchClause :: CatchClause SourcePos -> [(String, SourcePos)]
catchClause (CatchClause _ _ s) = stmt s

varDecl :: VarDecl SourcePos -> (String, SourcePos)
varDecl (VarDecl _ id Nothing)  = decl id
varDecl (VarDecl _ id (Just e)) = decl id


expr :: Expression SourcePos -> [(String, SourcePos)]
expr e = case e of
  StringLit _ _ -> []
  RegexpLit {}   -> []
  NumLit _ _    -> []
  IntLit _ _    -> []
  BoolLit _ _   -> []
  NullLit _     -> []
  ArrayLit _ _  -> []
  ObjectLit _ _ -> []
  ThisRef _     -> []
  VarRef _ _    -> []
  DotRef _ _ _  -> []
  BracketRef _ _ _ -> []
  NewExpr _ _ _ -> []
  PrefixExpr _ _ _ -> []
  InfixExpr _ _ _ _ -> []
  CondExpr _ _ _ _ -> []
  AssignExpr p _ lv _ -> [(show lv, p)]
  UnaryAssignExpr _ _ _ -> [] -- handle the case of unary assignment such x--, etc.
  ListExpr _ _ -> []
  CallExpr _ _ _   -> []
  FuncExpr _ _ _ _ -> []


decl :: Id SourcePos -> (String, SourcePos)
decl (Id p v) = (v, p)
