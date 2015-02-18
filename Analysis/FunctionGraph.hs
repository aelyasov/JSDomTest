module Analysis.FunctionGraph where

import Control.Monad
import Data.List
import Language.JavaScript.Parser (parseFile)
import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser.SrcLocation
import System.FilePath.Posix
import System.IO.HVFS (SystemFS(..))
import System.IO.HVFS.Utils (recurseDir)

-- import Language.JavaScript.Parser.ParseError
import Language.JavaScript.Parser.Grammar5
import Language.JavaScript.Parser.Lexer

import Debug.Trace

-- | The function findSourcesJS recursively traverses the folder given as its first argument and returns
-- | the list of  
findSourcesJS :: FilePath -> IO [FilePath]
findSourcesJS dir = do 
  allFiles <- recurseDir SystemFS dir
  return $ filter (\f -> takeExtension f == ".js") allFiles

type Loc = (Int, Int)
type LocBegin = Loc
type LocEnd   = Loc
type FunName = String
type FileName = String
data FunctionScope = FUNScope FunName FileName LocBegin LocEnd
                     deriving Show

isNTJSLiteral :: JSNode -> Bool
isNTJSLiteral (NT (JSLiteral "=") _ _) = True
isNTJSLiteral _                        = False

isNNFunExpr :: JSNode -> Bool
isNNFunExpr (NN (JSFunctionExpression _ _ _ _ _ _)) = True
isNNFunExpr _                           = False  

getFunName :: JSNode -> FunName
getFunName (NT (JSIdentifier fname) _  _) = fname
getFunName jnd = error $ "given node: " ++ show jnd ++ " is not a function identifier"

getFunLocations :: JSNode -> (LocBegin, LocEnd)
getFunLocations (NN (JSBlock [NT _ (TokenPn _ bl bc) _] _ [NT _ (TokenPn _ el ec) _])) = ((bl,bc), (el, ec))
getFunLocations jnd = error $ "given node: " ++ show jnd ++ " is not a block"

collectConstantInfoJS :: FilePath -> IO [FunctionScope]
collectConstantInfoJS jsF = do 
  NN (JSSourceElementsTop nodes) <- parseFile jsF
  let jsNode2funScope :: JSNode -> [FunctionScope]
      jsNode2funScope (NN nd)     = node2funScope nd 
      jsNode2funScope (NT nd _ _) = node2funScope nd

      n2fscMap = concatMap jsNode2funScope
                                
      node2funScope :: Node -> [FunctionScope]
      node2funScope (JSIdentifier _)      = []
      node2funScope (JSDecimal _)         = []
      node2funScope (JSLiteral _)         = []
      node2funScope (JSHexInteger _)      = []
      node2funScope (JSOctal _)           = []
      node2funScope (JSStringLiteral _ _) = []
      node2funScope (JSRegEx _)           = []
      node2funScope (JSArguments ljnd jnds rjnd)    = n2fscMap $ [ljnd] ++ jnds ++ [rjnd]
      node2funScope (JSArrayLiteral ljnd jnds rjnd) = n2fscMap $ [ljnd] ++ jnds ++ [rjnd]
      node2funScope (JSBlock ljnds jnds rjnds)      = n2fscMap $ ljnds ++ jnds ++ rjnds
      node2funScope (JSBreak jnd1 jnds jnd2)        = n2fscMap $ [jnd1] ++ jnds ++ [jnd2]
      node2funScope (JSCallExpression _ jnds1 jnds2 jnds3) = n2fscMap $ jnds1 ++ jnds2 ++ jnds3
      node2funScope (JSCase jnd1 jnd2 jnd3 jnds)    = n2fscMap $ [jnd1] ++ [jnd2] ++ [jnd3] ++ jnds
      node2funScope (JSCatch jnd1 jnd2 jnd3 jnds jnd4 jnd5) = n2fscMap $ [jnd1] ++ [jnd2] ++ [jnd3] ++ jnds ++ [jnd4] ++ [jnd5]
      node2funScope (JSContinue jnd1 jnds jnd2) = n2fscMap $ [jnd1] ++ jnds ++ [jnd2]
      node2funScope (JSDefault jnd1 jnd2 jnds)  = n2fscMap $ [jnd1] ++ [jnd2] ++ jnds
      node2funScope (JSDoWhile jnd1 jnd2 jnd3 jnd4 jnd5 jnd6 jnd7) = n2fscMap $ [jnd1] ++ [jnd2] ++ [jnd3] ++ [jnd4] ++ [jnd5] ++ [jnd6] ++ [jnd7]
      node2funScope (JSElision jnd) = n2fscMap [jnd]
      node2funScope (JSExpression jnds) = n2fscMap jnds
      node2funScope (JSExpressionBinary _ jnds1 jnd jnds2) = n2fscMap $ jnds1 ++ [jnd] ++ jnds2
      node2funScope (JSExpressionParen jnd1 jnd2 jnd3) = n2fscMap $ [jnd1] ++ [jnd2] ++ [jnd3]
      node2funScope (JSExpressionPostfix _ jnds jnd) = n2fscMap $ jnds ++ [jnd]
      node2funScope (JSExpressionTernary jnds1 jnd1 jnds2 jnd2 jnds3) = n2fscMap $ jnds1 ++ [jnd1] ++ jnds2 ++ [jnd2] ++ jnds3
      node2funScope (JSFinally jnd1 jnd2) = n2fscMap $ [jnd1] ++ [jnd2]
      node2funScope (JSFor jnd1 jnd2 jnds1 jnd3 jnds2 jnd4 jnds3 jnd5 jnd6) = n2fscMap $ [jnd1] ++ [jnd2] ++ jnds1 ++ [jnd3] ++ jnds2 ++ [jnd4] ++ jnds3 ++ [jnd5] ++ [jnd6]
      node2funScope (JSForIn jnd1 jnd2 jnds jnd3 jnd4 jnd5 jnd6) = n2fscMap $ [jnd1] ++ [jnd2] ++ jnds ++ [jnd3] ++ [jnd5] ++ [jnd6]
      node2funScope (JSForVar jnd1 jnd2 jnd3 jnds1 jnd4 jnds2 jnd5 jnds3 jnd6 jnd7) = n2fscMap $ [jnd1] ++ [jnd2] ++ [jnd3] ++ jnds1 ++ [jnd4] ++ jnds2 ++ [jnd5] ++ jnds3 ++ [jnd6] ++ [jnd7]
      node2funScope (JSForVarIn jnd1 jnd2 jnd3 jnd4 jnd5 jnd6 jnd7 jnd8) = n2fscMap $ [jnd1] ++ [jnd2] ++ [jnd3] ++ [jnd4] ++ [jnd5] ++ [jnd6] ++ [jnd7] ++ [jnd8]
      node2funScope (JSFunction jnd1 jnd2 jnd3 jnds jnd4 jnd5) = 
          let fname = getFunName jnd2
              ((bl,bc), (el, ec)) = getFunLocations jnd5
              recCall = n2fscMap $ [jnd1] ++ [jnd2] ++ [jnd3] ++ jnds ++ [jnd4] ++ [jnd5]
          in  (FUNScope fname jsF (bl,bc) (el,ec)):recCall 
      node2funScope (JSFunctionExpression jnd1 jnds1 jnd2 jnds2 jnd3 jnd4) = n2fscMap $ [jnd1] ++ jnds1 ++ [jnd2] ++ jnds2 ++ [jnd3] ++ [jnd4]
      node2funScope (JSIf jnd1 jnd2 jnd3 jnd4 jnds1 jnds2) = n2fscMap $ [jnd1] ++ [jnd2] ++ [jnd3] ++ [jnd4] ++ jnds1 ++ jnds2
      node2funScope (JSLabelled jnd1 jnd2 jnd3) = n2fscMap $ [jnd1] ++ [jnd2] ++ [jnd3]
      node2funScope (JSMemberDot jnds jnd1 jnd2) = n2fscMap $ jnds ++ [jnd1] ++ [jnd2]  
      node2funScope (JSMemberSquare jnds jnd1 jnd2 jnd3) = n2fscMap $ jnds ++ [jnd1] ++ [jnd2] ++ [jnd3]
      node2funScope (JSObjectLiteral jnd1 jnds jnd2) = n2fscMap $ [jnd1] ++ jnds ++ [jnd2]
      node2funScope (JSOperator jnd) = n2fscMap $ [jnd]
      node2funScope (JSPropertyAccessor jnd1 jnd2 jnd3 jnds jnd4 jnd5) = n2fscMap $ [jnd1] ++ [jnd2] ++ [jnd3] ++ jnds ++ [jnd4] ++ [jnd5]
      node2funScope (JSPropertyNameandValue jnd1 jnd2 jnds) = 
          let isFunDef :: [JSNode] -> Bool
              isFunDef [n1] | isNNFunExpr n1 = True 
                            | otherwise      = False
              isFunDef _ = False
              fname = getFunName jnd1
              (NN (JSFunctionExpression _ _ _ _ _ blNode)) = head jnds
              ((bl,bc), (el, ec)) = getFunLocations blNode
              recCall = n2fscMap $ [jnd1] ++ [jnd2] ++ jnds
          in  if isFunDef jnds 
              then (FUNScope fname jsF (bl,bc) (el,ec)):recCall 
              else recCall 
      node2funScope (JSReturn jnd1 jnds jnd2) = n2fscMap $ [jnd1] ++ jnds ++ [jnd2]
      node2funScope (JSSourceElementsTop jnds) = n2fscMap $ jnds 
      node2funScope (JSSwitch jnd1 jnd2 jnd3 jnd4 jnd5) = n2fscMap $ [jnd1] ++ [jnd2] ++ [jnd3] ++ [jnd4] ++ [jnd5] 
      node2funScope (JSThrow jnd1 jnd2) = n2fscMap $ [jnd1] ++ [jnd2]
      node2funScope (JSTry jnd1 jnd2 jnds) = n2fscMap $ [jnd1] ++ [jnd2] ++ jnds
      node2funScope (JSUnary _ jnd) = n2fscMap $ [jnd]
      node2funScope (JSVarDecl jnd jnds) = 
          let isFunDef :: [JSNode] -> Bool
              isFunDef [n1,n2] | isNTJSLiteral n1 && isNNFunExpr n2 = True 
                               | otherwise                          = False
              isFunDef _ = False
              fname = getFunName jnd
              (NN (JSFunctionExpression _ _ _ _ _ blNode)) = head $ tail jnds
              ((bl,bc), (el, ec)) = getFunLocations blNode
              recCall = n2fscMap $ [jnd] ++ jnds
          in  if isFunDef jnds 
              then (FUNScope fname jsF (bl,bc) (el,ec)):recCall 
              else recCall 
      node2funScope (JSVariables jnd1 jnds jnd2) = n2fscMap $ [jnd1] ++ jnds ++ [jnd2]
      node2funScope (JSWhile jnd1 jnd2 jnd3 jnd4 jnd5) = n2fscMap $ [jnd1] ++ [jnd2] ++ [jnd3] ++ [jnd4] ++ [jnd5]
      node2funScope (JSWith jnd1 jnd2 jnd3 jnd4 jnds) = n2fscMap $ [jnd1] ++ [jnd2] ++ [jnd3] ++ [jnd4] ++ jnds
  return $ n2fscMap nodes


type StringPool = [String]
type IntPool = [String]
type ConstantPool = (IntPool, StringPool)
collectConstantInfoFileJS :: FilePath -> IO ConstantPool
-- collectConstantInfoFileJS = undefined
collectConstantInfoFileJS jsF = do
  NN (JSSourceElementsTop nodes) <- parseFile jsF
  let jsNode2funScope :: JSNode -> ConstantPool
      jsNode2funScope (NN nd)     = node2funScope nd 
      jsNode2funScope (NT nd _ _) = node2funScope nd

      n2fscMap = foldr (\n (ints, strs) 
                            -> let (n_ints, n_strs) = jsNode2funScope n
                               in  (n_ints ++ ints, n_strs ++ strs)) ([],[])
                 
      node2funScope :: Node -> ConstantPool
      node2funScope (JSIdentifier _)        = ([], [])
      node2funScope (JSDecimal jnd)         = ([jnd], [])
      node2funScope (JSLiteral _)           = ([], []) 
      node2funScope (JSHexInteger _)        = ([], [])
      node2funScope (JSOctal _)             = ([], [])
      node2funScope (JSStringLiteral _ jnd) = ([], [jnd])
      node2funScope (JSRegEx _)             = ([], [])
      node2funScope (JSArguments ljnd jnds rjnd)    = n2fscMap $ [ljnd] ++ jnds ++ [rjnd]
      node2funScope (JSArrayLiteral ljnd jnds rjnd) = n2fscMap $ [ljnd] ++ jnds ++ [rjnd]
      node2funScope (JSBlock ljnds jnds rjnds)      = n2fscMap $ ljnds ++ jnds ++ rjnds
      node2funScope (JSBreak jnd1 jnds jnd2)        = n2fscMap $ [jnd1] ++ jnds ++ [jnd2]
      node2funScope (JSCallExpression _ jnds1 jnds2 jnds3) = n2fscMap $ jnds1 ++ jnds2 ++ jnds3
      node2funScope (JSCase jnd1 jnd2 jnd3 jnds)    = n2fscMap $ [jnd1] ++ [jnd2] ++ [jnd3] ++ jnds
      node2funScope (JSCatch jnd1 jnd2 jnd3 jnds jnd4 jnd5) = n2fscMap $ [jnd1] ++ [jnd2] ++ [jnd3] ++ jnds ++ [jnd4] ++ [jnd5]
      node2funScope (JSContinue jnd1 jnds jnd2) = n2fscMap $ [jnd1] ++ jnds ++ [jnd2]
      node2funScope (JSDefault jnd1 jnd2 jnds)  = n2fscMap $ [jnd1] ++ [jnd2] ++ jnds
      node2funScope (JSDoWhile jnd1 jnd2 jnd3 jnd4 jnd5 jnd6 jnd7) = n2fscMap $ [jnd1] ++ [jnd2] ++ [jnd3] ++ [jnd4] ++ [jnd5] ++ [jnd6] ++ [jnd7]
      node2funScope (JSElision jnd) = n2fscMap [jnd]
      node2funScope (JSExpression jnds) = n2fscMap jnds
      node2funScope (JSExpressionBinary _ jnds1 jnd jnds2) = n2fscMap $ jnds1 ++ [jnd] ++ jnds2
      node2funScope (JSExpressionParen jnd1 jnd2 jnd3) = n2fscMap $ [jnd1] ++ [jnd2] ++ [jnd3]
      node2funScope (JSExpressionPostfix _ jnds jnd) = n2fscMap $ jnds ++ [jnd]
      node2funScope (JSExpressionTernary jnds1 jnd1 jnds2 jnd2 jnds3) = n2fscMap $ jnds1 ++ [jnd1] ++ jnds2 ++ [jnd2] ++ jnds3
      node2funScope (JSFinally jnd1 jnd2) = n2fscMap $ [jnd1] ++ [jnd2]
      node2funScope (JSFor jnd1 jnd2 jnds1 jnd3 jnds2 jnd4 jnds3 jnd5 jnd6) = n2fscMap $ [jnd1] ++ [jnd2] ++ jnds1 ++ [jnd3] ++ jnds2 ++ [jnd4] ++ jnds3 ++ [jnd5] ++ [jnd6]
      node2funScope (JSForIn jnd1 jnd2 jnds jnd3 jnd4 jnd5 jnd6) = n2fscMap $ [jnd1] ++ [jnd2] ++ jnds ++ [jnd3] ++ [jnd5] ++ [jnd6]
      node2funScope (JSForVar jnd1 jnd2 jnd3 jnds1 jnd4 jnds2 jnd5 jnds3 jnd6 jnd7) = n2fscMap $ [jnd1] ++ [jnd2] ++ [jnd3] ++ jnds1 ++ [jnd4] ++ jnds2 ++ [jnd5] ++ jnds3 ++ [jnd6] ++ [jnd7]
      node2funScope (JSForVarIn jnd1 jnd2 jnd3 jnd4 jnd5 jnd6 jnd7 jnd8) = n2fscMap $ [jnd1] ++ [jnd2] ++ [jnd3] ++ [jnd4] ++ [jnd5] ++ [jnd6] ++ [jnd7] ++ [jnd8]
      node2funScope (JSFunction jnd1 jnd2 jnd3 jnds jnd4 jnd5) = n2fscMap $ [jnd1] ++ [jnd2] ++ [jnd3] ++ jnds ++ [jnd4] ++ [jnd5]
      node2funScope (JSFunctionExpression jnd1 jnds1 jnd2 jnds2 jnd3 jnd4) = n2fscMap $ [jnd1] ++ jnds1 ++ [jnd2] ++ jnds2 ++ [jnd3] ++ [jnd4]
      node2funScope (JSIf jnd1 jnd2 jnd3 jnd4 jnds1 jnds2) = n2fscMap $ [jnd1] ++ [jnd2] ++ [jnd3] ++ [jnd4] ++ jnds1 ++ jnds2
      node2funScope (JSLabelled jnd1 jnd2 jnd3) = n2fscMap $ [jnd1] ++ [jnd2] ++ [jnd3]
      node2funScope (JSMemberDot jnds jnd1 jnd2) = n2fscMap $ jnds ++ [jnd1] ++ [jnd2]  
      node2funScope (JSMemberSquare jnds jnd1 jnd2 jnd3) = n2fscMap $ jnds ++ [jnd1] ++ [jnd2] ++ [jnd3]
      node2funScope (JSObjectLiteral jnd1 jnds jnd2) = n2fscMap $ [jnd1] ++ jnds ++ [jnd2]
      node2funScope (JSOperator jnd) = n2fscMap $ [jnd]
      node2funScope (JSPropertyAccessor jnd1 jnd2 jnd3 jnds jnd4 jnd5) = n2fscMap $ [jnd1] ++ [jnd2] ++ [jnd3] ++ jnds ++ [jnd4] ++ [jnd5]
      node2funScope (JSPropertyNameandValue jnd1 jnd2 jnds) = n2fscMap $ [jnd1] ++ [jnd2] ++ jnds
      node2funScope (JSReturn jnd1 jnds jnd2) = n2fscMap $ [jnd1] ++ jnds ++ [jnd2]
      node2funScope (JSSourceElementsTop jnds) = n2fscMap $ jnds 
      node2funScope (JSSwitch jnd1 jnd2 jnd3 jnd4 jnd5) = n2fscMap $ [jnd1] ++ [jnd2] ++ [jnd3] ++ [jnd4] ++ [jnd5] 
      node2funScope (JSThrow jnd1 jnd2) = n2fscMap $ [jnd1] ++ [jnd2]
      node2funScope (JSTry jnd1 jnd2 jnds) = n2fscMap $ [jnd1] ++ [jnd2] ++ jnds
      node2funScope (JSUnary _ jnd) = n2fscMap $ [jnd]
      node2funScope (JSVarDecl jnd jnds) = n2fscMap $ [jnd] ++ jnds
      node2funScope (JSVariables jnd1 jnds jnd2) = n2fscMap $ [jnd1] ++ jnds ++ [jnd2]
      node2funScope (JSWhile jnd1 jnd2 jnd3 jnd4 jnd5) = n2fscMap $ [jnd1] ++ [jnd2] ++ [jnd3] ++ [jnd4] ++ [jnd5]
      node2funScope (JSWith jnd1 jnd2 jnd3 jnd4 jnds) = n2fscMap $ [jnd1] ++ [jnd2] ++ [jnd3] ++ [jnd4] ++ jnds

      (intPool, strPool) = n2fscMap nodes
  mapM print $ nub strPool
  return (nub intPool, nub strPool)

-- | createElement(String name)
-- | createElementNS(String namespace, String name)
-- | createTextNode(String text)
-- | getElementsByClassName(String className)
-- | getElementsByTagName(String tagName)
-- | getElementsByTagNameNS(String namespace, String tagName)
-- | getElementById(String id)
-- | querySelector(String selector)
-- | querySelectorAll(String selector)
-- | getElementsByName(String name)

-- | Parse one compound statement, or a sequence of simple statements.
-- Generally used for interactive input, such as from the command line of an interpreter.

test :: IO ()
test = do
  s <- getContents
  case (gettokens s) of
    Right tokens
        -> putStr( (show tokens ))
    Left string
        -> putStr("Failure detected: " ++ string ++ "\n")


gettokens  str =
    runAlex str $ do 
      let loop toks = do tok <- alexMonadScan -- lexToken;
                         case tok of
                           EOFToken _ _ ->  return $ reverse toks
                           t            ->  do loop  $! (t:toks)
      loop []


{-
-- Return comments in addition to the parsed statements.
parseT :: String -- ^ The input stream (Javascript source code).
       -> String -- ^ The name of the Javascript source (filename or input device).
       -> Either String Token
-- ^ An error or maybe the abstract syntax tree (AST) of zero
-- or more Javascript statements, plus comments.
parseT input _srcName = runAlex input parseStatement
-- parseProgram


readJsT :: String -> Token
readJsT input = do
  case (parseT input "src") of
    Left msg -> error (show msg)
    Right p -> p

-- | Parse the given file.
-- For UTF-8 support, make sure your locale is set such that
-- "System.IO.localeEncoding" returns "utf8"
parseFileT :: FilePath -> IO Token
parseFileT filename =
    do x <- readFile (filename)
       return $ readJsT x
-}
