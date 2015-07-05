{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, ScopedTypeVariables #-}

module Mutation.Dom.Operators where 

import Language.ECMAScript3.Parser
import Language.ECMAScript3.Analysis.LabelSet
import Language.ECMAScript3.Syntax
import Language.ECMAScript3.Syntax.Annotations
import qualified Language.ECMAScript3.PrettyPrint as JSP
import qualified Text.PrettyPrint.Leijen as PPL

import Data.Typeable
import Data.Data

import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations

import Data.Default.Class
import Text.Parsec.Pos
import Data.Either

import Control.Monad

import Analysis.CFG.Build

-- ---------------------------------------------------------------------
-- Experimentation code. Remove after

data Expr = Val Int
          | Var String
          | Neg Expr
          | Add Expr Expr  deriving (Show, Data, Typeable)

mutants :: Expr -> [Expr]
mutants x = [c (Val j) | (Val i, c) <- contexts x, j <- [i-1, i+1]]

variables :: Biplate b Expr => b -> [String]
variables x = [y | Var y <- universeBi x]

-- ---------------------------------------------------------------------

-- | ===================================================================
-- | Mirshokraie DOM mutation operators
-- | ===================================================================

type FunctionName = String

type SourcePosLab = (SourcePos, SLab)

-- | Change the order of arguments in insertBefore/replaceChild methods
-- | TODO: make sure that the function can handle more than two arguments
swapArguments :: forall b a. (Data b, Data a) => FunctionName -> b -> [(b, a)]
swapArguments fn x = 
    [ (c (CallExpr l1 (DotRef l2 ref (Id l3 fn)) [v2, v1]), l1) 
    | (CallExpr l1 (DotRef l2 ref (Id l3 fn')) [v1, v2], c :: (Expression a -> b)) <- contextsBi x
    , fn == fn'
    ]

insertSwapArguments :: Data a => JavaScript a -> [(JavaScript a, a)]
insertSwapArguments = swapArguments "insertBefore"

replaceSwapArguments :: Data a => JavaScript a -> [(JavaScript a, a)]
replaceSwapArguments = swapArguments "replaceChild"


-- | Change the name of the id/tag used in getElementById and getElementByTagName methods
changeArgument :: forall b a. (Data b, Data a, Default a) => FunctionName -> b -> [(b, a)]
changeArgument fn x = 
    [ (c (CallExpr l1 (DotRef l2 ref (Id l3 fn)) [v1']), l1) 
    | (CallExpr l1 (DotRef l2 ref (Id l3 fn')) [v1], c :: (Expression a -> b)) <- contextsBi x
    , fn' == fn
    , let v1' = InfixExpr def OpAdd (InfixExpr def OpAdd (StringLit def "_") v1) (StringLit def "_")
    ]

idChangeArgument :: (Data a, Default a) => JavaScript a -> [(JavaScript a, a)]
idChangeArgument = changeArgument "getElementById"


tagChangeArgument :: (Data a, Default a) => JavaScript a -> [(JavaScript a, a)]
tagChangeArgument = changeArgument "getElementByTagName"

-- | TODO
changeSetAttribute' :: forall b a. (Data b, Data a) => b -> [(b, a)]
changeSetAttribute' = undefined

changeSetAttribute :: Data a => JavaScript a -> [(JavaScript a, a)]
changeSetAttribute = changeSetAttribute'

-- | TODO
changeGetAttribute' :: forall b a. (Data b, Data a) => b -> [(b, a)]
changeGetAttribute' = undefined

changeGetAttribute :: Data a => JavaScript a -> [(JavaScript a, a)]
changeGetAttribute = changeGetAttribute


-- | TODO
changeRemoveAttribute' :: forall b a. (Data b, Data a) => b -> [(b, a)]
changeRemoveAttribute' = undefined

changeRemoveAttribute :: Data a => JavaScript a -> [(JavaScript a, a)]
changeRemoveAttribute = changeRemoveAttribute

-- | Swap innerHTML and innerText properties
swapInnerHtmlAndText' :: forall b a. (Data b, Data a) => b -> [(b, a)] 
swapInnerHtmlAndText' x = 
    [ (c (DotRef l1 ref (Id l2 prop')), l1) 
    | (DotRef l1 ref (Id l2 prop), c :: (Expression a -> b)) <- contextsBi x
    , prop == "innerHTML" || prop == "innerText"
    , let prop' = if (prop == "innerHTML") then "innerText" else "innerHTML"
    ]

swapInnerHtmlAndText :: Data a => JavaScript a -> [(JavaScript a, a)]
swapInnerHtmlAndText = swapInnerHtmlAndText'

-- testMutation :: Data a => JavaScript a -> [(JavaScript a, a)]
testMutation =  map (show . JSP.prettyPrint . fst) $ swapInnerHtmlAndText $ either (const (Script (initialPos "") [])) id $ parseFromString "p.innerHTML" 


domMutations :: (Data a, Default a) => [JavaScript a -> [(JavaScript a, a)]]
domMutations = [ insertSwapArguments
               , replaceSwapArguments
               , idChangeArgument
               , tagChangeArgument
               , swapInnerHtmlAndText
               ]

               -- , changeSetAttribute
               -- , changeGetAttribute
               -- , changeRemoveAttribute

-- | ===================================================================


renameMutatedFunction :: JavaScript a -> JavaScript a
renameMutatedFunction = undefined
