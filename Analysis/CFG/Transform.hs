module Analysis.CFG.Transform where

import Language.ECMAScript3.Syntax (JavaScript(..), Statement(..), SourcePos)
import Data.Generics.Schemes (everywhere)
import Data.Generics.Aliases (mkT)

transfromJS :: JavaScript SourcePos -> JavaScript SourcePos
transfromJS = everywhere (mkT transformStmt)


transformStmt :: Statement SourcePos -> Statement SourcePos
transformStmt (IfSingleStmt l expr thenBr) = IfStmt l expr thenBr (BlockStmt l [])
transformStmt s = s
