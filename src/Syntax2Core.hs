module Syntax2Core where

import Language.Haskell.Syntax

import Core

-- -----------------------------------------------------------------------------

translateName (HsIdent name) = mkName name
translateName (HsSymbol name) = mkName name

-- -----------------------------------------------------------------------------

translateQName (UnQual name) = translateName name

-- -----------------------------------------------------------------------------

translateLit (HsInt int) = LInt int

-- -----------------------------------------------------------------------------

translateQOp (HsQVarOp qname) = EVar (translateQName qname)
translateQOp (HsQConOp qname) = EVar (translateQName qname)

-- -----------------------------------------------------------------------------

translateExpr (HsVar qname) = EVar (translateQName qname)
translateExpr (HsLit lit) = ELit (translateLit lit)
translateExpr (HsApp e0 e1) = EApp (translateExpr e0) (translateExpr e1)
translateExpr (HsIf c t f) = ECase (translateExpr c) [aTrue (translateExpr t), aFalse (translateExpr f)]
translateExpr (HsList es) = EList (map translateExpr es)
translateExpr (HsEnumFromTo (HsLit (HsInt f)) (HsLit (HsInt t))) = EList (map (\x -> ELit (LInt x)) [f..t])
translateExpr (HsInfixApp e0 op e1) = EApp (translateQOp op) (EList [translateExpr e0, translateExpr e1])
translateExpr (HsCase e0 alts) = ECase (translateExpr e0) (map translateAlt alts)
translateExpr expr = error ("Unknown expression: " ++ (show expr))

-- -----------------------------------------------------------------------------

--translateAlt
translateAlt alt = error ("Alt translation not yet implemented: " ++ (show alt))

-- -----------------------------------------------------------------------------

translateRhs (HsUnGuardedRhs expr) = translateExpr expr

-- -----------------------------------------------------------------------------

translateDecl (HsPatBind _ (HsPVar n) rhs _) =
	Decl (EVar (translateName n)) (translateRhs rhs)
translateDecl (HsFunBind ms@((HsMatch _ n ps _ _):_)) =
	Decl (EVar (translateName n)) (ELam (map (\x -> EVar (mkName x)) $ mkPNames (length ps)) (EVar (mkName "HsFunBind")))

-- -----------------------------------------------------------------------------

translateModule (HsModule _ (Language.Haskell.Syntax.Module name) _ _ decls) =
	Core.Module (mkName name) (map translateDecl decls)
