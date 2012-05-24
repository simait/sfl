module Syntax2Core where

import Language.Haskell.Syntax

import Core

-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------

translateName (HsIdent n) = mkName n
translateName (HsSymbol n) = mkName n

-- -----------------------------------------------------------------------------

translateQName (UnQual n) = translateName n

-- -----------------------------------------------------------------------------

translateLit (HsInt i) = LInt i
translateLit (HsChar c) = LChar c
translateLit (HsString s) = LString s

-- -----------------------------------------------------------------------------

translateQOp (HsQVarOp qn) = EVar (translateQName qn)
translateQOp (HsQConOp qn) = EVar (translateQName qn)

-- -----------------------------------------------------------------------------

translateExpr (HsVar qn) = EVar (translateQName qn)
translateExpr (HsLit lit) = ELit (translateLit lit)
translateExpr (HsCon qn) = ECon (translateQName qn) []
translateExpr (HsApp e0 e1) = EApp (translateExpr e0) (translateExpr e1)
translateExpr (HsIf c t f) = ECase (translateExpr c) [aTrue (translateExpr t), aFalse (translateExpr f)]
translateExpr (HsList es) = EList (map translateExpr es)
translateExpr (HsEnumFromTo (HsLit (HsInt f)) (HsLit (HsInt t))) = EList (map (\x -> ELit (LInt x)) [f..t])
translateExpr (HsInfixApp e0 op e1) = EApp (translateQOp op) (EList [translateExpr e0, translateExpr e1])
translateExpr (HsCase e0 alts) = ELet [Decl (EVar x) (translateExpr e0)] (ECase (EVar x) (map translateAlt alts))
	where x:xs = map mkName $ mkPNames 1
translateExpr expr = error ("Unknown expression: " ++ (show expr))

-- -----------------------------------------------------------------------------

translatePat (HsPVar n) = PVar (translateName n)
translatePat (HsPLit l) = PLit (translateLit l)
translatePat (HsPApp qn ps) = PCon (translateQName qn) (map translatePat ps)
translatePat pat = error ("Not yet implemented. " ++ (show pat))

-- -----------------------------------------------------------------------------

--translateAlt alt = error ("Alt translation not yet implemented: " ++ (show alt))
translateAlt (HsAlt _ pat (HsUnGuardedAlt expr) _) = Alt (translatePat pat) (translateExpr expr)

-- -----------------------------------------------------------------------------

translateRhs (HsUnGuardedRhs expr) = translateExpr expr

-- -----------------------------------------------------------------------------

isVarHsP (HsPVar _) = True
isVarHsP _ = False

--isLitP (HsPLit _) = True
--isLitP _ = False

--isConHsP (HsPLit _) = True
isConHsP (HsPList _) = True
--isConHsP (HsPApp _ _) = True
isConHsP _ = False

--isTupP (HsPTuple _) = True
--isTupP _ = False

--isParenP (HsPParen _) = True
--isParenP _ = False

--isInfinixP (HsPInfinixApp _ _ _) = True
--isInfinixP _ = False

-- substHsExp x v e = [x/v]e - substitute v for x in e
substHsExp x v e@(HsVar (UnQual (HsIdent i))) = if i == v then HsVar (UnQual (HsIdent x)) else e
substHsExp x v e@(HsInfixApp e0 op e1)        = HsInfixApp (substHsExp x v e0) op (substHsExp x v e1)
substHsExp x v e@(HsApp e0 e1)                = HsApp (substHsExp x v e0) (substHsExp x v e1)
substHsExp x v e@(HsIf e0 e1 e2)              = HsIf (substHsExp x v e0) (substHsExp x v e1) (substHsExp x v e2)
substHsExp _ _ e                              = error ("substHsExp for " ++ (show e) ++ " is not yet implemented.")

--match _ _ err = err
--match (x:xs) (HsMatch _ _ (p@(HsPVar _):ps) rhs []:ms) err = ECase (
match (x:xs) (m@(HsMatch _ _ (p:ps) rhs []):ms) err
 | isConHsP p = ECase x []
 | isVarHsP p   = match xs (subst p x (m:ms)) err
 |otherwise   = match xs (m:ms) (match xs ms err)
 where
	 subst (HsPVar (HsIdent i)) (EVar (Name n)) ms = subst' i n ms
	 subst' _ _ [] = []
	 subst' x v (HsMatch l n ps (HsUnGuardedRhs rhs) decls:ms) = (HsMatch l n ps (HsUnGuardedRhs (substHsExp x v rhs)) decls):subst' x v ms
match [] ms = 
match (x:xs) (HsMatch _ _ ps rhs _:ms) err = error ("local declarations not implemented yet (where)")
match xs ms err = error ("unimplemented match: " ++ (show xs) ++ " --> " ++ (show ms))
match xs ms err = EVar (mkName ("match " ++ (show xs) ++ " " ++ (show ms)))

-- -----------------------------------------------------------------------------

translateDecl (HsPatBind _ (HsPVar n) rhs _) =
	Decl (EVar (translateName n)) (translateRhs rhs)
translateDecl (HsFunBind ms@((HsMatch _ n ps _ _):_)) =
	Decl (EVar (translateName n)) (ELam xs (match xs ms (error ("PMC Failure!"))))
	where xs = map (\x -> EVar (mkName x)) $ mkPNames (length ps)

-- -----------------------------------------------------------------------------

translateModule (HsModule _ (Language.Haskell.Syntax.Module name) _ _ decls) =
	Core.Module (mkName name) (map translateDecl decls)
