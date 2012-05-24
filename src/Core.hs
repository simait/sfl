module Core where

gensupply :: Char -> [String]
gensupply c = map (\i -> [c] ++ "_" ++ (show i)) [0..]

pSupply = gensupply 'p'
mkPNames n = take n pSupply

data Name = Name String
	deriving (Show)

mkName s = Name s
name (Name s) = s

data Type = Unknown
	deriving (Show)

data Lit =
	LInt Integer   |
	LChar Char     |
	LString String
	deriving (Show)

data Module = Module Name [Decl]
	deriving (Show)

moduleDecls (Module _ decls) = decls

data Decl = Decl Expr Expr
	deriving (Show)

data Alt = Alt Pat Expr
	deriving (Show)

aTrue expr = Alt (PCon (mkName "True") []) expr
aFalse expr = Alt (PCon (mkName "False") []) expr

data Pat =
	PLit Lit|
	PVar Name |
	PCon Name [Pat]
	deriving (Show)

data Expr =
	ELit  Lit |
	EVar  Name |
	ECon  Name [Expr] |
	EApp  Expr Expr |
	ELam  [Expr] Expr |
	ELet  [Decl] Expr |
	EList [Expr] |
	ECase Expr [Alt]
	deriving (Show)

data Match =
	Fatbar 