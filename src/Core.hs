module Core where

gensupply :: Char -> [String]
gensupply c = map (\i -> [c] ++ "_" ++ (show i)) [0..]

pSupply = gensupply 'p'
mkPNames n = take n pSupply

data Name = Name String
	deriving (Show)

mkName str = Name str

data Type = Unknown
	deriving (Show)

data Literal = LInt Integer
	deriving (Show)

-- The Module data type, contains all the information regarding a single Module.
data Module = Module Name [Decl]
	deriving (Show)

moduleDecls (Module _ decls) = decls

data Decl = Decl Expr Expr
	deriving (Show)

data Alt = Alt Pat Expr
	deriving (Show)

aTrue expr = Alt (PCon (mkName "True") []) expr
aFalse expr = Alt (PCon (mkName "False") []) expr

-- Pattern data type, for all the nifty matching.
data Pat =
	PLit |
	PCon Name [Expr]
	deriving (Show)

-- The Expr data type, is the foundation of our language.
data Expr =
	ELit  Literal |
	EVar  Name |
	ECon  Name [Expr] |
	EApp  Expr Expr |
	ELam  [Expr] Expr |
	ELet  [Decl] Expr |
	EList [Expr] |
	ECase Expr [Alt]
	deriving (Show)