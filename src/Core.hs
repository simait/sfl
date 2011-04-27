module Core where

data Expr =
	EVar String |
	EApp Expr Expr |
	EBind Expr Expr |
	ELet Expr Expr Expr