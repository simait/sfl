module Parse where

import System.Exit

import Language.Haskell.Parser
import Language.Haskell.Syntax

-- Parse a single file, should produce a HsModule etc.
parseFile f = do
	putStrLn ("Reading: " ++ f)
	input <- readFile f
	putStrLn ("Parsing: " ++ f)
	let result = parseModuleWithMode (ParseMode f) input
	case result of
		ParseFailed (SrcLoc f l c) err ->
			putStrLn ("Error at line " ++ (show l) ++ " column " ++ (show c) ++ ": " ++ err) >> exitFailure
		ParseOk tree -> return ()
	let (ParseOk tree) = result
	return tree

moduleName (HsModule _ (Module name) _ _ _) = name
moduleDecls (HsModule _ _ _ _ decls) = decls