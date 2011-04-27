module Main where

import List
import System
import System.Exit
import Language.Haskell.Parser
import Language.Haskell.Syntax
import Language.Haskell.Pretty

-- Check a filename, .sfl is the only valid extension, pointless right now but...
checkFileName f = do
	let nameOk = isSuffixOf ".sfl"
	if isSuffixOf ".sfl" f then return () else putStrLn ("Invalid file: " ++ f) >> exitFailure
	
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

-- For now, just assume that all parameters are files that should be parsed, compiled, etc.
main = do
	-- Check all filenames, I know pointless right now...
	files <- getArgs
	mapM checkFileName files
	
	-- Parse all files...
	parsed <- mapM parseFile files
	
	mapM print parsed
	
	-- ...and for now just pretty print them.
	let pps = map prettyPrint parsed
	mapM putStrLn pps
	
	-- Well success is a strong word ;)
	exitSuccess