module Main where

-- import Control.Monad (void)
-- import Control.Monad.Combinators.Expr -- from parser-combinators
import Text.Read
import Text.Megaparsec
import System.Environment

import CocExpr
import CocEval
import CocParser

main :: IO ()
main = do
    args <- getArgs
    case args of
        "parsetest":filename:_ -> do
            input <- if filename == "-"
                then getContents
                else readFile filename
            parseTest parseCocProgram input
        "eval":systemType:filename:_ -> do
            case readMaybe systemType :: Maybe Int of
                Just systemNum -> do
                    input <- if filename == "-"
                        then getContents
                        else readFile filename
                    case parse parseCocProgram filename input of
                        Left err -> do
                            putStr $ errorBundlePretty err
                        Right prog ->
                            putStrLn $ (show expr) ++ "\n=>\n" ++ (show val) ++ "\n:\n" ++ (show typ)
                            where expr = fromCocProgram prog
                                  settings = systemNumToSettings systemNum
                                  val = cocNorm settings expr
                                  typ = cocType settings [] expr
                Nothing -> do
                    putStrLn "Invalid system type"
        "findtype":systemType:filename:_ -> do
            case readMaybe systemType :: Maybe Int of
                Just systemNum -> do
                    input <- if filename == "-"
                        then getContents
                        else readFile filename
                    case parse parseCocProgram filename input of
                        Left err -> do
                            putStr $ errorBundlePretty err
                        Right prog ->
                            putStrLn $ (show expr) ++ "\n:\n" ++ (show typ)
                            where expr = fromCocProgram prog
                                  settings = systemNumToSettings systemNum
                                  typ = cocType settings [] expr
                Nothing -> do
                    putStrLn "Invalid system type"
        "checktype":systemType:prooffilename:propositionfilename:_ -> do
            case readMaybe systemType :: Maybe Int of
                Just systemNum -> do
                    proofinput <- readFile prooffilename
                    propositioninput <- readFile propositionfilename
                    case parse parseCocProgram prooffilename proofinput of
                        Left err -> do
                            putStr $ errorBundlePretty err
                        Right proofprog ->
                            case parse parseCocProgram propositionfilename propositioninput of
                                Left err -> do
                                    putStr $ errorBundlePretty err
                                Right propositionprog ->
                                    putStrLn ((show proofexpr)
                                            ++ "\n=>\n"
                                            ++ (show proofval)
                                            ++ "\n:\n"
                                            ++ (show prooftyp)
                                            ++ (if prooftyp == propositionval then "\n==\n" else "\n!=\n")
                                            ++ (show propositionval))
                                    where proofexpr = fromCocProgram proofprog
                                          propositionexpr = fromCocProgram propositionprog
                                          settings = systemNumToSettings systemNum
                                          proofval = cocNorm settings proofexpr
                                          Right prooftyp = cocType settings [] proofexpr
                                          propositionval = cocNorm settings propositionexpr
                Nothing -> do
                    putStrLn "Invalid system type"
        _ -> putStrLn "Invalid action"
