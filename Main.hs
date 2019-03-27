module Main where

-- import Control.Monad (void)
-- import Control.Monad.Combinators.Expr -- from parser-combinators
import Text.Read
import Text.Megaparsec
import System.Environment
import System.Exit

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
                            exitFailure
                        Right prog ->
                            case typ of
                                Left err -> do
                                    putStrLn $ show expr
                                    putStrLn $ "=>"
                                    putStrLn $ show val
                                    putStrLn $ ":"
                                    putStrLn $ show err
                                Right typ -> do
                                    putStrLn $ show expr
                                    putStrLn $ "=>"
                                    putStrLn $ show val
                                    putStrLn $ ":"
                                    putStrLn $ show typ
                            where expr = fromCocProgram prog
                                  settings = systemNumToSettings systemNum
                                  val = cocNorm settings expr
                                  typ = cocType settings [] expr
                Nothing -> do
                    putStrLn "Invalid system type"
        "test":systemType:filename:_ -> do
            case readMaybe systemType :: Maybe Int of
                Just systemNum -> do
                    input <- if filename == "-"
                        then getContents
                        else readFile filename
                    case parse parseCocProgram filename input of
                        Left err -> do
                            putStr $ errorBundlePretty err
                            exitFailure
                        Right prog ->
                            case typ of
                                Left err -> do
                                    putStrLn $ show expr
                                    putStrLn $ "=>"
                                    putStrLn $ show val
                                    putStrLn $ ":"
                                    putStrLn $ show err
                                    exitFailure
                                Right typ -> do
                                    exitSuccess
                            where expr = fromCocProgram prog
                                  settings = systemNumToSettings systemNum
                                  val = cocNorm settings expr
                                  typ = cocType settings [] expr
                Nothing -> do
                    putStrLn "Invalid system type"
        "testfail":systemType:filename:_ -> do
            case readMaybe systemType :: Maybe Int of
                Just systemNum -> do
                    input <- if filename == "-"
                        then getContents
                        else readFile filename
                    case parse parseCocProgram filename input of
                        Left err -> do
                            putStr $ errorBundlePretty err
                            exitFailure
                        Right prog ->
                            case typ of
                                Left err -> do
                                    exitSuccess
                                Right typ -> do
                                    putStrLn $ show expr
                                    putStrLn $ "=>"
                                    putStrLn $ show val
                                    putStrLn $ ":"
                                    putStrLn $ show typ
                                    exitFailure
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
                            exitFailure
                        Right prog ->
                            case typ of
                                Left err -> do
                                    putStrLn $ show expr
                                    putStrLn $ ":"
                                    putStrLn $ show err
                                Right typ -> do
                                    putStrLn $ show expr
                                    putStrLn $ ":"
                                    putStrLn $ show typ
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
                            exitFailure
                        Right proofprog ->
                            case parse parseCocProgram propositionfilename propositioninput of
                                Left err -> do
                                    putStr $ errorBundlePretty err
                                    exitFailure
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
