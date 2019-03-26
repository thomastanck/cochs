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
            parseTest parseCocSyntax input
        "eval":systemType:filename:_ -> do
            case readMaybe systemType :: Maybe Int of
                Just systemNum -> do
                    input <- if filename == "-"
                        then getContents
                        else readFile filename
                    case parse parseCocSyntax filename input of
                        Left err -> do
                            putStr $ errorBundlePretty err
                        Right expr ->
                            putStrLn $ (show (fromCocSyntax expr)) ++ "\n=>\n" ++ (show val) ++ "\n:\n" ++ (show typ)
                            where settings = systemNumToSettings systemNum
                                  val = cocNorm settings (fromCocSyntax expr)
                                  typ = cocType settings [] (fromCocSyntax expr)
                Nothing -> do
                    putStrLn "Invalid system type"
        "findtype":systemType:filename:_ -> do
            case readMaybe systemType :: Maybe Int of
                Just systemNum -> do
                    input <- if filename == "-"
                        then getContents
                        else readFile filename
                    case parse parseCocSyntax filename input of
                        Left err -> do
                            putStr $ errorBundlePretty err
                        Right expr ->
                            putStrLn $ (show (fromCocSyntax expr)) ++ "\n:\n" ++ show typ
                            where settings = systemNumToSettings systemNum
                                  typ = cocType settings [] (fromCocSyntax expr)
                Nothing -> do
                    putStrLn "Invalid system type"
        "checktype":systemType:prooffilename:propositionfilename:_ -> do
            case readMaybe systemType :: Maybe Int of
                Just systemNum -> do
                    proofinput <- readFile prooffilename
                    propositioninput <- readFile propositionfilename
                    case parse parseCocSyntax prooffilename proofinput of
                        Left err -> do
                            putStr $ errorBundlePretty err
                        Right proofexpr ->
                            case parse parseCocSyntax propositionfilename propositioninput of
                                Left err -> do
                                    putStr $ errorBundlePretty err
                                Right propositionexpr ->
                                    putStrLn ((show (fromCocSyntax proofexpr))
                                            ++ "\n=>\n"
                                            ++ (show proofval)
                                            ++ "\n:\n"
                                            ++ (show prooftyp)
                                            ++ (if prooftyp == propositionval then "\n==\n" else "\n!=\n")
                                            ++ (show propositionval))
                                    where settings = systemNumToSettings systemNum
                                          proofval = cocNorm settings (fromCocSyntax proofexpr)
                                          Right prooftyp = cocType settings [] (fromCocSyntax proofexpr)
                                          propositionval = cocNorm settings (fromCocSyntax propositionexpr)
                Nothing -> do
                    putStrLn "Invalid system type"
        _ -> putStrLn "Invalid action"
