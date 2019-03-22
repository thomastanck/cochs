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
                            putStrLn $ (show val) ++ "\n:\n" ++ (show typ)
                            where (CocJudgement [] val [] typ) = cocEval systemNum (fromCocSyntax expr)
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
                            putStrLn $ show (cocFindType (cocInitState (systemNumToSettings systemNum)) (fromCocSyntax expr))
                Nothing -> do
                    putStrLn "Invalid system type"
        _ -> putStrLn "Invalid action"
