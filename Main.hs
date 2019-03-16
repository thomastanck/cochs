module Main where

-- import Control.Monad (void)
-- import Control.Monad.Combinators.Expr -- from parser-combinators
import Text.Read
import Text.Megaparsec
import System.Environment

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
            parseTest parseCocExpr input
        "eval":systemType:filename:_ -> do
            case readMaybe systemType :: Maybe Int of
                Just systemNum -> do
                    input <- if filename == "-"
                        then getContents
                        else readFile filename
                    case parse parseCocExpr filename input of
                        Left err -> do
                            putStr $ errorBundlePretty err
                        Right expr ->
                            putStrLn $ (show val) ++ "\n:\n" ++ (show typ)
                            where (val, typ) = cocEval systemNum expr
                Nothing -> do
                    putStrLn "Invalid system type"
        _ -> putStrLn "Invalid action"
