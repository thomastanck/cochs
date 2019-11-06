module Main where

-- import Control.Monad (void)
-- import Control.Monad.Combinators.Expr -- from parser-combinators
import Text.Read
import Text.Megaparsec
import System.Environment
import System.Exit
import Data.Map.Strict as Map
import Data.Maybe
import Path
import Control.Monad.Reader

import CocExpr
import CocEval
import CocParser
import CocSyntax

parseProgramOrError :: String -> String -> ([CocImport],[CocDefinition])
parseProgramOrError filename filecontents
    = case parse parseCocProgram filename filecontents of
        Left err -> error (errorBundlePretty err)
        Right prog -> prog

preprocessimports :: String -> [CocImport] -> IO [CocDefinition]
preprocessimports filepath imports = fmap concat $ traverse (preprocessimport filepath) imports

preprocessimport :: String -> CocImport -> IO [CocDefinition]
preprocessimport filepath (CocImport packagename defnamemap) = do
    filepath <- parseRelFile filepath
    packagepath <- parseRelFile packagename
    let packagepathstring = toFilePath $ (parent filepath) </> packagepath
    packagecontents <- readFile packagepathstring
    let (packageimports,packagedefs) = parseProgramOrError packagepathstring packagecontents
    packageimporteddefs <- preprocessimports packagepathstring packageimports
    return $ packageimporteddefs ++ packagedefs

parsetestcmd :: String -> IO ()
parsetestcmd filename = do
    input <- if filename == "-"
             then getContents
             else readFile filename
    parseTest parseCocProgram input

evalcmd :: String -> String -> String -> IO ()
evalcmd systemType defname filename = do
    case readMaybe systemType :: Maybe Int of
        Just systemNum -> do
            input <- if filename == "-"
                then getContents
                else readFile filename
            case parse parseCocProgram filename input of
                Left err -> do
                    putStr $ errorBundlePretty err
                    exitFailure
                Right (imports,localdefs) -> do
                    importeddefs <- preprocessimports filename imports
                    let defs = importeddefs ++ localdefs
                    let defmap = fromCocDefs defs
                    let expr = fromJust $ Map.lookup defname defmap
                    let settings = systemNumToSettings systemNum
                    let val = runReader (cocNorm expr) settings
                    let typ = cocType settings expr
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
        Nothing -> do
            putStrLn "Invalid system type"

testcmd :: String -> String -> String -> IO ()
testcmd systemType defname filename = do
    case readMaybe systemType :: Maybe Int of
        Just systemNum -> do
            input <- if filename == "-"
                then getContents
                else readFile filename
            case parse parseCocProgram filename input of
                Left err -> do
                    putStr $ errorBundlePretty err
                    exitFailure
                Right (imports,localdefs) -> do
                    importeddefs <- preprocessimports filename imports
                    let defs = importeddefs ++ localdefs
                    let defmap = fromCocDefs defs
                    let expr = fromJust $ Map.lookup defname defmap
                    let settings = systemNumToSettings systemNum
                    let val = runReader (cocNorm expr) settings
                    let typ = cocType settings expr
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
        Nothing -> do
            putStrLn "Invalid system type"

testfailcmd :: String -> String -> String -> IO ()
testfailcmd systemType defname filename = do
    case readMaybe systemType :: Maybe Int of
        Just systemNum -> do
            input <- if filename == "-"
                then getContents
                else readFile filename
            case parse parseCocProgram filename input of
                Left err -> do
                    putStr $ errorBundlePretty err
                    exitFailure
                Right (imports,localdefs) -> do
                    importeddefs <- preprocessimports filename imports
                    let defs = importeddefs ++ localdefs
                    let defmap = fromCocDefs defs
                    let expr = fromJust $ Map.lookup defname defmap
                    let settings = systemNumToSettings systemNum
                    let val = runReader (cocNorm expr) settings
                    let typ = cocType settings expr
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
        Nothing -> do
            putStrLn "Invalid system type"

findtypecmd :: String -> String -> String -> IO ()
findtypecmd systemType defname filename = do
    case readMaybe systemType :: Maybe Int of
        Just systemNum -> do
            input <- if filename == "-"
                then getContents
                else readFile filename
            case parse parseCocProgram filename input of
                Left err -> do
                    putStr $ errorBundlePretty err
                    exitFailure
                Right (imports,localdefs) -> do
                    importeddefs <- preprocessimports filename imports
                    let defs = importeddefs ++ localdefs
                    let defmap = fromCocDefs defs
                    let expr = fromJust $ Map.lookup defname defmap
                    let settings = systemNumToSettings systemNum
                    let val = runReader (cocNorm expr) settings
                    let typ = cocType settings expr
                    case typ of
                        Left err -> do
                            putStrLn $ show expr
                            putStrLn $ ":"
                            putStrLn $ show err
                        Right typ -> do
                            putStrLn $ show expr
                            putStrLn $ ":"
                            putStrLn $ show typ
        Nothing -> do
            putStrLn "Invalid system type"

main :: IO ()
main = do
    args <- getArgs
    case args of
        "parsetest":filename:_ -> parsetestcmd filename
        "eval":systemType:defname:filename:_ -> evalcmd systemType defname filename
        "test":systemType:defname:filename:_ -> testcmd systemType defname filename
        "testfail":systemType:defname:filename:_ -> testfailcmd systemType defname filename
        "findtype":systemType:defname:filename:_ -> findtypecmd systemType defname filename
        _ -> putStrLn "Invalid action"
