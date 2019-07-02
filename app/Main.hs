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

main :: IO ()
main = do
    args <- getArgs
    case args of
        "parsetest":filename:_ -> do
            input <- if filename == "-"
                then getContents
                else readFile filename
            parseTest parseCocProgram input
        "eval":systemType:defname:filename:_ -> do
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
                            let val = cocNorm settings expr
                            let typ = cocType settings [] expr
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
        "test":systemType:defname:filename:_ -> do
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
                            let val = cocNorm settings expr
                            let typ = cocType settings [] expr
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
        "testfail":systemType:defname:filename:_ -> do
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
                            let val = cocNorm settings expr
                            let typ = cocType settings [] expr
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
        "findtype":systemType:defname:filename:_ -> do
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
                            let val = cocNorm settings expr
                            let typ = cocType settings [] expr
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
        -- "checktype":systemType:prooffilename:propositionfilename:_ -> do
        --     case readMaybe systemType :: Maybe Int of
        --         Just systemNum -> do
        --             proofinput <- readFile prooffilename
        --             propositioninput <- readFile propositionfilename
        --             case parse parseCocProgram prooffilename proofinput of
        --                 Left err -> do
        --                     putStr $ errorBundlePretty err
        --                     exitFailure
        --                 Right proofprog ->
        --                     case parse parseCocProgram propositionfilename propositioninput of
        --                         Left err -> do
        --                             putStr $ errorBundlePretty err
        --                             exitFailure
        --                         Right propositionprog ->
        --                             putStrLn ((show proofexpr)
        --                                     ++ "\n=>\n"
        --                                     ++ (show proofval)
        --                                     ++ "\n:\n"
        --                                     ++ (show prooftyp)
        --                                     ++ (if prooftyp == propositionval then "\n==\n" else "\n!=\n")
        --                                     ++ (show propositionval))
        --                             where proofexpr = snd $ elemAt 0 $ fromCocProgram proofprog
        --                                   propositionexpr = snd $ elemAt 0 $ fromCocProgram propositionprog
        --                                   settings = systemNumToSettings systemNum
        --                                   proofval = cocNorm settings proofexpr
        --                                   Right prooftyp = cocType settings [] proofexpr
        --                                   propositionval = cocNorm settings propositionexpr
        --         Nothing -> do
        --             putStrLn "Invalid system type"
        _ -> putStrLn "Invalid action"
