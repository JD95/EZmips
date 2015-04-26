-- | Main entry point to the application.
module Main where


import System.IO;

import Scanner
import Parser
import ParserTypes
import MathConversion
import Compiler
import CompilerTypes


{--}
main :: IO ()
main = do
    --test_getExpr
    --test_treeToMips
    --test_ParseMath_as
    --test_ProcessStatement
    --test_replaceVars
    --test_convertStatement
    --test_getStatement
    --test_gatherFunction
    --test_gatherStatements
    --test_convertFunction
    --test_pushStack
    interface "EZmips: "

interface :: String -> IO ()
interface filePath = do
    handle <- openFile "testFile" ReadMode
    ezmips <- hGetContents handle
    --putStrLn ezmips
    case scan ezmips of
        Just tokens -> do
            --putStrLn (show tokens)
            case gatherData tokens of
                Just (dataSec, functions) -> do
                    --putStrLn (show functions)
                    case gatherFunctions functions of
                        Just functions' -> do
                            --putStrLn (show function)
                            case mergeMaybes (Prelude.map convertFunction functions') of
                                Just program -> mapM_ (putStrLn) program
                                Nothing -> putStrLn "Could not compile functions"
                        Nothing -> putStrLn "Error in function formatting"
                Nothing -> putStrLn "Error in reading data section"
        Nothing -> putStrLn "Bad syntax...somewhere"
    hClose handle
