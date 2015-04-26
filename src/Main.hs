-- | Main entry point to the application.
module Main where


import System.IO;

import Scanner
import Parser
import ParserTypes
import MathConversion
import Compiler


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
            case gatherFunction tokens of
                Just (function,_) -> do
                    --putStrLn (show function)
                    case convertFunction function of
                        Just instructions -> mapM_ (putStrLn) instructions
                        Nothing -> putStrLn "Could not generate instructions"
                Nothing -> putStrLn "Could not gather function"
        Nothing -> putStrLn "Something went wrong in scanning"
    hClose handle
    
