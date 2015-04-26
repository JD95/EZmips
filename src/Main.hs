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
    test_gatherFunction
    --test_gatherStatements
    --test_convertFunction
    --test_pushStack
    --interface "EZmips: "

interface :: String -> IO ()
interface prompt = do
    putStr prompt
    hFlush stdout
    end <- hIsEOF stdin
    if end then print "End of File"
    else do
        input <- getLine
        case scan input of
            Just tokens -> do
                putStrLn (show tokens)
                interface "EZmips: "
            Nothing -> do
                print "Syntax Error"
                putStrLn " "
                interface "EZmips: "
