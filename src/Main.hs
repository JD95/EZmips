-- | Main entry point to the application.
module Main where


import System.IO;

import Scanner;
import MathParsing
import Compiler


{--}
main :: IO ()
main = do
    test_treeToMips
    --test_ParseMath_as
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