module Parser where

import Data.Char

import Scanner
import ScannerTypes

import ParserTypes


gatherData :: [Token] -> Maybe ([Data], [Token])

gatherData [] = Nothing

gatherData ((Token SYMBOL "main"):(Token PUNCTUATION ":"):more) = Just ([],((Token SYMBOL "main"):(Token PUNCTUATION ":"):more))

gatherData (token:more) = do
    (dataVar, moreCode') <- getUntil_SemiColon more 0
    (moreData, functions) <- gatherData moreCode'
    dataVar' <- processData (token:dataVar)
    Just (dataVar':moreData,functions)


processData :: [Token] -> Maybe Data
processData ((Token SYMBOL "array"):(Token SYMBOL dataType):(Token SYMBOL name):(Token ASSIGNMENT "="):(Token STRING size):(Token PUNCTUATION ";"):[]) =
    case dataType of
        "numbers" -> Just (Array dataType name size)
        "chars"   -> Just (Array dataType name size)
        _ -> Nothing

processData ((Token SYMBOL "string"):(Token SYMBOL name):(Token ASSIGNMENT "="):(Token STRING lit):(Token PUNCTUATION ";"):[]) = do
    Just (Global "string" name lit)

processData ((Token SYMBOL "char"):(Token SYMBOL name):(Token ASSIGNMENT "="):(Token CHAR lit):(Token PUNCTUATION ";"):[]) = do
    Just (Global "char" name lit)

processData ((Token SYMBOL "number"):(Token SYMBOL name):(Token ASSIGNMENT "="):(Token INTEGER num):(Token PUNCTUATION ";"):[]) = do
    Just (Global "number" name num)

processData _ = Nothing

gatherFunctions :: [Token] -> Maybe [Function]
gatherFunctions [] = Just []
gatherFunctions input = do
    (function, more) <- gatherFunction input
    rest <- gatherFunctions more
    Just (function:rest)

gatherFunction :: [Token] -> Maybe (Function, [Token])
gatherFunction ((Token SYMBOL name):rest) = do
    (args, more) <- getUntil_Colon rest 0
    let (argRegs,newTable) = argsToReg (init args) [] -- Assigns save registers to the arguements
    args' <- symbolsToString argRegs                  -- Extracts values from tokens
    (body, nextFunc) <- gatherStatements more (Fdata name newTable ("",0,0,0))
    Just ((Function name args' body), nextFunc)

gatherFunction _ = Nothing

gatherStatements :: [Token] -> Fdata -> Maybe ([Statement], [Token])

-- If the file ends and function isn't closed
gatherStatements [] _ = Nothing

-- Function ends properly
gatherStatements ((Token SYMBOL "end~"):more) _ = Just ([],more)

-- More statements in function
gatherStatements tokens fData = do
    (statement, more) <- getStatement tokens
    (statement', fData') <- processStatement statement fData
    (rest, nextFunc) <- gatherStatements more fData'
    Just ((statement':rest), nextFunc)


getStatement :: [Token] -> Maybe ([Token],[Token])

-- getUntil_end&
getStatement ((Token SYMBOL "if"):(Token PUNCTUATION "("):more) = do
    (condition, rest) <- getParenExpr more 0
    case (head rest) of
        (Token PUNCTUATION "{") -> do
            (inner, rest') <- getCurlBraceExpr (tail rest) 0
            Just ((Token SYMBOL "if"):(Token PUNCTUATION "("):condition ++ (Token PUNCTUATION "{"):inner, rest')
        _ -> Nothing

getStatement ((Token SYMBOL "while"):(Token PUNCTUATION "("):more) = do
    (condition, rest) <- getParenExpr more 0
    case (head rest) of
        (Token PUNCTUATION "{") -> do
            (inner, rest') <- getCurlBraceExpr (tail rest) 0
            Just ((Token SYMBOL "while"):(Token PUNCTUATION "("):condition ++ (Token PUNCTUATION "{"):inner, rest')
        _ -> Nothing

getStatement ((Token SYMBOL "for"):(Token PUNCTUATION "("):more) = do
    (condition, rest) <- getParenExpr more 0
    case (head rest) of
        (Token PUNCTUATION "{") -> do
            (inner, rest') <- getCurlBraceExpr (tail rest) 0
            Just ((Token SYMBOL "for"):(Token PUNCTUATION "("):condition ++ (Token PUNCTUATION "{"):inner, rest')
        _ -> Nothing

-- getUntil_SemiColon
getStatement ((Token SYMBOL var):more) = do
    (statement, rest) <- getUntil_SemiColon more 0
    Just ((Token SYMBOL var):statement, rest)

getStatement ((Token PUNCTUATION "["):more) = do
    (statement, rest) <- getUntil_SemiColon more 0
    Just ((Token PUNCTUATION "["):statement, rest)

getStatement ((Token FUNC "~"):more) = do
    (statement, rest) <- getUntil_SemiColon more 0
    Just ((Token FUNC "~"):statement, rest)

getStatement _ = Nothing

processStatement :: [Token] -> Fdata -> Maybe (Statement, Fdata)

-- Case of assignment to a literal value
processStatement ((Token SYMBOL sym):(Token ASSIGNMENT _):(Token INTEGER num):(Token PUNCTUATION ";"):[]) (Fdata name table counts) =
    case lookUpVar sym table of
        Just reg -> Just ((Assignment [(Token SYMBOL reg)] [(Token INTEGER num)]), (Fdata name table counts))
        Nothing -> let tokens = ((Token SYMBOL sym):(Token ASSIGNMENT "="):(Token INTEGER num):(Token PUNCTUATION ";"):[])
                       newTable = (addToTable sym table)
                   in  processStatement tokens (Fdata name newTable counts)

-- Case of assignment to result of math
processStatement ((Token SYMBOL sym):(Token ASSIGNMENT _):(Token MATH _):more) (Fdata name table counts) =
    case lookUpVar sym table of
        Just reg -> case getUntil_SemiColon more 0 of
                        Just (math,_) -> let (math', newTable) = replaceMathVars math table in
                                         Just ((Assignment [(Token SYMBOL reg)] ((Token MATH "@"):init math')), (Fdata name newTable counts))
                        Nothing -> Nothing
        Nothing -> let tokens = ((Token SYMBOL sym):(Token ASSIGNMENT "="):(Token MATH "@"):more)
                       newTable = (addToTable sym table)
                   in  processStatement tokens (Fdata name newTable counts)


-- Case of assignment to return of a function
processStatement ((Token SYMBOL sym):(Token ASSIGNMENT _):(Token FUNC _):(Token SYMBOL fname):more) (Fdata name table counts) =
    case lookUpVar sym table of
        Just reg -> case getUntil_SemiColon more 0 of
                        Just (args,_) -> let (args', newTable) = argsToReg args table in
                                                    -- The name is the first arguement of the function
                                                    Just ((Assignment [(Token SYMBOL reg)] ((Token FUNC "~"):(Token SYMBOL fname):init args')), (Fdata name newTable counts))
                        _  -> Nothing
        Nothing -> let tokens = ((Token SYMBOL sym):(Token ASSIGNMENT "="):(Token FUNC "~"):(Token SYMBOL fname):more)
                       newTable = (addToTable sym table)
                   in  processStatement tokens (Fdata name newTable counts)

-- Case of assignment to value of an Array
processStatement ((Token SYMBOL sym):(Token ASSIGNMENT _):(Token PUNCTUATION "["):(Token SYMBOL arrName):(Token INTEGER index):(Token PUNCTUATION "]"):(Token PUNCTUATION ";"):[]) (Fdata name table counts) = do
    case lookUpVar sym table of
        Just reg -> Just ((Assignment [(Token SYMBOL reg)] ((Token PUNCTUATION "["):(Token SYMBOL arrName):(Token INTEGER index):(Token PUNCTUATION "]"):[]), (Fdata name table counts)))
        Nothing -> let tokens = ((Token SYMBOL sym):(Token ASSIGNMENT "="):(Token PUNCTUATION "["):(Token SYMBOL arrName):(Token INTEGER index):(Token PUNCTUATION "]"):(Token PUNCTUATION ";"):[])
                       newTable = (addToTable sym table)
                   in  processStatement tokens (Fdata name newTable counts)

-- Case of assignment of a char value to and array index
processStatement ((Token PUNCTUATION "["):(Token SYMBOL arrName):(Token INTEGER index):(Token PUNCTUATION "]"):(Token ASSIGNMENT _):(Token CHAR sym):(Token PUNCTUATION ";"):[]) (Fdata name table counts) = do
    Just ((Assignment ((Token PUNCTUATION "["):(Token SYMBOL arrName):(Token INTEGER index):(Token PUNCTUATION "]"):[]) [(Token CHAR sym)], (Fdata name table counts)))


-- Error case, Symbol begin assigned to another variable
processStatement ((Token SYMBOL sym):(Token ASSIGNMENT _):(Token SYMBOL _):more) _ = Nothing

-- Returning an immediate number value
processStatement ((Token SYMBOL "return"):(Token INTEGER num):(Token PUNCTUATION ";"):[]) (Fdata name table count) =
    Just ((Return (Token INTEGER num) (Token SYMBOL ("end_"++name))), (Fdata name table count))

-- Returning whats in a save register
processStatement ((Token SYMBOL "return"):(Token SYMBOL sym):(Token PUNCTUATION ";"):[]) (Fdata name table counts) = do
    case lookUpVar sym table of
        Just reg -> Just ((Return (Token SYMBOL reg) (Token SYMBOL ("end_"++name))), (Fdata name table counts))
        Nothing -> let tokens = ((Token SYMBOL "return"):(Token SYMBOL sym):(Token PUNCTUATION ";"):[])
                       newTable = (addToTable sym table)
                   in  processStatement tokens (Fdata name newTable counts)

-- Return without loading a return value
processStatement ((Token SYMBOL "return"):(Token PUNCTUATION ";"):[]) (Fdata name table counts) = do
   Just ((Return (Token SYMBOL "") (Token SYMBOL ("end_"++name))), (Fdata name table counts))

-- Call to a print Function
processStatement ((Token FUNC "~"):(Token SYMBOL "printChar"):(Token CHAR dataVar):(Token PUNCTUATION ";"):[]) fdata = do
    Just ((FunCALL (Token SYMBOL "printChar") [(Token CHAR dataVar)]), fdata)

processStatement ((Token FUNC "~"):(Token SYMBOL "printString"):(Token SYMBOL dataVar):(Token PUNCTUATION ";"):[]) fdata = do
    Just ((FunCALL (Token SYMBOL "printString") [(Token SYMBOL dataVar)]), fdata)

processStatement ((Token FUNC "~"):(Token SYMBOL "printInt"):(Token INTEGER dataVar):(Token PUNCTUATION ";"):[]) fdata = do
    Just ((FunCALL (Token SYMBOL "printInt") [(Token INTEGER dataVar)]), fdata)

-- Free function call, will not load a return value
processStatement ((Token FUNC "~"):(Token SYMBOL fname):more) fdata = do
    (args,_) <- getUntil_SemiColon more 0 -- Gets args and semi colon
    Just ((FunCALL (Token SYMBOL fname) (init args)), fdata)

-- IF statement
processStatement ((Token SYMBOL "if"):(Token PUNCTUATION "("):(Token SYMBOL var):(Token SYMBOL logic):value:(Token PUNCTUATION ")"):(Token PUNCTUATION "{"):rest) (Fdata name table (cName, ifs, whiles, fors)) = do
    case lookUpVar var table of
        Just var' -> do
            let inner = (init rest) ++ [(Token SYMBOL "end~")] -- To make the gather statments end
            let newCName = cName++"_if"++[(intToDigit ifs)]
            let ifName = name++ newCName
            (innerStatements, _) <- gatherStatements inner (Fdata name table (newCName,ifs, whiles, fors)) -- Any other control statements in this one will have names in relation to it eg. func_if0_if1
            Just ((If ifName (Condition (Token SYMBOL var') (Token SYMBOL logic) value) innerStatements), (Fdata name table (newCName, ifs+1, whiles, fors)))
        Nothing -> let tokens = ((Token SYMBOL "if"):(Token PUNCTUATION "("):(Token SYMBOL var):(Token SYMBOL logic):value:(Token PUNCTUATION ")"):(Token PUNCTUATION "{"):rest)
                       newTable = (addToTable var table)
                   in  processStatement tokens (Fdata name newTable (cName, ifs, whiles, fors))

-- While Loop statement
processStatement ((Token SYMBOL "while"):(Token PUNCTUATION "("):(Token SYMBOL var):(Token SYMBOL logic):value:(Token PUNCTUATION ")"):(Token PUNCTUATION "{"):rest) (Fdata name table (cName, ifs, whiles, fors)) = do
    case lookUpVar var table of
        Just var' -> do
            let inner = (init rest) ++ [(Token SYMBOL "end~")] -- To make the gather statments end
            let newCName = cName++"_while"++[(intToDigit ifs)]
            let wName = name++newCName
            (innerStatements, _) <- gatherStatements inner (Fdata name table (newCName,ifs, whiles, fors)) -- Any other control statements in this one will have names in relation to it eg. func_if0_if1
            Just ((WhileLoop wName (Condition (Token SYMBOL var') (Token SYMBOL logic) value) innerStatements), (Fdata name table (newCName,ifs, whiles+1, fors)))
        Nothing -> let tokens = ((Token SYMBOL "while"):(Token PUNCTUATION "("):(Token SYMBOL var):(Token SYMBOL logic):value:(Token PUNCTUATION ")"):(Token PUNCTUATION "{"):rest)
                       newTable = (addToTable var table)
                   in  processStatement tokens (Fdata name newTable (cName, ifs, whiles, fors))
processStatement _ _ = Nothing

{-Tests-}
test_ProcessStatement :: IO ()
test_ProcessStatement = do
    case scan "while(i<5){if(i > 5){return;}}" of
        Just tokens -> do
            putStrLn (show tokens)
            case processStatement tokens (Fdata "main" ["b","a"] ("",0,0,0)) of
                Just (statement,_) -> putStrLn (show statement)
                Nothing -> putStrLn "Could not process Statement"
        Nothing -> putStrLn "Bad Test string"

test_getStatement :: IO ()
test_getStatement = do
    case scan "if(i<5){return;} a + b;" of
        Just tokens -> do
            putStrLn (show tokens)
            case getStatement tokens  of
                                Just (statement,rest) -> do
                                    putStrLn (show rest)
                                    putStrLn (show statement)
                                Nothing -> putStrLn "Could not get Statement"
        Nothing -> putStrLn "Bad Test string"

test_gatherStatements :: IO ()
test_gatherStatements = do
    case scan "a = 1; b = 1; c = 2; d = @ (a + b) / c; return d; end~" of
        Just tokens -> do
            --putStrLn (show tokens)
            case gatherStatements tokens (Fdata "main" [] ("",0,0,0)) of
                Just (statements,rest) -> do
                    putStrLn (show rest)
                    mapM_ (putStrLn . show) statements
                Nothing -> putStrLn "Could not get Statements"
        Nothing -> putStrLn "Bad Test string"

test_gatherFunction :: IO ()
test_gatherFunction = do
    case scan "main: a=1; b = 1; c = 2; d = @ (a + b) / c; return d; end~" of
        Just tokens -> do
            putStrLn (show tokens)
            case gatherFunction tokens of
                Just (function, rest) -> do
                    putStrLn (show rest)
                    putStrLn (show function)
                Nothing -> putStrLn "Could not get function"
        Nothing -> putStrLn "Bad Test string"