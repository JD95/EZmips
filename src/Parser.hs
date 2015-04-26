module Parser where

import Scanner
import ScannerTypes

import ParserTypes

gatherFunction :: [Token] -> Maybe (Function, [Token])
gatherFunction ((Token SYMBOL name):rest) = do
    (args, more) <- getUntil_Colon rest 0
    args' <- symbolsToString (init args)
    (body, nextFunc) <- gatherStatements more (Fdata name args'(0,0,0))
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
                                         Just ((Assignment [(Token SYMBOL reg)] (init math')), (Fdata name table counts))
                        Nothing -> Nothing
        Nothing -> let tokens = ((Token SYMBOL sym):(Token ASSIGNMENT "="):(Token MATH "@"):more)
                       newTable = (addToTable sym table)
                   in  processStatement tokens (Fdata name newTable counts)


-- Case of assignment to return of a function
processStatement ((Token SYMBOL sym):(Token ASSIGNMENT _):(Token FUNC _):more) (Fdata _ table _) = Nothing

-- Case of assignment to value of an Array
processStatement ((Token SYMBOL sym):(Token ASSIGNMENT _):(Token PUNCTUATION "["):more) (Fdata _ table _) = Nothing

-- Error case, Symbol begin assigned to another variable
processStatement ((Token SYMBOL sym):(Token ASSIGNMENT _):(Token SYMBOL _):more) _ = Nothing

-- Returning an immediate number value
processStatement ((Token SYMBOL "return"):(Token INTEGER num):(Token PUNCTUATION ";"):[]) (Fdata name table count) =
    Just ((Return (Token INTEGER num)), (Fdata name table count))

-- Returning whats in a save register
processStatement ((Token SYMBOL "return"):(Token SYMBOL sym):(Token PUNCTUATION ";"):[]) (Fdata name table counts) = do
    case lookUpVar sym table of
        Just reg -> Just ((Return (Token SYMBOL reg)), (Fdata name table counts))
        Nothing -> let tokens = ((Token SYMBOL "return"):(Token SYMBOL sym):(Token PUNCTUATION ";"):[])
                       newTable = (addToTable sym table)
                   in  processStatement tokens (Fdata name newTable counts)

processStatement _ _ = Nothing

{-Tests-}
test_ProcessStatement :: IO ()
test_ProcessStatement = do
    case scan "return 5;" of
        Just tokens -> case processStatement tokens (Fdata "main" ["b","a"] (0,0,0)) of
                            Just (statement,_) -> putStrLn (show statement)
                            Nothing -> putStrLn "Could not process Statement"
        Nothing -> putStrLn "Bad Test string"

test_getStatement :: IO ()
test_getStatement = do
    case scan "[arr 5] = 25;" of
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
    case scan "a = 25; if (a < 10){return 6;} end~" of
        Just tokens -> do
            --putStrLn (show tokens)
            case gatherStatements tokens (Fdata "main" [] (0,0,0)) of
                Just (statements,rest) -> do
                    putStrLn (show rest)
                    mapM_ (putStrLn . show) statements
                Nothing -> putStrLn "Could not get Statements"
        Nothing -> putStrLn "Bad Test string"

test_gatherFunction :: IO ()
test_gatherFunction = do
    case scan "foo a b c: a = 25; return a; end~" of
        Just tokens -> do
            --putStrLn (show tokens)
            case gatherFunction tokens of
                Just (function, rest) -> do
                    putStrLn (show rest)
                    putStrLn (show function)
                Nothing -> putStrLn "Could not get function"
        Nothing -> putStrLn "Bad Test string"