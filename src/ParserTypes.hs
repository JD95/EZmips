module ParserTypes where

import Data.List

import Scanner
import ScannerTypes

data Data = Array String String Int | Global String String String

data Function = Function String [String] [Statement] deriving (Show)

data Fdata = Fdata String [String] (String, Int,Int,Int) deriving (Show)

data Statement = Assignment [Token] [Token] | FunCALL Token [Token] | Return Token Token | Ending Token |
                If String Condition [Statement] | WhileLoop String Condition [Statement] | ForLoop  String Token [Statement] deriving (Show)

data Condition = Condition Token Token Token deriving (Show)



{- Utility Functions -}

getExpr :: String -> String -> ([Token] -> Int -> Maybe ([Token], [Token]))
-- Assumes first startSym has been removed already, returns with last stopSym

getExpr startSym stopSym (token:more) a =
    if token == (Token PUNCTUATION stopSym) then
        if more == [] && a /= 0 then Nothing
            else if a == 0 then Just ([(Token PUNCTUATION stopSym)], more)
                else if a > 0  then do
                   (rest, left) <- getExpr startSym stopSym more (a-1)
                   Just ((Token PUNCTUATION stopSym):rest, left)
                   else Nothing
    else if token == (Token PUNCTUATION startSym) then
        case getExpr startSym stopSym more (a+1) of
            Just (function, rest)   -> Just ((token:function), rest)
            Nothing                 -> Nothing
    else case getExpr startSym stopSym more a of
            Just (function, rest)   -> Just ((token:function), rest)
            Nothing                 -> Nothing

getExpr _ _ [] a
    | a > 0 = Nothing
    | otherwise = Nothing --Just ([], [])

getParenExpr = getExpr "(" ")"
getString = getExpr "\"" "\""
getBracketExpr = getExpr "[" "]"
getCurlBraceExpr = getExpr "{" "}"
getUntil_SemiColon = getExpr "" ";"
getUntil_Colon = getExpr "" ":"
getUntil_FuncEnd = getExpr "" "end~"

lookUpVar :: String -> [String] -> Maybe String
lookUpVar var table =
    case elemIndex var table of
        Just index -> Just ("$s" ++ (show index))
        Nothing -> Nothing

addToTable :: String -> [String] -> [String]
addToTable var table = (table ++ [var])

assignReg :: String -> [String] -> (String, [String])
assignReg var table =
    case lookUpVar var table of
      Just register -> (register, table)
      Nothing -> assignReg var (addToTable var table)

replaceMathVars :: [Token] -> [String] -> ([Token], [String])
replaceMathVars ((Token SYMBOL var):more) table
    | isMathOp var = let (rest, table') = replaceMathVars more table in
                         (((Token SYMBOL var):rest),table')
    | otherwise = let (reg, table')    = assignReg var table
                      (rest, newTable) = replaceMathVars more table'
                  in ((Token SYMBOL reg):rest, newTable)

replaceMathVars (token:more) table =
    let (rest, newTable) = replaceMathVars more table in
        (token:rest, newTable)

replaceMathVars [] table = ([],table)

argsToReg :: [Token] -> [String] -> ([Token], [String])
argsToReg ((Token SYMBOL var):more) table = 
    let (reg, table')    = assignReg var table
        (rest, newTable) = argsToReg more table'
    in ((Token SYMBOL reg):rest, newTable)
argsToReg ((Token INTEGER num): more) table =
    let (rest, table') = argsToReg more table in
        (((Token INTEGER num):rest),table')

argsToReg (token:more) table =
    let (rest, newTable) = argsToReg more table in
        (token:rest, newTable)

argsToReg [] table = ([], table)

{--- The subtrees are converted from Maybe values to values ---------------------------------}
mergeMaybes :: [Maybe [a]] -> Maybe [a]
mergeMaybes [] = Just []
mergeMaybes [value] = case value of
                           Just value' -> Just value'
                           Nothing     -> Nothing

mergeMaybes (value:rest) =
    let values = mergeMaybes rest in
    case values of
        Just values' -> case value of
                             Just value' -> Just (value'++ values')
                             Nothing  -> Nothing
        Nothing      -> Nothing

{-Tests-}
test_getExpr :: IO ()
test_getExpr =
    case scan "\"this is a test\"" of
        Just tokens -> do
            putStrLn (show tokens)
            case getString (tail tokens) 0 of
                Just (expr,_) -> putStrLn (show expr)
                Nothing -> putStrLn "Couldn't get expression"
        Nothing -> putStrLn "Bad test string"

test_replaceVars :: IO ()
test_replaceVars = do
    case scan "a + b + c" of
        Just tokens -> let (math, _) = replaceMathVars tokens ["a","b","c"]
                       in putStrLn (show math)
        Nothing -> putStrLn "Bad test string"