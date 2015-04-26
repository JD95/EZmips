module ParserTypes where

import Data.List

import Scanner
import ScannerTypes

data Function = Function String [String] [Statement] deriving (Show)

data Fdata = Fdata String [String] (Int,Int,Int) deriving (Show)

data Statement = Assignment [Token] [Token] | Return Token | Ending Token | ForLoop Condition [Statement] | WhileLoop Condition [Statement] deriving (Show)

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

replaceMathVars [] _ = ([],[])

{- Old getParenExpr
getParenExpr :: [Token] -> Int -> Maybe ([Token], [Token])
-- Gather tokens inside of a set of parenthesis, returns with closing paren
-- Assumes first open paren has been taken of already

getParenExpr ((Token PUNCTUATION ")"):more) a
--    | more == [] && a == 0 = Just ([(Token PUNCTUATION ")")], [])
    | more == [] && a /= 0 = Nothing
    | a == 0 = Just ([(Token PUNCTUATION ")")], more)
    | a > 0  = do
       (rest, left) <- getParenExpr more (a-1)
       Just ((Token PUNCTUATION ")"):rest, left)

getParenExpr (token: more) a
    |token == (Token PUNCTUATION "(") =
        case getParenExpr more (a+1) of
            Just (function, rest)   -> Just ((token:function), rest)
            Nothing                 -> Nothing
    |otherwise =
        case getParenExpr more a of
            Just (function, rest)   -> Just ((token:function), rest)
            Nothing                 -> Nothing

getParenExpr [] a
    | a > 0 = Nothing
    | otherwise = Just ([], [])
-}

{-Tests-}
test_getExpr :: IO ()
test_getExpr =
    case scan "foo: a = @ 4 + 5; return a; end~" of
        Just tokens -> do
            case getUntil_FuncEnd (tail tokens) 0 of
                Just (expr,_) -> putStrLn (show expr)
                Nothing -> putStrLn "Couldn't get expression"
        Nothing -> putStrLn "Bad test string"

test_replaceVars :: IO ()
test_replaceVars = do
    case scan "a + b + c" of
        Just tokens -> let (math, _) = replaceMathVars tokens ["a","b","c"]
                       in putStrLn (show math)
        Nothing -> putStrLn "Bad test string"