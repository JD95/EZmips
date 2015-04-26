module ScannerTypes where

import           Data.List
import           Data.Char

data TYPE = SYMBOL | INTEGER | PUNCTUATION | COMMENT | ASSIGNMENT | FUNC | MATH | ERROR deriving (Show, Eq)
data Token = Token TYPE String deriving (Eq)

instance Show Token where show (Token tokenType tokenStr) = (show tokenType) ++" " ++ tokenStr

{------------- Token tests ----------------------}
isNum :: Char -> Bool
isNum input = input `elem` (map intToDigit [0..9])

notNum :: String -> Bool
notNum token = not (and (map isNum token))

onlyNums :: String -> Bool
onlyNums input = filter (\a->a /= '.' && not (isNum a)) input == []

noDecimal :: String -> Bool
noDecimal input = filter (\a-> a == '.') input == []

singleDecimal :: String -> Bool
singleDecimal input = (length (filter (\a-> a == '.') input) == 1)

isAlphabet :: Char -> Bool
isAlphabet input = input `elem` (['a'..'z'] `union` ['A'..'Z'])

noPunctuation :: String -> Bool
noPunctuation input = filter (\a->a `elem` ['(',')','\'',':',',','=',';','[',']','{','}']) input == []

isComment :: String -> Bool
isComment input = (head input) == '#'

isMathOp :: String -> Bool
isMathOp input = input `elem` ["+","-","*","/","%"]

isLogicOp :: String -> Bool
isLogicOp input = input `elem` ["<",">","<=",">=","==","!="]

{-Utility functions-}

symbolsToString :: [Token] -> Maybe [String]

symbolsToString [(Token SYMBOL value)] = Just [value]

symbolsToString ((Token SYMBOL value):more) = do
    rest <- symbolsToString more
    Just (value:rest)

symbolsToString [] = Just []

symbolsToString _ = Nothing
