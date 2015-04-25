module ParserTypes where

import ScannerTypes

data MathTree = AddNode MathTree MathTree | SubNode MathTree MathTree |
                MulNode MathTree MathTree | DivNode MathTree MathTree | ModNode MathTree MathTree |
                Leaf String deriving (Show)


{- Utility Functions -}

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