module MathConversion where

import Scanner
import ScannerTypes

import ParserTypes
import CompilerTypes

{-
    Grammar for Math Expressions
    <as>  -> <md> + <as> | <md> - <as> | <md>
    <md>  -> <p>  * <md> | <p>  / <md> | <p> % <md> | <p>
    <p>   -> ( <as )     | <Num>
    <Num> -> (Token INTEGER var)
-}

-- Interface for function
convertMath :: [Token] -> Maybe MathTree
convertMath math = convertMath_as ([], math)

convertMath_as :: ([Token],[Token]) -> Maybe MathTree

convertMath_as ([],[]) = Nothing

convertMath_as ([],[sym]) = convertMath_l sym

-- try to evaluate as an as
convertMath_as ([],(hd:rest)) =
    case hd of
        (Token SYMBOL "+") -> Nothing -- No variable before operator
        (Token SYMBOL "-") -> Nothing -- No variable before operator
        (Token SYMBOL var) -> convertMath_as([hd], rest)
        (Token PUNCTUATION "(") -> do
                    (paren, rest') <- getParenExpr rest 0
                    convertMath_as ((hd:paren), rest')
        _                  -> Nothing -- Somehow a bad symbol got in

-- Move tokens to the left side until an <as> operator is found
convertMath_as (left, (hd:rest)) =
    case hd of
        (Token SYMBOL "+") -> do
            left' <- convertMath_md ([],left)
            right <- convertMath_as ([], rest)
            Just (AddNode left' right)
        (Token SYMBOL "-") -> do
            left' <- convertMath_md ([],left)
            right <- convertMath_as ([], rest)
            Just (SubNode left' right)
        (Token SYMBOL var) -> convertMath_as (left ++ [hd],rest)
        (Token PUNCTUATION "(") -> do
                    (paren, rest') <- getParenExpr rest 0
                    let newLeft = left ++ (hd:paren)
                    convertMath_as (newLeft, rest')
        _ -> Nothing -- A non symbol was found

-- Expression failed <as> try as an md
convertMath_as (left, []) = convertMath_md ([], left)

{-@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@-}


{- Attempt to parse Multiplcation, Division, or Mod -}
convertMath_md :: ([Token],[Token]) -> Maybe MathTree

convertMath_md ([],[]) = Nothing

convertMath_md ([],[sym]) = convertMath_l sym

convertMath_md ([],(hd:rest)) =
    case hd of
        (Token SYMBOL "*") -> Nothing -- No variable before operator
        (Token SYMBOL "/") -> Nothing -- No variable before operator
        (Token SYMBOL "%") -> Nothing -- No variable before operator
        (Token SYMBOL var) -> convertMath_md([hd], rest)
        (Token PUNCTUATION "(") -> do
                    (paren, rest') <- getParenExpr rest 0
                    convertMath_md ((hd:paren), rest')
        _                  -> Nothing -- Somehow a bad symbol got in

-- What is in the left portion must either be a parenthesis expression or a var
convertMath_md (left, (hd:rest)) =
    case hd of
        (Token SYMBOL "*") -> do
            left' <- convertMath_p left
            right <- convertMath_md ([], rest)
            Just (MulNode left' right)
        (Token SYMBOL "/") -> do
            left' <- convertMath_p left
            right <- convertMath_md ([], rest)
            Just (DivNode left' right)
        (Token SYMBOL "%") -> do
            left' <- convertMath_p left
            right <- convertMath_md ([], rest)
            Just (ModNode left' right)
        (Token PUNCTUATION "(") -> do
                    (paren, rest') <- getParenExpr rest 0
                    let newLeft = left ++ (hd:paren)
                    convertMath_md (newLeft, rest')
        (Token SYMBOL var) -> convertMath_md (left ++ [hd], rest) -- Non operator, move token to left
        _ -> Nothing -- Non symbol token made its way in

-- Doesn't work as an <md> try <p>
convertMath_md (left, []) = convertMath_p  left

{-@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@-}


{- Attempt to parse a Parenthsized expression -}
convertMath_p  :: [Token] -> Maybe MathTree

convertMath_p ((Token PUNCTUATION "("):rest) = do
                (tokens, _) <- getParenExpr rest 0
                convertMath_as ([], init tokens) -- Go back to the top with the inner expression, without the last paren

convertMath_p  [num] = convertMath_l num

convertMath_p [] = Nothing

convertMath_p _ = Nothing

{- Attempt to parse a variable -}
convertMath_l  :: Token -> Maybe MathTree
-- The only token that will pass is a symbol that is not punctuation

{-@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@-}

convertMath_l (Token SYMBOL var)
        | (head var) == '$' = Just (Leaf var)
        | otherwise = Nothing

convertMath_l _ = Nothing

{-@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@-}

{- Tests -}

test_ParseMath_as :: IO ()
test_ParseMath_as = do
    let tokens = scan "(a * b + c)"
    case tokens of
        Just tokens' -> do
            putStrLn (show tokens')
            case convertMath_as ([],tokens') of
                Just tree -> putStrLn (show tree)
                Nothing -> putStrLn "Could not make Tree"
        Nothing -> putStrLn "Bad test string"
