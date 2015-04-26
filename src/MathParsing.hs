module MathParsing where

import Scanner
import ParserTypes
import ScannerTypes

import CompilerTypes

{-
    Grammar for Math Expressions
    <as>  -> <md> + <as> | <md> - <as> | <md>
    <md>  -> <p>  * <md> | <p>  / <md> | <p> % <md> | <p>
    <p>   -> ( <as )     | <Num>
    <Num> -> (Token INTEGER var)
-}

parseMath_as :: ([Token],[Token]) -> Maybe MathTree

parseMath_as ([],[]) = Nothing

parseMath_as ([],[sym]) = parseMath_l sym

-- try to evaluate as an as
parseMath_as ([],(hd:rest)) =
    case hd of
        (Token SYMBOL "+") -> Nothing -- No variable before operator
        (Token SYMBOL "-") -> Nothing -- No variable before operator
        (Token SYMBOL var) -> parseMath_as([hd], rest)
        (Token PUNCTUATION "(") -> do
                    (paren, rest') <- getParenExpr rest 0
                    parseMath_as ((hd:paren), rest')
        _                  -> Nothing -- Somehow a bad symbol got in

-- Move tokens to the left side until an <as> operator is found
parseMath_as (left, (hd:rest)) =
    case hd of
        (Token SYMBOL "+") -> do
            left' <- parseMath_md ([],left)
            right <- parseMath_as ([], rest)
            Just (AddNode left' right)
        (Token SYMBOL "-") -> do
            left' <- parseMath_md ([],left)
            right <- parseMath_as ([], rest)
            Just (SubNode left' right)
        (Token SYMBOL var) -> parseMath_as (left ++ [hd],rest)
        (Token PUNCTUATION "(") -> do
                    (paren, rest') <- getParenExpr rest 0
                    let newLeft = left ++ (hd:paren)
                    parseMath_as (newLeft, rest')
        _ -> Nothing -- A non symbol was found

-- Expression failed <as> try as an md
parseMath_as (left, []) = parseMath_md ([], left)

{-@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@-}


{- Attempt to parse Multiplcation, Division, or Mod -}
parseMath_md :: ([Token],[Token]) -> Maybe MathTree

parseMath_md ([],[]) = Nothing

parseMath_md ([],[sym]) = parseMath_l sym

parseMath_md ([],(hd:rest)) =
    case hd of
        (Token SYMBOL "*") -> Nothing -- No variable before operator
        (Token SYMBOL "/") -> Nothing -- No variable before operator
        (Token SYMBOL "%") -> Nothing -- No variable before operator
        (Token SYMBOL var) -> parseMath_md([hd], rest)
        (Token PUNCTUATION "(") -> do
                    (paren, rest') <- getParenExpr rest 0
                    parseMath_md ((hd:paren), rest')
        _                  -> Nothing -- Somehow a bad symbol got in

-- What is in the left portion must either be a parenthesis expression or a var
parseMath_md (left, (hd:rest)) =
    case hd of
        (Token SYMBOL "*") -> do
            left' <- parseMath_p left
            right <- parseMath_md ([], rest)
            Just (MulNode left' right)
        (Token SYMBOL "/") -> do
            left' <- parseMath_p left
            right <- parseMath_md ([], rest)
            Just (DivNode left' right)
        (Token SYMBOL "%") -> do
            left' <- parseMath_p left
            right <- parseMath_md ([], rest)
            Just (ModNode left' right)
        (Token PUNCTUATION "(") -> do
                    (paren, rest') <- getParenExpr rest 0
                    let newLeft = left ++ (hd:paren)
                    parseMath_md (newLeft, rest')
        (Token SYMBOL var) -> parseMath_md (left ++ [hd], rest) -- Non operator, move token to left
        _ -> Nothing -- Non symbol token made its way in

-- Doesn't work as an <md> try <p>
parseMath_md (left, []) = parseMath_p  left

{-@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@-}


{- Attempt to parse a Parenthsized expression -}
parseMath_p  :: [Token] -> Maybe MathTree

parseMath_p ((Token PUNCTUATION "("):rest) = do
                (tokens, _) <- getParenExpr rest 0
                parseMath_as ([], init tokens) -- Go back to the top with the inner expression, without the last paren

parseMath_p  [num] = parseMath_l num

{- Attempt to parse a variable -}
parseMath_l  :: Token -> Maybe MathTree
-- The only token that will pass is a symbol that is not punctuation

{-@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@-}

parseMath_l (Token SYMBOL var)
        | isAlphabet (head var) = Just (Leaf var)
        | otherwise = Nothing

parseMath_l _ = Nothing

{-@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@-}

{- Tests -}

test_ParseMath_as :: IO ()
test_ParseMath_as = do
    let tokens = scan "(a * b + c)"
    case tokens of
        Just tokens' -> do
            putStrLn (show tokens')
            case parseMath_md ([],tokens') of
                Just tree -> putStrLn (show tree)
                Nothing -> putStrLn "Could not make Tree"
        Nothing -> putStrLn "Bad test string"