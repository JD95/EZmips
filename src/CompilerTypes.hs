module CompilerTypes where

data MathTree = AddNode MathTree MathTree | SubNode MathTree MathTree |
                MulNode MathTree MathTree | DivNode MathTree MathTree | ModNode MathTree MathTree |
                Leaf String deriving (Show)

{-Utilities-}

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