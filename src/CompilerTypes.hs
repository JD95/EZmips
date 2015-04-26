module CompilerTypes where

data MathTree = AddNode MathTree MathTree | SubNode MathTree MathTree |
                MulNode MathTree MathTree | DivNode MathTree MathTree | ModNode MathTree MathTree |
                Leaf String deriving (Show)

{-Utilities-}

{--- The subtrees are converted from Maybe values to values ---------------------------------}
mergeInstructions :: [Maybe [String]] -> Maybe [String]
mergeInstructions [] = Just []
mergeInstructions [value] = case value of
                           Just value' -> Just value'
                           Nothing     -> Nothing

mergeInstructions (value:rest) =
    let values = mergeInstructions rest in
    case values of
        Just values' -> case value of
                             Just value' -> Just (value'++ values')
                             Nothing  -> Nothing
        Nothing      -> Nothing