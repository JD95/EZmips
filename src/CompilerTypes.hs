module CompilerTypes where

data MathTree = AddNode MathTree MathTree | SubNode MathTree MathTree |
                MulNode MathTree MathTree | DivNode MathTree MathTree | ModNode MathTree MathTree |
                Leaf String deriving (Show)

{-Utilities-}

