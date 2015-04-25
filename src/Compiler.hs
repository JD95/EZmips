module Compiler where

import Scanner
import MathParsing
import ParserTypes

treeToMips:: MathTree -> Int -> Maybe (String, [String], Int)

treeToMips (AddNode left right) c = convertTree "add" left right c
treeToMips (SubNode left right) c = convertTree "sub" left right c
treeToMips (MulNode left right) c = convertTree "mul" left right c

treeToMips (DivNode left right) c = divisionOp "mflo" left right c
treeToMips (ModNode left right) c = divisionOp "mfhi" left right c

treeToMips (Leaf a) c = Just (a,[],c)

convertTree :: String -> MathTree -> MathTree -> Int -> Maybe (String, [String], Int)
convertTree op left right c = do
    (right', b, tNum) <- treeToMips right c
    (left', a, tNum') <- treeToMips left tNum
    let tReg = "$t"++(show (tNum'+1))
    let instruction = [op ++ " " ++ tReg ++", " ++ left' ++ ", " ++ right']
    Just (tReg,a ++ b ++ instruction , tNum'+ 1)

divisionOp :: String -> MathTree -> MathTree -> Int -> Maybe (String, [String], Int)
divisionOp op left right c = do
    (right', b, tNum) <- treeToMips right c
    (left', a, tNum') <- treeToMips left tNum
    let tReg = "$t"++(show (tNum'+1))
    let instruction1 = ["div" ++ " " ++ left' ++ ", " ++ right']
    let instruction2 = [op ++ " " ++ tReg]
    Just (tReg,a ++ b ++ instruction1 ++ instruction2 , tNum'+ 1)

{-Tests-}

test_treeToMips :: IO ()
test_treeToMips = do
   let input = "s0 + (s1 - s2) / s3"
   putStrLn input
   case scan input of
       Just expr ->  do
           putStrLn (show expr)
           case parseMath_as ([],expr) of
             Just mathTree -> do
                 putStrLn (show mathTree)
                 case (treeToMips mathTree (-1)) of
                   Just (location, commands,_) -> do
                       mapM_ (putStrLn) commands
                       putStrLn ("Result in " ++ location)
                   Nothing -> putStrLn "Didn't work"
             Nothing -> putStrLn "Tree didn't Parse"
       Nothing -> putStrLn "Expression didn't scan"

