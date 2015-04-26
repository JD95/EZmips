module Compiler where

import Data.List
import Data.Char

import Scanner
import ScannerTypes

import Parser
import ParserTypes

import CompilerTypes
import MathConversion

treeToMips:: MathTree -> Int -> Maybe (String, [String], Int)

treeToMips (AddNode left right) c = convertTree "add" left right c
treeToMips (SubNode left right) c = convertTree "sub" left right c
treeToMips (MulNode left right) c = convertTree "mul" left right c

treeToMips (DivNode left right) c = divisionOp "mflo" left right c
treeToMips (ModNode left right) c = divisionOp "mfhi" left right c

treeToMips (Leaf a) c = Just (a,[],c)

convertTree :: String -> MathTree -> MathTree -> Int -> Maybe (String, [String], Int)
convertTree op left right c = do
    (left', a, tNum) <- treeToMips left c
    (right', b, tNum') <- treeToMips right tNum
    let tReg = "$t"++(show (tNum'+1))
    let instruction = [op ++ " " ++ tReg ++", " ++ left' ++ ", " ++ right']
    Just (tReg,a ++ b ++ instruction , tNum'+ 1)

divisionOp :: String -> MathTree -> MathTree -> Int -> Maybe (String, [String], Int)
divisionOp op left right c = do
    (left', a, tNum) <- treeToMips left c
    (right', b, tNum') <- treeToMips right tNum
    let tReg = "$t"++(show (tNum'+1))
    let instruction1 = ["div" ++ " " ++ left' ++ ", " ++ right']
    let instruction2 = [op ++ " " ++ tReg]
    Just (tReg, a ++ b ++ instruction1 ++ instruction2 , tNum'+ 1)


loadFuncArgs :: [Token] -> Int -> Maybe [String]
loadFuncArgs ((Token SYMBOL arg):more) argNum = do
    let loadArg = "move $a" ++ [(intToDigit argNum)] ++ ", " ++ arg
    rest <- loadFuncArgs more (argNum + 1)
    Just (loadArg:rest)

loadFuncArgs ((Token INTEGER arg):more) argNum = do
    let loadArg = "li $a" ++ [(intToDigit argNum)] ++ ", " ++ arg
    rest <- loadFuncArgs more (argNum + 1)
    Just (loadArg:rest)
    
loadFuncArgs [] _ = Just []
loadFuncArgs _ _ = Nothing

{- Converting Statements-}
convertStatement :: Statement -> Maybe [String]

convertStatement (Assignment [(Token SYMBOL var)] [(Token INTEGER num)]) = do
    let assign = "li " ++ var ++ ", " ++ num
    Just ([assign])

convertStatement (Assignment [(Token SYMBOL var)] ((Token MATH _):math)) = do
    tree <- convertMath_as ([], math)
    (result, instructions, _) <- treeToMips tree (-1)
    let assign = "move " ++ var ++ ", " ++ result
    Just (instructions ++ [assign])

convertStatement (Assignment [(Token SYMBOL var)] ((Token FUNC _):(Token SYMBOL fname):args)) = do
    loadArgs <- loadFuncArgs args 0
    let call = "jal " ++ fname
    let assign = "move " ++ var ++ ", " ++ "$v0"
    Just (loadArgs ++ [call] ++ [assign])

convertStatement (Return (Token INTEGER num)) =
    let load = "li $t0, " ++ num
        return' = "move $v0, $t0"
    in Just [load, return']

convertStatement (Return (Token SYMBOL sym)) =
    Just [("move $v0, " ++ sym)]

convertStatement _ = Nothing

convertFunction :: Function -> Maybe [String]

convertFunction (Function "main" _ body) = do
    let endInstructions = ["li $v0, 10", "syscall"]
    instructions <- mergeMaybes (map convertStatement body)
    Just (["main" ++ ":"] ++ instructions ++ ["end_"++"main"++":"] ++ endInstructions)

convertFunction (Function name args body) = do
    instructions <- mergeMaybes (map convertStatement body)
    Just ([name ++ ":"]++pushStack ++ (moveArgs args (length args)) ++ instructions ++ ["end_"++name++":"] ++ popStack)



moveArgs :: [String] -> Int -> [String]
moveArgs [] _ = []
moveArgs args orgLen =
    let rest = moveArgs (tail args) orgLen in
        ["move " ++ (head args) ++ ", $a" ++ [intToDigit (orgLen -(length args))]] ++ rest


pushStack :: [String]
pushStack = let decStack = "addi $sp, $sp, -4"
                storeRa  = "sw $ra, 0($sp)"
                sw       = \ a -> "sw " ++ a ++ ", 0($sp)"
                saves    = ["$s" ++ (show x) | x <- [0..7]]
                saveSP   = map sw saves
             in tail (intersperse decStack ("a":storeRa:saveSP))

popStack :: [String]
popStack = let augStack  = "addi $sp, $sp, 4"
               loadRa   = "lw $ra, 0($sp)"
               lw       = \ a -> "lw " ++ a ++ ", 0($sp)"
               saves    = (map (\x-> "$s" ++ show x)  [0..7])
               loadSP   = map lw (reverse  saves)
            in (tail (intersperse augStack ("a":loadSP ++ [loadRa])) ++ ["jr $ra"])



{-Tests-}

test_treeToMips :: IO ()
test_treeToMips = do
   let input = "s0 + s1"
   putStrLn input
   case scan input of
       Just expr ->  do
           putStrLn (show expr)
           case convertMath_as ([],expr) of
             Just mathTree -> do
                 putStrLn (show mathTree)
                 case (treeToMips mathTree (-1)) of
                   Just (location, commands,_) -> do
                       mapM_ (putStrLn) commands
                       putStrLn ("Result in " ++ location)
                   Nothing -> putStrLn "Didn't work"
             Nothing -> putStrLn "Tree didn't Parse"
       Nothing -> putStrLn "Expression didn't scan"

test_convertStatement :: IO ()
test_convertStatement = do
    case scan "return a;" of
        Just tokens -> case processStatement tokens (Fdata "main" [] (0,0,0)) of
                            Just (statement,_) -> do
                                putStrLn (show statement)
                                case convertStatement statement of
                                    Just instructions -> mapM_ (putStrLn) instructions
                                    Nothing -> putStrLn "Could not generate instructions"
                            Nothing -> putStrLn "Could not process Statement"
        Nothing -> putStrLn "Bad Test string"

test_convertFunction :: IO ()
test_convertFunction = do
    let input = "main: a = 1; b = 1; c = 2; d = @ (a + b) / c; return d; end~"
    putStrLn input
    case scan input of
        Just tokens -> do
            --putStrLn (show tokens)
            case gatherFunction tokens of
                Just (function,_) -> do
                    --putStrLn (show function)
                    case convertFunction function of
                        Just instructions -> mapM_ (putStrLn) instructions
                        Nothing -> putStrLn "Could not generate instructions"
                Nothing -> putStrLn "Could not gather function"
        Nothing -> putStrLn "Bad Test string"

test_pushStack:: IO ()
test_pushStack = do
    mapM_ (putStrLn) popStack