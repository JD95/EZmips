module Scanner where

import           Data.Bool
import           Data.Maybe
import           System.IO

import           ScannerTypes

scan :: String -> Maybe [Token]
scan input = do
    let results = reverse (extractTokens input [])
    if (filter (\(Token tokType _) -> tokType == ERROR) results) == []
    then Just results
    else Nothing

extractTokens :: String -> [Token] -> [Token]
extractTokens [] tokenList = tokenList
extractTokens (' ':input) tokenList = extractTokens input tokenList                             -- Pull off whitespace until next token
extractTokens ('#':input) tokenList = (Token COMMENT ("#"++input)):tokenList
extractTokens input tokenList = let (newToken, rest) = getNextChunk ([], input)  in             -- Get next chunk before whitespace
                                    case determineToken (newToken, []) of
                                        (Just token, [])    -> extractTokens rest (token:tokenList)
                                        (Just token, more)  -> extractTokens (more++rest) (token:tokenList)
                                        (Nothing, invalid)  -> extractTokens rest ((Token ERROR invalid): tokenList)

getNextChunk :: (String, String) -> (String,String)
getNextChunk ([], '"':rest) = getStringChunk (['"'],rest)
getNextChunk (newChunk, []) = (reverse newChunk, [])                                           -- End of line
getNextChunk (newChunk, ' ':rest) = (reverse newChunk, ' ':rest)                               -- Stop at whitespace
getNextChunk (newChunk, '\n':rest) = getNextChunk (newChunk, rest)                             -- Eat new lines
getNextChunk (newChunk, '\t':rest) = getNextChunk (newChunk, rest)                             -- Eat new lines
getNextChunk (newChunk, rest) = getNextChunk ((head rest):newChunk, tail rest)                 -- Pull another character over

getStringChunk :: (String, String) -> (String,String)
getStringChunk (newChunk, '"':rest) = (newChunk ++ ['"'], rest)
getStringChunk (newChunk, next:rest) = getStringChunk (newChunk ++ [next], rest)
getStringChunk (newChunk, []) = (newChunk, [])

determineToken :: (String, String) -> (Maybe Token, String)
determineToken ([], more) = (Nothing, more)                                                    -- If all potential is in more, then error

determineToken (potential, more)
    | isSymbol potential = (Just (Token SYMBOL potential), more)
    | (isNum first || (first == '-') && rest /= []) && noDecimal rest && onlyNums rest                           = (Just (Token INTEGER potential), more)
    | isString potential                                                                                    = (Just (Token STRING potential), more)
    | isChar potential                                                                                      = (Just (Token CHAR potential), more)
    | potential `elem` ["(",")","'",":",",",";","[","]","{","}"]                                            = (Just (Token PUNCTUATION potential), more)
    | potential == "="                                                                                           = (Just (Token ASSIGNMENT potential), more)
    | potential == "~"                                                                                           = (Just (Token FUNC potential), more)
    | potential == "@"                                                                                           = (Just (Token MATH potential), more)
    | otherwise = determineToken (take ((length potential)-1) potential, [(last potential)] ++ more)             -- Shift the last char of potential to more
    where first = if potential /= [] then head potential else ' '
          rest  = if potential /= [] then tail potential else []