{-# LANGUAGE LambdaCase #-}

import Data.Char
import System.Environment

-- Lexer tokens
data Token
  = WildcardT
  | LiteralT Char
  | LBracket
  | RBracket
  | RPar
  | LPar
  | Backslash
  | StarOp
  | PlusOp
  | PE [RegEx]
  deriving (Show)

-- Parsed Expression
data RegEx
  = Star RegEx
  | Literal Char
  | Concat RegEx RegEx
  | Wildcard
  | CaptureGroup RegEx
  deriving (Show)

type ReplaceEx = [ReplaceTok]

data ReplaceTok
  = LitChar Char
  | CaptureID Int
  deriving (Show)

lexer :: String -> [Token]
lexer "" = []
lexer ('.' : s) = WildcardT : lexer s
lexer ('*' : s) = StarOp : lexer s
lexer ('+' : s) = PlusOp : lexer s
lexer ('\\' : s) = Backslash : lexer s
lexer ('(' : s) = LPar : lexer s
lexer (')' : s) = RPar : lexer s
lexer ('[' : s) = LBracket : lexer s
lexer (']' : s) = RBracket : lexer s
lexer (c : s) = LiteralT c : lexer s

sr :: [Token] -> [Token] -> [Token]
-- Concat
sr (PE b : PE a : s) q = sr (PE (a ++ b) : s) q
-- 
sr (RPar : PE (e:es) : LPar : s) q = sr (PE [CaptureGroup $ foldl Concat e es] : s) q
-- Handle escaped characters
sr (WildcardT : Backslash : s) q = sr (PE [Literal '.'] : s) q
sr (StarOp : Backslash : s) q = sr (PE [Literal '*'] : s) q
sr (PlusOp : Backslash : s) q = sr (PE [Literal '+'] : s) q
sr (LPar : Backslash : s) q = sr (PE [Literal '('] : s) q
sr (RPar : Backslash : s) q = sr (PE [Literal ')'] : s) q
sr (LBracket : Backslash : s) q = sr (PE [Literal '['] : s) q
sr (RBracket : Backslash : s) q = sr (PE [Literal ']'] : s) q
-- Repeats
sr (StarOp : PE (x : xs) : s) q = sr (PE (Star x : xs) : s) q
sr (PlusOp : PE (x : xs) : s) q = sr (PE (Concat x (Star x) : xs) : s) q -- Use star for plus
-- Wildcard
sr (WildcardT : s) q = sr (PE [Wildcard] : s) q
-- Normal Literals
sr (LiteralT c : s) q = sr (PE [Literal c] : s) q
sr s (x : xs) = sr (x : s) xs -- Shift
sr s [] = s

parseMatch :: [Token] -> RegEx
parseMatch t = case sr [] t of
  [PE (e:es)] -> foldl Concat e es
  s -> error $ "Failed to parse: " ++ show s

concatTok :: [Token] -> String
concatTok [] = []
concatTok (LiteralT x : xs) = x : concatTok xs

parseReplace :: [Token] -> ReplaceEx
parseReplace [] = []
parseReplace (Backslash : LiteralT c : s)
  | isDigit c =
    let (num, rst) =
          span
            ( \case
                (LiteralT x) | isDigit x -> True
                _ -> False
            )
            s
     in CaptureID (read (c : concatTok num)) : parseReplace rst
parseReplace (LiteralT c : s) = LitChar c : parseReplace s
parseReplace x = error $ "failed to parseReplace: " ++ show x

replace :: RegEx -> RegEx -> String -> String
-- Preform the actual replacement.
-- This function can assume the string given is a complete match
--
-- This function will also handle caputure groups
replace = error "Not implemented yet"

match :: RegEx -> RegEx -> String -> String
-- match will iterate through the given string and find the end of the match to
-- the regular expression
--
-- Once the end of the match is found, it will call the replace function to
-- preform the replacement
--
-- If the match function doesn't find a complete match, it will return ""
match = error "Not implemented yet"

replaceLine :: RegEx -> RegEx -> String -> String
replaceLine e r s@(x : xs) = case match e r s of
  "" -> x : replaceLine e r xs -- No match. Move on
  x -> x

main :: IO ()
main = do
  args@[search, replace, filename] <- getArgs
  let search_term = parseMatch $ lexer search
  putStrLn $ "Replacing: " ++ show search_term
  let replace_term = parseMatch $ lexer replace
  putStrLn $ "With: " ++ show replace_term
  content <- readFile filename
  let text = lines content
  let replaced = map (replaceLine search_term replace_term) text
  putStrLn $ unlines replaced
