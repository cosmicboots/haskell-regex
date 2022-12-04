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
  | PE RegEx
  deriving (Show)

-- Parsed Expression
data RegEx
  = Star RegEx
  | Literal Char
  | Concat RegEx RegEx
  | Wildcard
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
-- Handle escaped characters
sr (WildcardT : Backslash : s) q = sr (PE (Literal '.') : s) q
sr (StarOp : Backslash : s) q = sr (PE (Literal '*') : s) q
sr (PlusOp : Backslash : s) q = sr (PE (Literal '+') : s) q
sr (LPar : Backslash : s) q = sr (PE (Literal '(') : s) q
sr (RPar : Backslash : s) q = sr (PE (Literal ')') : s) q
sr (LBracket : Backslash : s) q = sr (PE (Literal '[') : s) q
sr (RBracket : Backslash : s) q = sr (PE (Literal ']') : s) q
-- Repeats
sr (StarOp : PE t : s) q = sr (PE (Star t) : s) q
sr (PlusOp : PE t : s) q = sr (PE (Concat t (Star t)) : s) q -- Use star for plus
-- Wildcard
sr (WildcardT : s) q = sr (PE Wildcard : s) q
-- Concat
sr (PE b : PE a : s) q = sr (PE (Concat a b) : s) q
-- Normal Literals
sr (LiteralT c : s) q = sr (PE (Literal c) : s) q
sr s (x : xs) = sr (x : s) xs -- Shift
sr s [] = s

parse :: [Token] -> RegEx
parse t = case sr [] t of
  [PE e] -> e
  s -> error $ "Failed to parse: " ++ show s

replace :: RegEx -> RegEx -> String -> String
-- Preform the actual replacement. 
-- This function can assume the string given is a complete match
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
replaceLine e r s@(x:xs) = case match e r s of
    "" -> x : replaceLine e r xs -- No match. Move on
    x -> x

main :: IO ()
main = do
  args@[search, replace, filename] <- getArgs
  let search_term = parse $ lexer search
  putStrLn $ "Replacing: " ++ show search_term
  let replace_term = parse $ lexer replace
  putStrLn $ "With: " ++ show replace_term
  content <- readFile filename
  let text = lines content
  let replaced = map (replaceLine search_term replace_term) text
  putStrLn $ unlines replaced
