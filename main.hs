{-# LANGUAGE LambdaCase #-}

import Data.Char
import Debug.Trace
import System.Environment

-- Lexer tokens
data Token
  = WildcardT
  | LiteralT Char
  | LBracket
  | RBracket
  | LBra
  | RBra
  | RPar
  | LPar
  | Backslash
  | StarOp
  | PlusOp
  | OrOp
  | IdTok Int
  | PE [RegEx]
  deriving (Show)

-- Parsed Expression
data RegEx
  = Star RegEx
  | Literal Char
  | Concat RegEx RegEx
  | Or RegEx RegEx
  | Wildcard
  | CaptureGroup RegEx
  | CapIdRE Int
  | Range Char Char
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
lexer ('\\' : c : s)
  | isDigit c =
    let (x, rst) = span isDigit s
        num = read (c : x) - 1
     in if num >= 0
          then IdTok (read (c : x) - 1) : lexer rst
          else lexer rst
lexer ('\\' : s) = Backslash : lexer s
lexer ('(' : s) = LPar : lexer s
lexer (')' : s) = RPar : lexer s
lexer ('[' : s) = LBracket : lexer s
lexer (']' : s) = RBracket : lexer s
lexer ('{' : s) = LBra : lexer s
lexer ('}' : s) = RBra : lexer s
lexer ('|' : s) = OrOp : lexer s
lexer (c : s) = LiteralT c : lexer s

sr :: [Token] -> [Token] -> [Token]
sr (IdTok x : s) q = sr (PE [CapIdRE x] : s) q
-- Concat
sr (PE b : PE a : s) q = sr (PE (a ++ b) : s) q
-- Or
sr (PE (b : bs) : OrOp : PE (a : as) : s) q = sr (PE [Or (foldl Concat a as) (foldl Concat b bs)] : s) q
-- Capture groups
sr (RPar : PE (e : es) : LPar : s) q = sr (PE [CaptureGroup $ foldl Concat e es] : s) q
sr (RBra : PE (e : es) : LBra : s) q = sr (PE [foldl Concat e es] : s) q
-- Handle escaped characters
sr (WildcardT : Backslash : s) q = sr (PE [Literal '.'] : s) q
sr (StarOp : Backslash : s) q = sr (PE [Literal '*'] : s) q
sr (PlusOp : Backslash : s) q = sr (PE [Literal '+'] : s) q
sr (LPar : Backslash : s) q = sr (PE [Literal '('] : s) q
sr (RPar : Backslash : s) q = sr (PE [Literal ')'] : s) q
sr (LBracket : Backslash : s) q = sr (PE [Literal '['] : s) q
sr (RBracket : Backslash : s) q = sr (PE [Literal ']'] : s) q
sr (LBra : Backslash : s) q = sr (PE [Literal '{'] : s) q
sr (RBra : Backslash : s) q = sr (PE [Literal '}'] : s) q
sr (OrOp : Backslash : s) q = sr (PE [Literal '|'] : s) q
-- Repeats
sr (StarOp : PE xs : s) q = let x = last xs in sr (PE (init xs ++ [Star x]) : s) q
sr (PlusOp : PE (x : xs) : s) q = sr (PE (Concat x (Star x) : xs) : s) q -- Use star for plus
-- Ranges
sr (RBracket : PE [Literal l, Literal '-', Literal r] : LBracket : s) q = sr (PE [Range l r] : s) q
-- Wildcard
sr (WildcardT : s) q = sr (PE [Wildcard] : s) q
-- Normal Literals
sr (LiteralT c : s) q = sr (PE [Literal c] : s) q
sr s (x : xs) = sr (x : s) xs -- Shift
sr s [] = s

parseMatch :: [Token] -> RegEx
parseMatch t = case sr [] t of
  [PE (e : es)] -> foldl Concat e es
  s -> error $ "Failed to parse: " ++ show s

concatTok :: [Token] -> String
concatTok [] = []
concatTok (LiteralT x : xs) = x : concatTok xs

lexReplace :: String -> [Token]
lexReplace [] = []
lexReplace ('\\' : s) = Backslash : lexReplace s
lexReplace (c : s) = LiteralT c : lexReplace s

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

replace :: [String] -> ReplaceEx -> String
replace env [] = []
replace env ((LitChar c) : s) = c : replace env s
replace env ((CaptureID x) : s) = (env !! x) ++ replace env s

replaceRE :: [String] -> RegEx -> RegEx
replaceRE env l@(Literal _) = l
replaceRE env Wildcard = Wildcard
replaceRE env (Star x) = Star (replaceRE env x)
replaceRE env (Concat x y) = Concat (replaceRE env x) (replaceRE env y)
replaceRE env (Or x y) = Or (replaceRE env x) (replaceRE env y)
replaceRE env (CaptureGroup x) = CaptureGroup (replaceRE env x)
replaceRE env (Range x y) = Range x y
replaceRE env (CapIdRE i) =
  if i < length env
    then parseMatch (lexer $ env !! i)
    else CapIdRE i

match :: RegEx -> ReplaceEx -> String -> Maybe ([String], String, String)
match (Literal x) r (c : cs) | x == c = Just ([], [c], cs)
match (Literal x) r (c : cs) | x /= c = Nothing
match Wildcard r (c : cs) = Just ([], [c], cs)
match (Range x y) r (c : cs) | c >= x && c <= y = Just ([], [c], cs)
match (Range x y) r (c : cs) = Nothing
match (Star x) r s = case match x r s of
  Nothing -> Just ([], [], s)
  Just l@(env, mat, rst) -> case match (Star x) r rst of
    Nothing -> Just l
    Just (env, k, l) -> Just (env, mat ++ k, l)
match (Concat x y) r s =
  case match x r s of
    Nothing -> Nothing
    Just (env1, r1, rst) -> case match (replaceRE env1 y) r rst of
      Nothing -> Nothing
      Just (env2, r2, rst2) -> Just (env1 ++ env2, r1 ++ r2, rst2)
match (Or x y) r s =
  case match x r s of
    r1@(Just _) -> r1
    Nothing -> case match y r s of
      r2@(Just _) -> r2
      Nothing -> Nothing
match (CaptureGroup x) r s = case match x r s of
  Nothing -> Nothing
  Just (env, mat, rst) -> Just (env ++ [mat], mat, rst)
match e r [] = Nothing
match x y z = error $ "Failed to match: " ++ show z

replaceLine :: RegEx -> ReplaceEx -> String -> String
replaceLine e r [] = []
replaceLine e r s@(x : xs) = case match e r s of
  Nothing -> x : replaceLine e r xs -- No match. Move on
  Just (env, [], rst) -> x : replaceLine e r xs
  Just (env, x1, []) -> replace (x1 : env) r
  Just (env, x1, rst) -> replace (x1 : env) r ++ replaceLine e r rst

main :: IO ()
main = do
  args@[search, replace, filename] <- getArgs
  let search_term = parseMatch $ lexer search
  --putStrLn $ "Replacing: " ++ show search_term
  let replace_term = parseReplace $ lexReplace replace
  --putStrLn $ "With: " ++ show replace_term
  content <- readFile filename
  let text = lines content
  let replaced = map (replaceLine search_term replace_term) text
  putStr $ unlines replaced
