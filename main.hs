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
  | Plus RegEx
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
