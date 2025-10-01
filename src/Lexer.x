{
module Lexer (Token(..), AlexPosn(..), alexScanTokens) where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$alphanum = [a-zA-Z0-9_]

tokens :-

  $white+                       ; -- ignore whitespace
  "#".*                         ; -- ignore comments (from # to end of line)
  
  \.                            { \p s -> TokenDot p }
  \-                            { \p s -> TokenDash p }
  "->"                          { \p s -> TokenArrow p }
  
  "Stay"                        { \p s -> TokenStay p }
  "JumpRight"                   { \p s -> TokenJumpRight p }
  "JumpLeft"                    { \p s -> TokenJumpLeft p }
  "Halt"                        { \p s -> TokenHalt p }
  
  $alpha $alphanum*             { \p s -> TokenIdent p s }

{
data Token
  = TokenDot AlexPosn
  | TokenDash AlexPosn
  | TokenArrow AlexPosn
  | TokenStay AlexPosn
  | TokenJumpRight AlexPosn
  | TokenJumpLeft AlexPosn
  | TokenHalt AlexPosn
  | TokenIdent AlexPosn String
  deriving (Eq, Show)
}
