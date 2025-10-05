{
module AbacusLexer (Token(..), alexScanTokens) where
}

%wrapper "basic"

$alpha = [a-zA-Z]
$digit = [0-9]
$alphanum = [a-zA-Z0-9]

tokens :-
  $white+                       ;
  "#".*                         ;
  ":"                           { \_ -> TColon }
  "Increment"                   { \_ -> TIncrement }
  "Decrement"                   { \_ -> TDecrement }
  $alpha [$alphanum \_]*        { \s -> TIdent s }

{
data Token = TColon
           | TIncrement
           | TDecrement
           | TIdent String
           deriving (Show, Eq)
}
