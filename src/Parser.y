{
module Parser (parseTransitions) where

import Lexer
import TuringMachine.Types (TapeSymbol(..), HeadState(..), HeadMove(..), TransitionMap)
import qualified Data.Map as Map
}

%name parseTransitions
%tokentype { Token }
%error { parseError }

%token
  '.'         { TokenDot _ }
  '-'         { TokenDash _ }
  '->'        { TokenArrow _ }
  stay        { TokenStay _ }
  jumpleft    { TokenJumpLeft _ }
  jumpright   { TokenJumpRight _ }
  halt        { TokenHalt _ }
  ident       { TokenIdent _ $$ }

%%

Transitions :: { TransitionMap }
Transitions : TransitionList { Map.fromList $1 }

TransitionList :: { [((TapeSymbol, HeadState), (TapeSymbol, HeadMove, HeadState))] }
TransitionList : Transition                { [$1] }
           | Transition TransitionList { $1 : $2 }

Transition :: { ((TapeSymbol, HeadState), (TapeSymbol, HeadMove, HeadState)) }
Transition : TapeSymbol State '->' TapeSymbol Move State 
             { (($1, $2), ($4, $5, $6)) }

TapeSymbol :: { TapeSymbol }
TapeSymbol : '.' { Blank }
           | '-' { Dash }

State :: { HeadState }
State : halt  { Halt }
      | ident { HeadState $1 }

Move :: { HeadMove }
Move : stay      { Stay }
     | jumpleft  { JumpLeft }
     | jumpright { JumpRight }

{
parseError :: [Token] -> a
parseError toks = error $ "Parse error at: " ++ show toks
}
