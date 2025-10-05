{
module AbacusParser (parseAbacus) where

import AbacusLexer
import Data.Map (Map)
import qualified Data.Map as Map
import AbacusMachine.Types (AbacusRule(..),AbacusStateName(..),AbacusMap(..),Register(..))
}

%name parseAbacus
%tokentype { Token }
%error { parseError }

%token
    ':'         { TColon }
    increment   { TIncrement }
    decrement   { TDecrement }
    ident       { TIdent $$ }

%%

AbacusMap : Rules                   { Map.fromList $1 }

Rules : Rule                        { [$1] }
      | Rule Rules                  { $1 : $2 }

Rule : ident ':' increment ident ident
        { ($1, Increment $4 $5) }
     | ident ':' decrement ident ident ident
        { ($1, Decrement $4 $5 $6) }

{
parseError :: [Token] -> a
parseError tokens = error $ "Parse error: " ++ show tokens
}
