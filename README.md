# turing

Turing machine simulator written in Haskell.

Try one of these

    cabal run turing -- --tapeState examples/tapestates/empty.tapestate --transitionRules examples/transitionrules/BB5.transitionrule  --initialState A

    cabal run turing -- --tapeState <(echo -e ".\n-\n3-.5-") --transitionRules examples/transitionrules/unary_multiply.transitionrule  --initialState start --displayMode slow
