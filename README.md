# turing

Turing machine simulator written in Haskell.

Try one of these

    cabal run turing -- --tapeState examples/tapestates/empty.tapestate --transitionRules examples/transitionrules/BB5.transitionrule  --initialState A

    cabal run turing -- --tapeState <(echo -e ".\n-\n3-.5-") --transitionRules examples/transitionrules/unary_multiply.transitionrule  --initialState start --displayMode slow

## Abacus machines

    cabal run abacus -- --memoryState <(echo -e "target:10") --abacusMap examples/abacusMaps/emptyRegister.txt --initialState "empty" --displayMode slow

## Compiling Abacus machines to Turing machines

    cabal run reduceAbacus -- --abacusMap examples/abacusMaps/emptyRegister.txt > test_empty.transitionrule
    cabal run turing -- --tapeState <(echo -e ".\n-\n----") --transitionRules test_empty.transitionrule --initialState SUB_empty___start --displayMode spacebar

