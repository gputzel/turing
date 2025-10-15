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

### To do

- Address all warnings during build process
- Use MegaParsec instead of Happy/Alex
- Implement tests
- Go into "standard position" at the end of an abacus computation (translated into Turing steps). That is, the Turing head should be at the beginning of the first output register, and there should be nothing else.
- Translate arbitrary recursive functions into abacus code
- Compile some high-level language into abacus code?
- Search for optimizations of Turing machines
- In the abacus to Turing reduction, make the composition process more Monad-like - possibly use a Monad?
