# turing

Turing machine simulator written in Haskell.

Try

    cabal run turing -- --tapeState examples/tapestates/empty.tapestate --transitionRules examples/transitionrules/BB5.transitionrule  --initialState A

## Known bugs

- `spacebar` print mode doesn't work - it shows the head shifted over one space with respect to its true position
