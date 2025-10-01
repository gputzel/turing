module TuringMachine where

import TuringMachine.Types

readSymbol :: TapeState -> TapeSymbol
readSymbol (leftTape,s,rightTape) = s

writeSymbol :: TapeSymbol -> TapeState -> TapeState
writeSymbol s (leftTape,_,rightTape) = (leftTape,s,rightTape)

applyMove :: HeadMove -> TapeState -> TapeState
applyMove Stay tState = tState
applyMove JumpLeft (leftSymbol:leftTape,s,rightTape) = (leftTape,leftSymbol,s:rightTape) 
applyMove JumpRight (leftTape,s,rightSymbol:rightTape) = (s:leftTape,rightSymbol,rightTape)

doStep :: TransitionRule -> MachineState -> MachineState
doStep transRule (MachineState tapeState headState) =
    let
        currentSymbol = readSymbol tapeState
        (newSymbol, headMove, newHeadState) = transRule (currentSymbol,headState)
        newTapeState = applyMove headMove $ writeSymbol newSymbol tapeState
    in MachineState newTapeState newHeadState
