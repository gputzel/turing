module TuringMachine where

import TuringMachine.Types
import TuringMachine.Display
import Control.Concurrent (threadDelay)

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

countStepsWithPrint :: DisplayMode -> Int -> TransitionRule -> (MachineState,Int) -> IO (MachineState,Int)
countStepsWithPrint dMode consoleWidth _ ((MachineState tState Halt),n) = do
    putStrLn $ showMachine consoleWidth (MachineState tState Halt)
    return ((MachineState tState Halt),n)
countStepsWithPrint dMode consoleWidth tRule (mState,n) = do
    case dMode of
        Silent -> return()
        Verbose -> putStrLn $ showMachine consoleWidth mState
        Slow -> do putStrLn $ showMachine consoleWidth mState; threadDelay 60000
        Spacebar -> do putStrLn $ showMachine consoleWidth mState; waitForSpacebar
    countStepsWithPrint dMode consoleWidth tRule (nextState,n+1) where
        nextState = doStep tRule mState
