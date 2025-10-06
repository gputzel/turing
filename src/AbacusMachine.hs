module AbacusMachine where

import AbacusMachine.Types
import AbacusMachine.Display
import Control.Concurrent (threadDelay)
import qualified Data.Map as Map

doStep :: AbacusMap -> AbacusMachineState -> AbacusMachineState
doStep amap (AbacusMachineState stateName mstate) =
    let
        rule = case Map.lookup stateName amap of
            Just result -> result
            Nothing -> error $ "No state named: " ++ show stateName
    in case rule of
        Increment register nextState -> AbacusMachineState nextState newMemoryState where
            newMemoryState = Map.adjust (+1) register mstate
        Decrement register nextState nextStateIfEmpty -> case Map.lookup register mstate of
            Just 0 -> AbacusMachineState nextStateIfEmpty mstate
            Just n -> AbacusMachineState nextState newMemoryState where
                newMemoryState = Map.adjust (subtract 1) register mstate
            Nothing -> error $ "There is no register called " ++ register

countStepsWithPrint :: DisplayMode -> Int -> AbacusMap -> (AbacusMachineState,Int) -> IO (AbacusMachineState,Int)
countStepsWithPrint dMode consoleWidth _ ((AbacusMachineState "Halt" memory),n) = do
    putStrLn $ showMachine consoleWidth (AbacusMachineState "Halt" memory)
    return ((AbacusMachineState "Halt" memory),n)
countStepsWithPrint dMode consoleWidth amap (mState,n) = do
    case dMode of
        Silent -> return()
        Verbose -> putStrLn $ showMachine consoleWidth mState
        Slow -> do putStrLn $ showMachine consoleWidth mState; threadDelay 60000
        Spacebar -> do putStrLn $ showMachine consoleWidth mState; waitForSpacebar
    countStepsWithPrint dMode consoleWidth amap (nextState,n+1) where
        nextState = doStep amap mState
