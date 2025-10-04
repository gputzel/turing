module AbacusMachine where

import AbacusMachine.Types
import qualified Data.Map as Map

doStep :: AbacusMap -> AbacusMachineState -> AbacusMachineState
doStep amap (AbacusMachineState stateName mstate) =
    let
        rule = case Map.lookup stateName amap of
            Just result -> result
            Nothing -> error $ "No state named: " ++ show stateName
    in case rule of
        Halt -> AbacusMachineState "Halt" mstate
        Increment register nextState -> AbacusMachineState nextState newMemoryState where
            newMemoryState = Map.adjust (+1) register mstate
        Decrement register nextState nextStateIfEmpty -> case Map.lookup register mstate of
            Just 0 -> AbacusMachineState nextStateIfEmpty mstate
            Just n -> AbacusMachineState nextState newMemoryState where
                newMemoryState = Map.adjust (subtract 1) register mstate
            Nothing -> error $ "There is no register called " ++ register
