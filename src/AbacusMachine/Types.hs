module AbacusMachine.Types where

import qualified Data.Map as Map
import Data.Map (Map)

type AbacusStateName = String
data AbacusRule = Halt | Increment {register :: Register, nextState :: AbacusStateName} | Decrement {register :: Register, nextState :: AbacusStateName, nextStateIfEmpty :: AbacusStateName}
    deriving (Show,Eq,Ord)
type AbacusMap = Map AbacusStateName AbacusRule

--The memory is made of registers (whose names are strings)
--The registers contain integers
type Register = String
type MemoryState = Map Register Int

data AbacusMachineState = AbacusMachineState {state :: AbacusStateName, memoryState :: MemoryState}
    deriving (Show)
