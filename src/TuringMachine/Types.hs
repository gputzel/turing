module TuringMachine.Types where

import Data.Map (Map)

data TapeSymbol = Dash | Blank
    deriving (Eq,Ord)

instance Show TapeSymbol where
    show Dash = "-"
    show Blank = "."

data HeadState = Halt | HeadState String
    deriving (Eq,Ord)

instance Show HeadState where
    show Halt = "Halt"
    show (HeadState str) = str

data HeadMove = JumpLeft | JumpRight | Stay
    deriving (Eq, Show)

-- A symbol is read in a given state
-- This will specify the symbol to be written, the move to be made on the tape,
-- and the next head state
type TransitionRule = (TapeSymbol, HeadState) -> (TapeSymbol, HeadMove, HeadState)
type TransitionMap = Map (TapeSymbol, HeadState) (TapeSymbol, HeadMove, HeadState)

mapToTransitionRule :: TransitionMap -> TransitionRule
mapToTransitionRule ruleMap = \key -> case Map.lookup key ruleMap of
    Just result -> result
    Nothing -> error $ "No transition rule found for: " ++ show key

-- The tape itself is two linked lists, representing
-- everything to the left of the head
-- and everything to the right of the head
type TapeState = ([TapeSymbol],TapeSymbol,[TapeSymbol])

-- we will need infinite numbers of blanks on either side
allBlanks :: [TapeSymbol]
allBlanks = Blank : allBlanks

allDashes :: [TapeSymbol]
allDashes = Dash : allDashes

data MachineState = MachineState 
    { tapeState :: TapeState
    , headState :: HeadState 
    }
