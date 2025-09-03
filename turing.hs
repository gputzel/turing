--Some ideas
--Optionally, view the tape as fixed so that the head moves back and forth

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Text.Parsec.Combinator

halfWidth :: Int
halfWidth = 20

data TapeSymbol = Dash | Blank
    deriving (Eq)

instance Show TapeSymbol where
    show Dash = "-"
    show Blank = "."

newtype HeadState = HeadState String
    deriving (Eq)

instance Show HeadState where
    show (HeadState str) = str

data HeadMove = JumpLeft | JumpRight | Stay
    deriving (Eq, Show)

-- A symbol is read in a given state
-- This will specify the symbol to be written, the move to be made on the tape,
-- and the next head state
type TransitionRule = (TapeSymbol, HeadState) -> (TapeSymbol, HeadMove, HeadState)

-- The tape itself is two linked lists, representing
-- everything to the left of the head
-- and everything to the right of the head
type TapeState = ([TapeSymbol],TapeSymbol,[TapeSymbol])

showTape :: TapeState -> String
showTape (leftTape,s,rightTape) = leftString ++ middleChar ++ rightString
    where
        leftString = concatMap show $ reverse $ take halfWidth leftTape
        middleChar = show s
        rightString = concatMap show $ take halfWidth rightTape

-- we will need infinite numbers of blanks on either side
allBlanks :: [TapeSymbol]
allBlanks = Blank : allBlanks

allDashes :: [TapeSymbol]
allDashes = Dash : allDashes

data MachineState = MachineState 
    { tapeState :: TapeState
    , headState :: HeadState 
    }

instance Show MachineState where
    show MachineState {tapeState=ts, headState=hs} = headRow ++ "\n" ++ tapeRow
        where
            headStateName = show hs
            headRow = spaces ++ "V[" ++ headStateName ++ "]" ++ rightSpaces
            spaces = concat $ take halfWidth $ repeat " "
            rightSpaces = concat $ take (halfWidth - 2 - (length headStateName)) $ repeat " "
            tapeRow = showTape ts

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

tapeSymbol :: Parser TapeSymbol
tapeSymbol = choice
    [ char '-' >> return Dash
    , char '.' >> return Blank
    ]

tapeStateParser :: Parser TapeState
tapeStateParser = do
    leftLine <- many tapeSymbol
    char '\n'
    currentSym <- tapeSymbol
    char '\n'
    rightLine <- many tapeSymbol
    return ((reverse leftLine) ++ allBlanks,currentSym,rightLine ++ allBlanks)

exampleRule :: TransitionRule
exampleRule (Blank,HeadState "state1") = (Blank,JumpRight,HeadState "state1")
exampleRule (Dash,HeadState "state1") = (Dash,JumpLeft,HeadState "state1")

main :: IO ()
main = do
    let tState = (allBlanks,Dash,allBlanks)
    let mState = MachineState{tapeState = tState,headState=HeadState "state1"}
    --putStrLn $ show mState
    --putStrLn $ show $ doStep exampleRule mState
    let msl = iterate (doStep exampleRule) mState
    --mapM_ putStrLn $ map show (take 5 msl)
    let input = "--.\n-\n--..000"
    case parse tapeStateParser "" input of
        Left err -> print err
        Right result -> print $ MachineState{tapeState=result,headState=HeadState "state1"}
