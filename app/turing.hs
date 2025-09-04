--Some ideas
--Optionally, view the tape as fixed so that the head moves back and forth

import Text.Parsec hiding ((<|>))
import Text.Parsec.String (Parser)
--import Text.Parsec.Char
--import Text.Parsec.Combinator
import qualified Options.Applicative as Opt
import Options.Applicative ((<**>))
import System.IO.Error (tryIOError)
import System.Exit (exitFailure)

--import System.Environment (lookupEnv)
import System.Process
import Text.Read (readMaybe)

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

showTape :: Int -> TapeState -> String
showTape halfWidth (leftTape,s,rightTape) = leftString ++ middleChar ++ rightString
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

--instance Show MachineState where
--    show MachineState {tapeState=ts, headState=hs} = headRow ++ "\n" ++ tapeRow
--        where
--            headStateName = show hs
--            headRow = spaces ++ "V[" ++ headStateName ++ "]" ++ rightSpaces
--            spaces = concat $ take halfWidth $ repeat " "
--            rightSpaces = concat $ take (halfWidth - 2 - (length headStateName)) $ repeat " "
--            tapeRow = showTape ts

showMachine :: Int -> MachineState -> String
showMachine consoleWidth MachineState {tapeState=ts, headState=hs} = headRow ++ "\n" ++ tapeRow
    where
        headStateName = show hs
        halfWidth = (consoleWidth - (if odd consoleWidth then 1 else 2)) `div` 2
        spaces = concat $ take halfWidth $ repeat " "
        rightSpaces = concat $ take (halfWidth - 2 - (length headStateName)) $ repeat " "
        headRow = spaces ++ "V[" ++ headStateName ++ "]" ++ rightSpaces
        tapeRow = showTape halfWidth ts

getColumnNumber :: IO Int
getColumnNumber = do
    result <- readProcess "tput" ["cols"] ""
    return $ case readMaybe (strip result) of
        Just n -> n
        Nothing -> 80
    where
        strip = reverse . dropWhile (== '\n') . reverse

displayMachine :: MachineState -> IO ()
displayMachine ms = do
    size <- getColumnNumber
    putStrLn $ showMachine size ms

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

data Options = Options
    { optTapeStateFile :: String
    } deriving (Show)

options :: Opt.Parser Options
options = Options
    <$> Opt.strOption
        ( Opt.long "tapeState"
       <> Opt.metavar "FILENAME"
       <> Opt.help "File containing the tape state" )

opts :: Opt.ParserInfo Options
opts = Opt.info (options <**> Opt.helper)
    ( Opt.fullDesc
   <> Opt.progDesc "Parse a Turing machine tape state"
   <> Opt.header "tape-parser - a Turing machine tape state parser" )

main :: IO ()
main = do
    Options { optTapeStateFile = tapeStateFile } <- Opt.execParser opts
    
    result <- tryIOError $ readFile tapeStateFile
    case result of
        Left ioErr -> do
            putStrLn $ "Error reading file: " ++ show ioErr
            exitFailure
        Right content -> 
            case parse tapeStateParser tapeStateFile content of
                Left parseErr -> do
                    putStrLn $ "Parse error: " ++ show parseErr
                    exitFailure
                Right tapeStateResult -> mapM_ displayMachine $ (take 5 msl) where
                    msl = iterate (doStep exampleRule) mState
                    mState = MachineState{tapeState = tapeStateResult,headState=HeadState "state1"}
