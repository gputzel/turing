--Some ideas
--Optionally, view the tape as fixed so that the head moves back and forth

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Char (toLower)

import Text.Parsec
import Text.Parsec.String (Parser)
--import Control.Applicative ((<|>))
--import Text.Parsec.Char
--import Text.Parsec.Combinator
import qualified Options.Applicative as Opt
import Options.Applicative ((<**>))
import System.IO.Error (tryIOError)
import System.Exit (exitFailure)

--import System.Environment (lookupEnv)
import System.Process
import Text.Read (readMaybe)

import Control.Concurrent (threadDelay)

import System.IO (hSetEcho, hSetBuffering, stdin, BufferMode(NoBuffering), hFlush, stdout)
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)

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

waitForSpacebar :: IO ()
waitForSpacebar = do
    hSetBuffering stdin NoBuffering  -- Disable input buffering
    hSetEcho stdin False
    --hFlush stdout
    waitForSpace
    hSetEcho stdin True
  where
    waitForSpace = do
        c <- getChar
        if c == ' '
            then return ()
            else waitForSpace

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

tapeSymbol :: Parser TapeSymbol
tapeSymbol = choice
    [ char '-' >> return Dash
    , char '.' >> return Blank
    ]

runCount :: Parser Int
runCount = do
    digits <- many1 digit
    return $ read digits
    <|> return 1

tapeSymbolRun :: Parser [TapeSymbol]
tapeSymbolRun = do
    n <- runCount
    theTapeSymbol <- tapeSymbol
    return $ replicate n theTapeSymbol

tapeSymbolSequence :: Parser [TapeSymbol]
tapeSymbolSequence = do
    theSymbols <- many tapeSymbolRun
    return $ concat theSymbols

tapeStateParser :: Parser TapeState
tapeStateParser = do
    leftLine <- tapeSymbolSequence
    char '\n'
    currentSym <- tapeSymbol
    char '\n'
    rightLine <- tapeSymbolSequence
    return ((reverse leftLine) ++ allBlanks,currentSym,rightLine ++ allBlanks)

headStateParser :: Parser HeadState
headStateParser = do
    str <- many1 (alphaNum <|> char '_')
    return $ if map toLower str == "halt" then Halt else HeadState str

headMoveParser :: Parser HeadMove
headMoveParser = choice
    [ try (string "JumpRight") >> return JumpRight
    , try (string "JumpLeft") >> return JumpLeft  
    , try (string "Stay") >> return Stay
    ]

transitionRuleParser :: Parser ((TapeSymbol, HeadState), (TapeSymbol, HeadMove, HeadState))
transitionRuleParser = do
    inputSymbol <- tapeSymbol
    spaces
    inputState <- headStateParser
    spaces
    string "->"
    spaces
    outputSymbol <- tapeSymbol
    spaces
    outputMove <- headMoveParser
    spaces
    outputState <- headStateParser
    return ((inputSymbol,inputState),(outputSymbol,outputMove,outputState))

transitionMapParser :: Parser TransitionMap
transitionMapParser = do
        spaces
        rules <- sepEndBy transitionRuleParser separator--(char '\n' >> spaces)
        spaces
        return $ Map.fromList rules
    where
        separator = do
            char '\n'
            spaces
            skipMany (char '\n' >> spaces)

parseTransitionRule :: String -> Either ParseError ((TapeSymbol, HeadState), (TapeSymbol, HeadMove, HeadState))
parseTransitionRule = parse transitionRuleParser ""

parseTransitionRules :: String -> String -> Either ParseError TransitionMap
parseTransitionRules filename content = parse transitionMapParser filename content

mapToTransitionRule :: TransitionMap -> TransitionRule
mapToTransitionRule ruleMap = \key -> case Map.lookup key ruleMap of
    Just result -> result
    Nothing -> error $ "No transition rule found for: " ++ show key

data DisplayMode = Silent | Verbose | Slow | Spacebar
    deriving (Show, Read, Eq)

displayModeReader :: Opt.ReadM DisplayMode
displayModeReader = Opt.eitherReader $ \s ->
    case s of
        "silent"   -> Right Silent
        "verbose"  -> Right Verbose
        "slow"     -> Right Slow
        "spacebar" -> Right Spacebar
        _          -> Left $ "Invalid display mode: " ++ s ++ 
                            ". Valid options are: silent, verbose, slow, spacebar"

data Options = Options
    { optTapeStateFile :: String
     ,optTransRuleFile :: String
     ,optInitialHeadState :: String
     ,optDisplayMode :: DisplayMode
    } deriving (Show)

options :: Opt.Parser Options
options = Options
    <$> Opt.strOption
        ( Opt.long "tapeState"
       <> Opt.metavar "FILENAME"
       <> Opt.help "File containing the tape state" )
    <*> Opt.strOption
        ( Opt.long "transitionRules"
        <> Opt.metavar "FILENAME"
        <> Opt.help "File containing the transition rules")
    <*> Opt.strOption
        ( Opt.long "initialState"
        <> Opt.metavar "STATE"
        <> Opt.help "Initial state of Turing machine head")
    <*> Opt.option displayModeReader
        ( Opt.long "displayMode"
        <> Opt.metavar "MODE"
        <> Opt.help "Display mode (silent, verbose, slow, spacebar)"
        <> Opt.value Silent  -- Default value
        <> Opt.showDefault )

opts :: Opt.ParserInfo Options
opts = Opt.info (options <**> Opt.helper)
    ( Opt.fullDesc
   <> Opt.progDesc "Simulate a Turing machine"
   <> Opt.header "turing - Turing machine simulator" )

main :: IO ()
main = do
    Options { optTapeStateFile = tapeStateFile, optTransRuleFile = transRuleFile, optInitialHeadState = initialState, optDisplayMode = displayMode } <- Opt.execParser opts
  
    --Find out column numbers 
    nCols <- getColumnNumber

    --Read in transition rule
    transRuleResult <- tryIOError $ readFile transRuleFile
    transFunc <- case transRuleResult of
        Left ioErr -> do
            putStrLn $ "Error reading transition rules file: " ++ show ioErr
            exitFailure
        Right transRuleContent ->
            case parseTransitionRules transRuleFile transRuleContent of
                Left err -> do
                    putStrLn $ "Parse error in transition rules: " ++ show err
                    exitFailure
                Right ruleMap -> return $ mapToTransitionRule ruleMap

    --Read in tape state
    result <- tryIOError $ readFile tapeStateFile
    tapeStateResult <- case result of
        Left ioErr -> do
            putStrLn $ "Error reading file: " ++ show ioErr
            exitFailure
        Right tapeStateContent -> 
            case parse tapeStateParser tapeStateFile tapeStateContent of
                Left parseErr -> do
                    putStrLn $ "Parse error: " ++ show parseErr
                    exitFailure
                Right tapeStateResult -> return tapeStateResult
    --Run it
    let mState = MachineState{tapeState = tapeStateResult,headState=HeadState initialState}
    (finalState,nSteps) <- countStepsWithPrint displayMode nCols transFunc (mState, 0)
    putStrLn $ "Halted in " ++ (show nSteps) ++ " steps."
