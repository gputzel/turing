import AbacusMachine
import AbacusMachine.Types
import AbacusMachine.ParseMemory
import AbacusMachine.Display

import AbacusLexer
import AbacusParser

import qualified Data.Map as Map
import Text.Parsec

import qualified Options.Applicative as Opt
import Options.Applicative ((<**>))
import System.IO.Error (tryIOError)
import System.Exit (exitFailure)

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
    { optMemoryFile :: String
     ,optAbacusMapFile :: String
     ,optInitialState :: String
     ,optDisplayMode :: DisplayMode
    } deriving (Show)

options :: Opt.Parser Options
options = Options
    <$> Opt.strOption
        ( Opt.long "memoryState"
       <> Opt.metavar "FILENAME"
       <> Opt.help "File containing the memory state" )
    <*> Opt.strOption
        ( Opt.long "abacusMap"
        <> Opt.metavar "FILENAME"
        <> Opt.help "File containing the abacus rules map")
    <*> Opt.strOption
        ( Opt.long "initialState"
        <> Opt.metavar "STATE"
        <> Opt.help "Initial state of abacus machine")
    <*> Opt.option displayModeReader
        ( Opt.long "displayMode"
        <> Opt.metavar "MODE"
        <> Opt.help "Display mode (silent, verbose, slow, spacebar)"
        <> Opt.value Silent  -- Default value
        <> Opt.showDefault )

opts :: Opt.ParserInfo Options
opts = Opt.info (options <**> Opt.helper)
    ( Opt.fullDesc
   <> Opt.progDesc "Simulate an abacus machine"
   <> Opt.header "abacus - abacus machine simulator" )

main :: IO ()
main = do
    Options { optMemoryFile = memoryFile, optAbacusMapFile = abacusMapFile, optInitialState = initialState, optDisplayMode = displayMode } <- Opt.execParser opts    

    --Find out column numbers 
    nCols <- getColumnNumber

    abacusInput <- readFile abacusMapFile
    let tokens = alexScanTokens abacusInput
    let amap = parseAbacus tokens
    

    --read memory state
    memoryFileResults <- tryIOError $ readFile memoryFile    
    memory <- case memoryFileResults of
        Left ioErr -> do
            putStrLn $ "Error reading file: " ++ show ioErr
            exitFailure
        Right memoryStateContents ->
            case parse memoryStateParser memoryFile memoryStateContents of
                Left parseErr -> do
                    putStrLn $ "Parse error: " ++ show parseErr
                    exitFailure
                Right memoryStateResult -> return memoryStateResult

    let mstate = AbacusMachineState initialState memory
    (finalState,nSteps) <- countStepsWithPrint displayMode nCols amap (mstate,0)
    putStrLn $ "Halted in " ++ (show nSteps) ++ " steps."
