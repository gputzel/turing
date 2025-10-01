import TuringMachine
import TuringMachine.Types
import TuringMachine.Display
import TuringMachine.Parse

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
