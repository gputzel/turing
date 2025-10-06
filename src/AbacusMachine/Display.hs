module AbacusMachine.Display where

import AbacusMachine.Types
import System.Process
import Text.Read (readMaybe)
import System.IO (hSetEcho, hSetBuffering, stdin, BufferMode(NoBuffering), hFlush, stdout)
import Data.List (intercalate)
import qualified Data.Map as Map

showMachine :: Int -> AbacusMachineState -> String
showMachine consoleWidth (AbacusMachineState stateName mstate) = "[" ++ stateName ++ "]" ++ someSpaces ++ memoryString where
    memoryString = intercalate " | " $ map formatPair $ Map.toList mstate
    formatPair (k, v) = k ++ "=" ++ show v
    someSpaces = "     "

getColumnNumber :: IO Int
getColumnNumber = do
    result <- readProcess "tput" ["cols"] ""
    return $ case readMaybe (strip result) of
        Just n -> n
        Nothing -> 80
    where
        strip = reverse . dropWhile (== '\n') . reverse

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

data DisplayMode = Silent | Verbose | Slow | Spacebar
    deriving (Show, Read, Eq)
