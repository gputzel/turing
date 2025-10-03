module TuringMachine.Display where

import System.Process
import TuringMachine.Types
import Text.Read (readMaybe)
import System.IO (hSetEcho, hSetBuffering, stdin, BufferMode(NoBuffering), hFlush, stdout)

showTape :: Int -> TapeState -> String
showTape halfWidth (leftTape,s,rightTape) = leftString ++ middleChar ++ rightString
    where
        leftString = concatMap show $ reverse $ take halfWidth leftTape
        middleChar = show s
        rightString = concatMap show $ take halfWidth rightTape

showMachine :: Int -> MachineState -> String
showMachine consoleWidth MachineState {tapeState=ts, headState=hs} = headRow ++ "\n" ++ tapeRow
    where
        headStateName = show hs
        halfWidth = (consoleWidth - (if odd consoleWidth then 1 else 2)) `div` 2
        spaces = concat $ take halfWidth $ repeat " "
        rightSpaces = concat $ take (halfWidth - 2 - (length headStateName)) $ repeat " "
        headRow = spaces ++ "↓[" ++ headStateName ++ "]" ++ rightSpaces
        tapeRow = showTape halfWidth ts

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
