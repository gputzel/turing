import AbacusMachine
import AbacusMachine.Types
import AbacusMachine.ParseMemory

import qualified Data.Map as Map

main :: IO ()
main = do
    let memoryString = "A:5\nB:0"
    let memory = case parseMemoryState memoryString of
                    Left err -> error $ show err
                    Right memState -> memState
    let amap = Map.fromList[("start",Decrement "A" "start" "Halt")]
    let mstate = AbacusMachineState "start" memory
    let newstate = doStep amap mstate
    putStrLn $ show mstate
    putStrLn $ show newstate
