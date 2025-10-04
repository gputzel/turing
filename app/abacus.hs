import AbacusMachine
import AbacusMachine.Types

import qualified Data.Map as Map

main :: IO ()
main = do
    let memory = Map.fromList [("A",10)]
    let amap = Map.fromList[("start",Decrement "A" "start" "Halt")]
    let mstate = AbacusMachineState "start" memory
    let newstate = doStep amap mstate
    putStrLn $ show mstate
    putStrLn $ show newstate
