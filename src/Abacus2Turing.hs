module Abacus2Turing where

import TuringMachine.Types
import qualified Data.Map as Map
import Data.Map (Map)

--Note that we need to do prepending in both the keys and values
prependToStateNames :: String -> TransitionMap -> TransitionMap
prependToStateNames prefix = (Map.map value_prep) . (Map.mapKeys key_prep) where
    value_prep (tSymbol,hMove,hstate) = (tSymbol,hMove, state_prepper hstate) 
    key_prep (tSymbol,hstate) = (tSymbol, state_prepper hstate)
    state_prepper Halt = Halt
    state_prepper (HeadState s) = HeadState $ prefix ++ s

stitch :: TransitionMap -> TransitionMap -> HeadState -> TransitionMap
stitch map1 map2 hstate =
    Map.union map1renamed map2 where
        map1renamed = Map.map transform map1
        transform (tapeSymbol,headMove,Halt) = (tapeSymbol,headMove,hstate)
        transform (tapeSymbol,headMove,headState) = (tapeSymbol,headMove,headState)

--The "incrementer" that increments register number s has an initial stage
--consisting of s - 1 of these initial blocks
incrementer_initial_block :: TransitionMap
incrementer_initial_block = Map.fromList [
                                    ((Dash,HeadState "a"),(Dash,JumpRight,HeadState "b"))
                                    ,((Blank,HeadState "a"),(Dash,Stay,HeadState "a"))
                                    ,((Dash,HeadState "b"),(Dash,JumpRight,HeadState "b"))
                                    ,((Blank,HeadState "b"),(Blank,JumpRight,Halt))
                            ]

incrementer_transitional_block :: TransitionMap
incrementer_transitional_block = Map.fromList [
                                    ((Dash,HeadState "a"),(Dash,JumpRight,HeadState "b"))
                                    ,((Blank,HeadState "a"),(Dash,Stay,HeadState "a"))
                                    ,((Dash,HeadState "b"),(Dash,JumpRight,HeadState "b"))
                                    ,((Blank,HeadState "b"),(Dash,Stay,Halt))
                                ]


