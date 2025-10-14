module Abacus2Turing where

import AbacusMachine.Types
import TuringMachine.Types
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (sort)

import qualified Data.Set as Set

printMap :: TransitionMap -> IO ()
printMap tMap = printList (Map.toList tMap) where
    printList [] = return () 
    printList (p:l) =
        do
            let ((tSym1,hState1),(tSym2,hMove,hState2)) = p
            let lhs = (show tSym1) ++ " " ++ (show hState1)
            let rhs = (show tSym2) ++ " " ++ (show hMove) ++ " " ++ (show hState2)
            putStrLn $ lhs ++ " -> " ++ rhs
            printList l

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

incrementer_end :: TransitionMap
incrementer_end = Map.fromList [
        ((Dash,HeadState "lower_left_of_square"),(Dash,JumpRight,HeadState "lower_right_of_square"))
        ,((Blank,HeadState "lower_left_of_square"),(Blank,Stay,HeadState "error"))
        ,((Dash,HeadState "lower_right_of_square"),(Blank,Stay,HeadState "upper_right_of_square"))
        ,((Blank,HeadState "lower_right_of_square"),(Blank,JumpLeft,HeadState "left_loop"))
        ,((Dash,HeadState "upper_right_of_square"),(Dash,Stay,HeadState "error"))
        ,((Blank,HeadState "upper_right_of_square"),(Blank,JumpRight,HeadState "upper_left_of_square"))
        ,((Dash,HeadState "upper_left_of_square"),(Dash,JumpRight,HeadState "upper_left_of_square"))
        ,((Blank,HeadState "upper_left_of_square"),(Dash,Stay,HeadState "lower_left_of_square"))
        ,((Dash,HeadState "left_loop"),(Dash,JumpLeft,HeadState "left_loop"))
        ,((Blank,HeadState "left_loop"),(Blank,JumpLeft,HeadState "second_to_last"))
        ,((Dash,HeadState "second_to_last"),(Dash,JumpLeft,HeadState "left_loop"))
        ,((Blank,HeadState "second_to_last"),(Blank,JumpRight,HeadState "last"))
        ,((Dash,HeadState "last"),(Dash,Stay,HeadState "error"))
        ,((Blank,HeadState "last"),(Blank,JumpRight,HeadState "incrementer_end"))
    ]

--This is a helper function for stitching together lists
--of transition maps. For each one, we need to know what prefix
--to prepend to the state names, and also the HeadState that we
--need to wire Halt into
stitchList :: [(TransitionMap,String,HeadState)] -> TransitionMap
stitchList [] = Map.fromList []
stitchList ((tMap,prefix,hState):l) = stitch leftMap rightMap hState
    where
        leftMap = prependToStateNames prefix tMap
        rightMap = stitchList l 

entry_point :: TransitionMap
entry_point = Map.fromList [
        ((Dash,HeadState "start"),(Dash,Stay,HeadState "s1a"))
        ,((Blank,HeadState "start"),(Blank,Stay,HeadState "s1a"))
    ]

make_incrementer :: Int -> TransitionMap
make_incrementer n = Map.union entry_point the_rest
    where
        the_rest = stitchList $ zip3 tMaps prefixes hStates
        tMaps = initialBlocks ++ [incrementer_transitional_block,incrementer_end]
        initialBlocks = take (n-1) $ repeat incrementer_initial_block
        prefixes = initialPrefixes ++ ["s" ++ (show n),""]
        initialPrefixes = take (n-1) (zipWith (++) s int_strs_start_1)
        int_strs_start_1 = map show [1..]
        hStates = initialStates ++ [HeadState "lower_left_of_square",Halt]
        initialStates = take (n-1) $ zipWith zipper s [2..]
        zipper str i = HeadState (str ++ (show i) ++ "a")
        s = repeat "s"

rewire :: TransitionMap -> (TapeSymbol,HeadState) -> (TapeSymbol, HeadMove, HeadState) -> TransitionMap
rewire tMap key value = Map.insert key value tMap

decrementer_begin :: Int -> TransitionMap
decrementer_begin n = Map.union entry_point the_rest
    where
        the_rest = rewire initial_segment key value
        key = (Dash,rewireState)
        value = (Dash,JumpRight,HeadState "check_zero")
        rewireState = HeadState rewireStateName
        rewireStateName = "s" ++ (show n) ++ "a"
        initial_segment = stitchList $ zip3 tMaps prefixes hStates
        tMaps = initialBlocks ++ [incrementer_transitional_block]
        prefixes = initialPrefixes ++ ["s" ++ (show n)]
        initialBlocks = take (n-1) $ repeat incrementer_initial_block
        initialPrefixes = take (n-1) (zipWith (++) s int_strs_start_1)
        int_strs_start_1 = map show [1..]
        hStates = initialStates ++ [Halt]
        initialStates = take (n-1) $ zipWith zipper s [2..]
        zipper str i = HeadState (str ++ (show i) ++ "a")
        s = repeat "s"

decrementer_end :: TransitionMap
decrementer_end = Map.fromList[
        ((Dash,HeadState "check_zero"),(Dash,JumpRight,HeadState "erase_rightmost_dash_then_fill"))
        ,((Blank,HeadState "check_zero"),(Blank,JumpLeft,HeadState "zero_left_loop"))
        ,((Dash,HeadState "zero_left_loop"),(Dash,JumpLeft,HeadState "zero_left_loop"))
        ,((Blank,HeadState "zero_left_loop"),(Blank,JumpLeft,HeadState "zero_second_to_last"))
        ,((Dash,HeadState "zero_second_to_last"),(Dash,JumpLeft, HeadState "zero_left_loop"))
        ,((Blank,HeadState "zero_second_to_last"),(Blank,JumpRight,HeadState "zero_last"))
        ,((Dash,HeadState "zero_last"),(Dash,Stay,HeadState "error"))
        ,((Blank,HeadState "zero_last"),(Blank,JumpRight,HeadState "zero_decrementer_end"))
        ,((Dash,HeadState "erase_rightmost_dash_then_fill"),(Dash,JumpRight,HeadState "erase_rightmost_dash_then_fill"))
        ,((Blank,HeadState "erase_rightmost_dash_then_fill"),(Blank,JumpLeft,HeadState "erase_it"))
        ,((Dash,HeadState "erase_it"),(Blank,Stay,HeadState "erase_it"))
        ,((Blank,HeadState "erase_it"),(Blank,JumpRight,HeadState "fill_next"))
        ,((Dash,HeadState "fill_next"),(Dash,JumpRight,HeadState "are_there_dashes"))
        ,((Blank,HeadState "fill_next"),(Dash,Stay,HeadState "fill_next"))
        ,((Dash,HeadState "are_there_dashes"),(Dash,JumpRight, HeadState "erase_rightmost_dash_then_fill"))
        ,((Blank,HeadState "are_there_dashes"),(Blank,JumpLeft,HeadState "non_zero_left_loop"))
        ,((Dash,HeadState "non_zero_left_loop"),(Dash,JumpLeft,HeadState "non_zero_left_loop"))
        ,((Blank,HeadState "non_zero_left_loop"),(Blank,JumpLeft,HeadState "non_zero_second_to_last"))
        ,((Dash,HeadState "non_zero_second_to_last"),(Dash,JumpLeft, HeadState "non_zero_left_loop"))
        ,((Blank,HeadState "non_zero_second_to_last"),(Blank,JumpRight,HeadState "non_zero_last"))
        ,((Dash,HeadState "non_zero_last"),(Dash,Stay,HeadState "error"))
        ,((Blank,HeadState "non_zero_last"),(Blank,JumpRight,HeadState "non_zero_decrementer_end"))
    ]

make_decrementer :: Int -> TransitionMap
make_decrementer n = Map.union (decrementer_begin n) decrementer_end

registersUsed :: AbacusMap -> [Register]
registersUsed aMap = Set.toList $ Set.fromList registers
    where
        registers = map getRegister $ Map.toList aMap
        getRegister (_,(Increment r _)) = r
        getRegister (_,(Decrement r _ _)) = r

--This will make register mappers
--A register mapper will map a register name to an integer
makeRegisterMapper :: [String] -> (String -> Int)
makeRegisterMapper strings = f
    where
        f s = Map.findWithDefault 0 s rankMap
        rankMap = Map.fromList $ zip (sort strings) [1..]


--The point of this is to give an entry point state
--into the TransitionMap that has the same name as the Abacus state
--For example, if the intended initial abacus state is named "start" then
--we can run the reduced (Turing) machine using "start" insted of
-- SUB_start___start
outside_entry_point :: String -> TransitionMap
outside_entry_point stateName = Map.fromList [
        ((Dash,HeadState stateName),(Dash,Stay,HeadState internalName))
        ,((Blank,HeadState stateName),(Blank,Stay,HeadState internalName))
    ] where internalName = "SUB_" ++ stateName ++ "___start"

--Note that we need to feed in a register mapper of type (String -> Int)
processAbacusPair :: (String -> Int) -> (String,AbacusRule) -> TransitionMap
processAbacusPair regmap (abacusStateName,Increment register nextState) = Map.union (outside_entry_point abacusStateName) rewiredIncrementer
    where
        rewiredIncrementer = rewire renamedIncrementer (Blank, HeadState (prefix ++ "last")) (Blank,JumpRight, nextTuringState)
        prefix = "SUB_" ++ abacusStateName ++ "___"
        n = regmap register
        nextTuringState = if nextState == "Halt" then Halt else HeadState ("SUB_" ++ nextState ++ "___start")
        renamedIncrementer = prependToStateNames prefix tMapRaw
        tMapRaw = make_incrementer n
processAbacusPair regmap (abacusStateName, Decrement register nextState nextStateIfEmpty) = Map.union (outside_entry_point abacusStateName) twiceRewiredDecrementer
    where
        twiceRewiredDecrementer = rewire rewiredDecrementer (Blank,HeadState (prefix ++ "non_zero_last")) (Blank,JumpRight,nextTuringState)
        prefix = "SUB_" ++ abacusStateName ++ "___"
        n = regmap register
        nextTuringState = if nextState == "Halt" then Halt else HeadState ("SUB_" ++ nextState ++ "___start")
        rewiredDecrementer = rewire renamedDecrementer (Blank,HeadState (prefix ++ "zero_last")) (Blank,JumpRight,nextTuringStateIfEmpty)
        nextTuringStateIfEmpty = if nextStateIfEmpty == "Halt" then Halt else HeadState ("SUB_" ++ nextStateIfEmpty ++ "___start")
        renamedDecrementer = prependToStateNames prefix tMapRaw
        tMapRaw = make_decrementer n

processAbacusMap_help :: AbacusMap -> [TransitionMap]
processAbacusMap_help amap = map (processAbacusPair regmap) $ Map.toList amap
    where
        regmap = makeRegisterMapper $ registersUsed amap

processAbacusMap :: AbacusMap -> TransitionMap
processAbacusMap = Map.unions . processAbacusMap_help
