module TuringMachine.Parse where

import Text.Parsec
import Text.Parsec.String (Parser)

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
