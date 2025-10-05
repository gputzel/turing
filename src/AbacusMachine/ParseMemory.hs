module AbacusMachine.ParseMemory where

import AbacusMachine.Types

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Data.Map as Map
import Data.Map (Map)

registerParser :: Parser Register
registerParser = do
    first <- letter
    rest <- many alphaNum
    return(first:rest)

intParser :: Parser Int
intParser = read <$> many1 digit

registerLineParser :: Parser (Register,Int)
registerLineParser = do
    reg <- registerParser
    spaces *> char ':' <* spaces
    val <- intParser
    return (reg,val)

memoryStateParser :: Parser MemoryState
memoryStateParser = do
    lines <- registerLineParser `sepEndBy` newline
    eof
    return $ Map.fromList lines

parseMemoryState :: String -> Either ParseError MemoryState
parseMemoryState = parse memoryStateParser ""
