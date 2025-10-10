import qualified Options.Applicative as Opt
import Options.Applicative ((<**>))
import AbacusMachine.Types
import AbacusMachine.Display

import TuringMachine.Types

import AbacusLexer
import AbacusParser

import Abacus2Turing

data Options = Options
    { optAbacusMapFile :: String
    } deriving (Show)

options :: Opt.Parser Options
options = Options
    <$> Opt.strOption
        ( Opt.long "abacusMap"
        <> Opt.metavar "FILENAME"
        <> Opt.help "File containing the abacus rules map")

opts :: Opt.ParserInfo Options
opts = Opt.info (options <**> Opt.helper)
    ( Opt.fullDesc
   <> Opt.progDesc "Reduce an abacus machine to a Turing machine"
   <> Opt.header "abacusReduce - abacus machine reducer" )

main :: IO ()
main = do
    Options { optAbacusMapFile = abacusMapFile } <- Opt.execParser opts    
    abacusInput <- readFile abacusMapFile
    let tokens = alexScanTokens abacusInput
    let amap = parseAbacus tokens
    let incrementer_transitional_block_renamed = prependToStateNames "s2" incrementer_transitional_block
    let incrementer_begin = stitch incrementer_initial_block incrementer_transitional_block_renamed (HeadState "s2a")
    putStrLn $ show incrementer_initial_block
    putStrLn $ show incrementer_transitional_block
    putStrLn $ show incrementer_begin
