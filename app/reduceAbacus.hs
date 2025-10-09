import qualified Options.Applicative as Opt
import Options.Applicative ((<**>))

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
    putStrLn "Hello world"
