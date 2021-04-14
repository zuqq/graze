import Options.Applicative (Parser, ParserInfo)

import qualified Data.Text as Text
import qualified Options.Applicative as Options

import Graze

parser :: Parser Config
parser =
        Config
    <$> Options.argument
            (Options.eitherReader (parseUrl . Text.pack))
            (Options.metavar "<URL>" <> Options.help "URL to start at")
    <*> Options.argument
            Options.str
            (Options.metavar "<folder>" <> Options.help "Download folder")
    <*> Options.option
            Options.auto
            (   Options.long "depth"
            <>  Options.metavar "<depth>"
            <>  Options.value 3
            <>  Options.showDefault
            <>  Options.help "Depth of the search"
            )
    <*> Options.option
            Options.auto
            (   Options.long "threads"
            <>  Options.metavar "<threads>"
            <>  Options.value 10
            <>  Options.showDefault
            <>  Options.help "Number of threads"
            )

parserInfo :: ParserInfo Config
parserInfo = Options.info (Options.helper <*> parser) Options.fullDesc

main :: IO ()
main = Options.execParser parserInfo >>= run
