import Control.Applicative ((<**>))
import qualified Data.Text as T (pack)
import qualified Options.Applicative as O

import Graze


parser :: O.Parser Config
parser = Config
    <$> O.argument
            (O.eitherReader $ parseUrl . T.pack)
            (O.metavar "<URL>" <> O.help "URL to start at")
    <*> O.argument
            O.str
            (O.metavar "<folder>" <> O.help "Download folder")
    <*> O.option
            O.auto
            (  O.long "depth"
            <> O.metavar "<depth>"
            <> O.value 3
            <> O.showDefault
            <> O.help "Depth of the search"
            )
    <*> O.option
            O.auto
            (  O.long "threads"
            <> O.metavar "<threads>"
            <> O.value 10
            <> O.showDefault
            <> O.help "Number of threads"
            )

main :: IO ()
main = do
    config <- O.execParser $ O.info (parser <**> O.helper) O.fullDesc
    run config
