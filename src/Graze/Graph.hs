{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module Graze.Graph (main) where

import Control.Exception (Exception, IOException, catch, throwIO)
import Data.Foldable (toList)
import Language.Dot.Pretty
import Language.Dot.Syntax
import Options.Applicative (Parser, ParserInfo)
import System.Directory (listDirectory)
import System.FilePath ((</>))

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as Lazy
import qualified Options.Applicative as Options

import Graze.Node
import Graze.URI

data GraphOptions = GraphOptions
    { folder :: FilePath  -- ^ Folder that the nodes are stored in.
    , output :: FilePath  -- ^ Output file.
    }

graphOptionsParser :: Parser GraphOptions
graphOptionsParser =
        GraphOptions
    <$> Options.argument
            Options.str
            (   Options.metavar "<folder>"
            <>  Options.help "Folder that the nodes are stored in."
            )
    <*> Options.argument
            Options.str
            (Options.metavar "<output>" <> Options.help "Output file.")

graphOptionsParserInfo :: ParserInfo GraphOptions
graphOptionsParserInfo =
    Options.info (Options.helper <*> graphOptionsParser) Options.fullDesc

data GrazeGraphException
    = NodeReadException IOException
    | NodeParseException FilePath String
    deriving Show

instance Exception GrazeGraphException

wrapIOException :: IO a -> IO a
wrapIOException m = catch @IOException m (throwIO . NodeReadException)

throwsWith :: Exception e => (a -> e) -> Either a b -> IO b
throwsWith f = either (throwIO . f) pure

readNode :: FilePath -> IO Node
readNode file = do
    bs <- wrapIOException (Lazy.readFile file)
    throwsWith (NodeParseException file) (Aeson.eitherDecode' bs)

writeGraph :: FilePath -> Graph -> IO ()
writeGraph file = writeFile file . renderDot

makeId :: URI -> Id
makeId = StringId . show

makeEdge :: URI -> URI -> Statement
makeEdge x y = EdgeStatement [u, v] mempty
  where
    u = ENodeId NoEdge (NodeId (makeId x) Nothing)
    v = ENodeId DirectedEdge (NodeId (makeId y) Nothing)

makeNode :: Node -> [Statement]
makeNode Node {..} =
    fmap
        (makeEdge nodeLocation)
        (filter sameAuthority (toList nodeLinks))
  where
    sameAuthority uri = uriAuthority uri == uriAuthority nodeLocation

makeGraph :: [Node] -> Graph
makeGraph = Graph UnstrictGraph DirectedGraph Nothing . concatMap makeNode

main :: IO ()
main = do
    GraphOptions {..} <- Options.execParser graphOptionsParserInfo
    files <- listDirectory folder
    nodes <- traverse (readNode . (folder </>)) files
    writeGraph output (makeGraph nodes)
