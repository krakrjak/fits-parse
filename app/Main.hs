module Main where

import Data.Monoid( (<>) )
import Control.Applicative( (<|>), (<**>) )
import System.IO ( stdin )

import Options.Applicative( Parser, strOption, optional, short, long
                          , flag', metavar, help, execParser, info
                          , fullDesc, header, helper, progDesc
                          , argument, str )

import qualified Data.ByteString as BS



-- | Paramaterized input type for files or standard input.
data Input = FileInput FilePath -- ^ The 'FileInput' constructor needs a path name
           | StdInput           -- ^ The `StdInput` constructor stands in for the obvious

-- | Parameterized output type for files or standard output.
data Output = FileOutput FilePath -- ^ When you have a file use 'FileOutput'
            | StdOutput           -- ^ For a screen dump use 'StdOutput'

fileInput :: Parser Input
fileInput = FileInput <$> argument str
    ( metavar "FILE"
   <> help "Input FITS file to parse" )

stdInput :: Parser Input
stdInput = flag' StdInput
    ( long "stdin"
   <> help "Input FITS from stdin" )

fileOutput :: Parser Output
fileOutput = FileOutput <$> strOption
    ( long "outfile"
   <> short 'o'
   <> help "Output filename to store" )

stdOutput :: Parser Output
stdOutput = flag' StdOutput
    ( long "stdout"
   <> help "Output picture to stdout" )

inputParse :: Parser Input
inputParse = fileInput <|> stdInput

outputParse :: Parser Output
outputParse = fileOutput <|> stdOutput

data FitsConfig = FitsConfig
    { infile :: Input
    , outImage :: Maybe Output
    }

fitsConfigParser :: Parser FitsConfig
fitsConfigParser = FitsConfig <$> inputParse <*> optional outputParse

main :: IO ()
main = workOnFITS =<< execParser opts
    where
        opts = info (fitsConfigParser <**> helper)
            ( fullDesc
           <> progDesc "Parse, generate, and render FITS files"
           <> header "fits-parse - a FITS swiss army knife" )

workOnFITS :: FitsConfig -> IO ()
workOnFITS (FitsConfig i o) = do
    bs i >>= putStrLn . show . BS.length
  where
    bs (FileInput f) = BS.readFile f
    bs StdInput = BS.hGetContents stdin
