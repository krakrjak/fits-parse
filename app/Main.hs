module Main where

-- base
import Control.Monad ( unless )
import Data.Monoid( (<>) )
import Control.Applicative( (<|>), (<**>) )
import System.IO ( stdin )

import Options.Applicative( Parser, strOption, optional, short, long
                          , flag', metavar, help, execParser, info
                          , fullDesc, header, helper, progDesc
                          , argument, str )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import qualified Data.Vector as V

import System.Log.FastLogger( TimedFastLogger, ToLogStr, LogType( LogStderr )
                            , defaultBufSize, newTimeCache, simpleTimeFormat
                            , toLogStr, newTimedFastLogger, withTimedFastLogger )


-- local library imports
import Data.Fits ( HeaderDataUnit(..), HeaderData(..), Axis(..)
                 , parsePix, isBitPixInt, isBitPixFloat, pixsUnwrapI, pixsUnwrapD )
import Data.Fits.MegaParser ( getAllHDUs )

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
    timeCache <- newTimeCache simpleTimeFormat
    withTimedFastLogger timeCache (LogStderr defaultBufSize) $ \logger -> do
        fits <- bs i
        myLog logger $ "[DEBUG] input file size " ++ show (BS.length fits) ++ " bytes\n"
        hdus <- getAllHDUs fits
        myLog logger ("[DEBUG] found " ++ show (length hdus) ++ " hdu record(s)\n")
        mapM_ (processHDU logger) hdus
  where
    bs (FileInput f) = BS.readFile f
    bs StdInput = BS.hGetContents stdin

processHDU :: TimedFastLogger -> HeaderDataUnit -> IO ()
processHDU logger hdu = do
    myLog logger $ "[DEBUG] Bit Format " ++ show bpf ++ "\n"
    myLog logger $ "[DEBUG] data block size " ++ show (BS.length pd) ++ " bytes\n"
    myLog logger $ "[DEBUG] " ++ show (length ax) ++ " Axes\n"
    unless (null ax) (mapM_ logAxes ax)
    let pixCount = foldr (\x acc -> axisElementCount x * acc) 1 ax
    pixs <- parsePix pixCount bpf (LBS.fromStrict pd)
    let pxsI = if isBitPixInt bpf then pixsUnwrapI bpf pixs else []
        pxsD = if isBitPixFloat bpf then pixsUnwrapD bpf pixs else []
        pVI = V.fromList pxsI
        pVD = V.fromList pxsD
    myLog logger $ "[DEBUG] Total Pix Count: " ++ show (length pixs) ++ "\n"
    myLog logger $ "[DEBUG] Unwrapped Int Count: " ++ show (length pxsI) ++ "\n"
    myLog logger $ "[DEBUG] Unwrapped Double Count: " ++ show (length pxsD) ++ "\n"
    myLog logger $ "[DEBUG] Vector Int Count: " ++ show (length pVI) ++ "\n"
    myLog logger $ "[DEBUG] Vector Double Count: " ++ show (length pVD) ++ "\n"

  where
    hd = headerData hdu
    pd = payloadData hdu
    ax = axes hd
    bpf = bitPixFormat hd
    logAxes a = myLog logger $ "[DEBUG] Axis: "
                            ++ show (axisNumber a)
                            ++ " count: "
                            ++ show (axisElementCount a) ++ "\n"

myLog:: ToLogStr msg => TimedFastLogger -> msg -> IO ()
myLog logger msg = logger $ \ft -> toLogStr ft <> toLogStr ": " <> toLogStr msg
