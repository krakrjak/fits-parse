{-|
Module      : MegaParser
Description : MegaParsec based parser for an HDU.
Copyright   : (c) Zac Slade, 2018
License     : BSD2
Maintainer  : krakrjak@gmail.com
Stability   : experimental

Parsing rules for an HDU in a FITS file.
-}

module MegaParser where

-- qualified imports
import qualified Data.ByteString.Lazy as LBS
import qualified Text.Megaparsec.Byte.Lexer as L

-- explicit imports
import Data.ByteString.Lazy (ByteString)
import Data.Word (Word8)
import Data.Void (Void)

-- full module imports
import Text.Megaparsec

-- local imports
import HeaderDataUnit

type Parser = Parsec Void ByteString
type ParseErr = ParseError Word8 Void
