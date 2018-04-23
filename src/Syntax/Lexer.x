{

{-# language BangPatterns #-}
{-# language TypeFamilies #-}
{-# language DeriveGeneric #-}
{-# language DeriveDataTypeable #-}
{-# language StrictData #-}
{-# options_ghc -funbox-strict-fields #-}

---------------------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017-2018
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- A simple input adapter that allows @alex@ to work with 'Text'
---------------------------------------------------------------------------------

module Syntax.Lexer
  ( AlexInput(..)
  , AlexInputState(..)
  , alexGetByte
  , fromText
  , Tok
  ) where

import Data.Bits
import Data.Data
-- import Data.String
import Data.List as List
import Data.Text as Text
import qualified Data.Text.Short as ShortText
import Data.Text.Short (ShortText)
import Data.Text.Unsafe as Text
import Data.Word (Word8)
import GHC.Generics
import Text.Megaparsec.Pos
import Text.Megaparsec.Stream

import Syntax.Located

}

$whitechar = [ \t\n\r\f\v]
$special = [\,\;\`]
$closing = [\)\]\}]
$opening = [\(\[\{]
$digit = 0-9
$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$unisymbol = [] -- TODO
$symbol = [$ascsymbol $unisymbol] # [$special $closing $opening \_\:\"\']
$large = [A-Z \xc0-\xd6 \xd8-\xde]
$small = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha = [$small $large]
$graphic = [$small $large $symbol $digit $special $closing $opening \:\"\']
$octit = 0-7
$hexit = [0-9 A-F a-f]
$idchar = [$alpha $digit \']
$symchar = [$symbol \:]
$nl = [\n\r]
$charesc = [abfnrtv\\\"\'\&]

@layoutid = do | of | let | where
@keyid = case | in

@keyop = ".." | ":" | "::" | "=>"

@varid = $small $idchar*
@conid = $large $idchar*
@varop = $symbol $symchar*
@conop = \: $symchar*
@decimal = $digit+
@octal = $octit+
@hexadecimal = $hexit+
@exponent = [eE] [\-\+] @decimal
@sign = [\-\+]?

$cntrl = [$large \@\[\\\]\^\_]

@ascii
  = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
  | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
  | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
  | SUB | ESC | FS | GS | RS | US | SP | DEL
@escape = \\ ($charesc | @ascii | @decimal | o @octal | x @hexadecimal)
@string  = $graphic # [\"\\] | " " | @escape

coda :-
<0> $white+  ;
<0> "#"\#*[^$symbol].* ;
<0> $special { Special }
<0> $closing { Closing }
<0> $opening { Opening }
<0> @layoutid { LayoutId }
<0> @keyid { KeyId }
<0> (@conid \.)+ @varid { QualifiedId False False }
<0> (@conid \.)+ @conid { QualifiedId False True }
<0> (@conid \.)+ @varop { QualifiedOp True False }
<0> (@conid \.)+ @conop { QualifiedOp True True }
<0> @varid { UnqualifiedId False False }
<0> @conid { UnqualifiedId False True }
<0> @varop { UnqualifiedOp True False }
<0> @conop { UnqualifiedOp True True }
<0> @keyop { KeyOp }
<0> @decimal { decimal }
<0> 0[oO] @octal { octal }
<0> 0[xX] @hexadecimal { hexadecimal }
<0> @sign @decimal \. @decimal @exponent?
 | @sign @decimal @exponent { double }
<0> \' ($graphic # [\'\\] | " " | @escape) \' { literalChar }
<0> \" @string* \" { LiteralString }

{


data AlexInputState
  = S0 | S1 | S2 | S3
  deriving (Eq, Ord, Show, Read, Data, Generic)

-- |
-- Invariants:
--
-- @
-- 'delta' >= 0
-- @
data AlexInput = AlexInput
  { alexInputState     :: AlexInputState
  , alexInputPrevChar  :: Char
  , alexInputSourcePos :: {-# nounpack #-} SourcePos
  , alexInputDelta     :: Int
  , alexInputText      :: Text
  } deriving (Eq, Ord, Show, Read, Data, Generic)

instance IsString AlexInput where
  fromString = fromText . fromString

fromText :: Text -> AlexInput
fromText = AlexInput S0 '\n' (initialPos "stdin") 0
{-# inline conlike fromText #-}

ok :: a -> b -> Maybe (a,b)
ok !a !b = Just (a,b)

-- |
-- >>> Prelude.take 20 $ List.unfoldr alexGetByte "hello world"
-- [104,101,108,108,111,32,119,111,114,108,100]
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (AlexInput !s !c !p !d !t) = case s of
  S3 | i <- fromEnum c -> ok (fromIntegral $ 0x80 + unsafeShiftR i 12 .&. 0x3f) (AlexInput S2 c p (d+1) t)
  S2 | i <- fromEnum c -> ok (fromIntegral $ 0x80 + unsafeShiftR i 6  .&. 0x3f) (AlexInput S1 c p (d+1) t)
  S1 | i <- fromEnum c -> ok (fromIntegral $ 0x80 + i .&. 0x3f)                 (AlexInput S0 c p (d+1) t)
  S0 | d < Text.lengthWord16 t -> case Text.iter t d of
      Text.Iter c' d'
        | i' == 10     -> ok (fromIntegral i')                          (AlexInput S0 c' (nl p) (d+d') t)
        | i' == 9      -> ok (fromIntegral i')                          (AlexInput S0 c' (nt p) (d+d') t)
        | i' <= 0x7f   -> ok (fromIntegral i')                          (AlexInput S0 c' (nc p) (d+d') t)
        | i' <= 0x7ff  -> ok (fromIntegral $ 0xc0 + unsafeShiftR i' 6)  (AlexInput S1 c' (nc p) (d+d') t)
        | i' <= 0xffff -> ok (fromIntegral $ 0xe0 + unsafeShiftR i' 12) (AlexInput S2 c' (nc p) (d+d') t)
        | otherwise    -> ok (fromIntegral $ 0xf0 + unsafeShiftR i' 18) (AlexInput S3 c' (nc p) (d+d') t)
        where i' = fromEnum c'
              nc (SourcePos n l col) = SourcePos n l (col<>pos1)
              nt (SourcePos n l col) = SourcePos n l $ mkPos $ unPos col + tabs - rem (unPos col-1) tabs
              nl (SourcePos n l _) = SourcePos n (l<>pos1) pos1
              tabs = 8

    | otherwise -> Nothing

type Action = ShortText -> Tok

literalChar, octal, decimal, hexadecimal :: Action
literalChar _ = LiteralChar 'x' -- TODO
octal _ = LiteralInteger 0
decimal _ = LiteralInteger 0
hexadecimal _ = LiteralInteger 0
double _ = LiteralDouble 0

data Tok
  = Special ShortText 
  | Closing ShortText
  | Opening ShortText
  | LayoutId ShortText
  | KeyId ShortText
  | KeyOp ShortText
  | QualifiedId Bool Bool ShortText
  | QualifiedOp Bool Bool ShortText
  | UnqualifiedId Bool Bool ShortText
  | UnqualifiedOp Bool Bool ShortText
  | LiteralDouble Double
  | LiteralInteger Integer
  | LiteralChar Char
  | LiteralString ShortText
  deriving (Eq,Ord,Show,Read)

instance Stream AlexInput where
  type Token AlexInput = Located Tok
  type Tokens AlexInput = [Located Tok]
  tokenToChunk _ t = [t]
  tokensToChunk _ = id
  chunkToTokens _ = id
  chunkLength _ = List.length
  chunkEmpty _ = Prelude.null
  positionAt1 _ _ (Located p _ _) = p
  positionAtN _ s [] = s
  positionAtN _ _ (Located p _ _:_) = p
  advance1 _ _ _ (Located _ _ q) = q
  advanceN _ _ s [] = s
  advanceN _ _ _ xs = case List.last xs of Located _ _ q -> q
  take1_ s = case alexScan s 0 of
    AlexEOF -> Nothing
    AlexError _ -> Nothing
    AlexSkip s' _ -> take1_ s'
    AlexToken s' l act -> Just (Located (alexInputSourcePos s) t (alexInputSourcePos s'), s')
      where t = act $ ShortText.fromText $ Text.take l $ Text.drop (alexInputDelta s) $ alexInputText s
  takeN_ n s
    | n <= 0 = Just ([],s)
    | otherwise = case take1_ s of
      Nothing -> Just ([],s)
      Just (a,s') -> case takeN_ (n-1) s' of
        Nothing -> Just ([a],s')
        Just (as,s'') -> Just (a:as,s'')
  takeWhile_ p s = case take1_ s of
    Nothing -> ([],s)
    Just (a,s') 
      | p a -> case takeWhile_ p s' of
         (as,s'') -> (a:as,s'')
      | otherwise -> ([],s)

}
