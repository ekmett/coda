{

{-# language OverloadedStrings #-}
{-# language TypeFamilies #-}
{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}

---------------------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
---------------------------------------------------------------------------------

module Coda.Syntax.Lexer
  ( Tok(..)
  , TokenType(..)
  , Pair(..)
  ) where

import Coda.Relative.Class
import Coda.Relative.Delta
import Coda.Relative.Located
import Coda.Syntax.Dyck
import Coda.Syntax.Token
import Coda.Syntax.Rope
import Coda.Util.Alex
import Data.Char
import Data.Data
import Data.Default
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Read
import Data.Text.Unsafe as Text
import GHC.Generics

}

$whitechar = [ \t\n\r\f\v]
$special   = [\,\;\`]
$closing   = [\)\]\}]
$opening   = [\(\[\{]
$digit     = 0-9
$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$unisymbol = [] -- TODO
$symbol    = [$ascsymbol $unisymbol] # [$special \_\:\"\']
$large     = [A-Z \xc0-\xd6 \xd8-\xde]
$small     = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha     = [$small $large]
$graphic   = [$small $large $symbol $digit $special \:\"\']
$octit     = 0-7
$hexit     = [0-9 A-F a-f]
$idchar    = [$alpha $digit \']
$symchar   = [$symbol \:]
$nl        = [\n\r]
$charesc   = [abfnrtv\\\"\'\&]

@reservedid
  = as | case | class | data | default | deriving | do | else | hiding | if
  | import | in | infix | infixl | infixr | instance | let | module | newtype
  | of | qualified | then | type | where
@reservedop =
  ".." | ":" | "::" | "=" | \\ | "|" | "<-" | "->" | "@" | "~" | "=>"

@varid  = $small $idchar*
@conid  = $large $idchar*
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

@escape  = \\ ($charesc | @ascii | @decimal | o @octal | x @hexadecimal)
@string  = $graphic # [\"\\] | " " | @escape

haskell :-
<0> $white+ { skip }
<0> "--"\-*[^$symbol].* { skip }
<0> $special { lexeme LSpecial }
<0> $closing { closing }
<0> $opening { opening }
<0> @reservedid { lexeme LReservedId }
<0> (@conid \.)+ @varid { lexeme LQVarId }
<0> (@conid \.)+ @conid { lexeme LQConId }
<0> @varid { lexeme LVarId }
<0> @conid { lexeme LConId }
<0> @reservedop { lexeme LReservedOp }
<0> (@conid \.)+ @varop { lexeme LQVarOp }
<0> (@conid \.)+ @conop { lexeme LQConOp }
<0> @varop { lexeme LVarOp }
<0> @conop { lexeme LConOp }
<0> @decimal { decimal }
<0> 0[oO] @octal { octal }
<0> 0[xX] @hexadecimal { hexadecimal }
<0> @sign @decimal \. @decimal @exponent?
  | @sign @decimal @exponent { double }
<0> \' ($graphic # [\'\\] | " " | @escape) \' { lexeme LChar }
<0> \" @string* \" { lexeme LString }

{

data TokenType
  = LSpecial
  | LReservedId
  | LQVarId
  | LQConId
  | LVarId
  | LConId
  | LReservedOp
  | LQVarOp
  | LQConOp
  | LVarOp
  | LConOp
  | LInteger
  | LFloat
  | LChar
  | LString
  deriving (Eq,Ord,Show,Read,Ix,Enum,Bounded,Data,Generic)

data Tok
  = Tok        {-# unpack #-} !Delta !TokenType {-# unpack #-} !Text
  | TokInteger {-# unpack #-} !Delta !Integer
  | TokDouble  {-# unpack #-} !Delta !Double
  deriving (Eq,Ord,Show,Read,Data,Generic)

instance Relative Tok where
  rel d (Tok d' ty t) = Tok (d+d') ty t
  rel d (TokInteger d' i) = TokInteger (d+d') i
  rel d (TokDouble d' f) = TokDouble (d+d') f

data instance Pair Tok = Brace | Bracket | Paren
  deriving (Eq,Ord,Show,Read,Ix,Enum,Bounded,Data,Generic)

pair :: Char -> Pair Tok
pair '(' = Paren
pair ')' = Paren
pair '[' = Bracket
pair ']' = Bracket
pair '{' = Brace
pair '}' = Brace
pair _ = error "bad pair"

type Action = Dyck Tok -> Int -> Text -> Int -> Dyck Tok

skip :: Action
skip xs _ _ _ = xs

lexeme :: TokenType -> Action
lexeme ty xs d t len = token xs $ Token $ Tok (Delta d) ty $ Text.takeWord16 len $ Text.dropWord16 d t

opening :: Action
opening xs d t _ = open xs $ Located (Delta d) $ case Text.iter t d of Text.Iter c _ -> pair c

closing :: Action
closing xs d t _ = close xs $ Located (Delta d) $ case Text.iter t d of Text.Iter c _ -> pair c

reader :: Int -> (Delta -> a -> Tok) -> Read.Reader a -> Action
reader o f p xs d t l = token xs $ case p $ Text.takeWord16 (l-o) $ Text.dropWord16 (d+o) t of
  Left _       -> LexicalError (Delta d)
  Right (a, _) -> Token $ f (Delta d) a

decimal, double, hexadecimal, octal :: Action
decimal     = reader 0 TokInteger (Read.signed Read.decimal)
double      = reader 0 TokDouble (Read.signed Read.double)
hexadecimal = reader 2 TokInteger Read.hexadecimal
octal       = reader 2 TokInteger $ \ txt -> Right (Text.foldl' go 0 txt, "") where
  go n d = n * 8 + fromIntegral (digitToInt d)

instance Lexer Tok where
  lex t0 = go t0 (fromText t0) def where
    go t inp xs = case alexScan inp 0 of
      AlexEOF              -> xs
      AlexError inp'       -> go t inp' $ token xs $ LexicalError $ Delta $ alexInputDelta inp
      AlexSkip inp' _      -> go t inp' xs
      AlexToken inp' l act -> go t inp' $ act xs (alexInputDelta inp) t l

}
