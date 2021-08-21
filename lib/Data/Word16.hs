-- | Word16 module to be used with ByteString/ShortByteString.
-- All functions assumes that 'Word16' is encoded in BE.
module Data.Word16 (
  -- * Re-exporting
    Word16
  -- * conversion
  , word16ToChar
  , charToWord16
  -- * Character classification
  , isControl, isSpace, isLower, isUpper
  , isAlpha, isAlphaNum, isPrint, isDigit, isOctDigit, isHexDigit
  , isLetter, isMark, isNumber, isPunctuation, isSymbol, isSeparator
  -- * Subranges
  , isAscii, isLatin1, isAsciiUpper, isAsciiLower
  -- * Case conversion
  , toUpper, toLower, toTitle
  -- * ASCII characters
  , _nul, _tab, _lf, _vt, _np, _cr
  , _space, _exclam, _quotedbl, _numbersign, _dollar, _percent, _ampersand, _quotesingle, _parenleft, _parenright, _asterisk, _plus, _comma, _hyphen, _period, _slash
  , _0, _1, _2, _3, _4, _5, _6, _7, _8, _9
  , _colon, _semicolon, _less, _equal, _greater, _question, _at
  , _A, _B, _C, _D, _E, _F, _G, _H, _I, _J, _K, _L, _M, _N, _O, _P, _Q, _R, _S, _T, _U, _V, _W, _X, _Y, _Z
  , _bracketleft, _backslash, _bracketright, _circum, _underscore, _grave
  , _a, _b, _c, _d, _e, _f, _g, _h, _i, _j, _k, _l, _m, _n, _o, _p, _q, _r, _s, _t, _u, _v, _w, _x, _y, _z
  , _braceleft, _bar, _braceright, _tilde, _del
  -- * Some Latin-1 characters
  , _nbsp
  , _ordfeminine, _softhyphen, _mu, _ordmasculine
  , _s2, _s3, _s1, _1'4, _1'2, _3'4
  , _Agrave, _Odieresis, _Oslash, _Thorn
  , _germandbls, _agrave, _odieresis, _oslash, _thorn, _ydieresis
  ) where

import qualified Data.Char as C
import Data.Word
    ( Word16 )


----------------------------------------------------------------

-- | Total conversion to char.
word16ToChar :: Word16 -> Char
word16ToChar = C.chr . fromIntegral

-- | This is unsafe and clamps at Word16 maxbound.
charToWord16 :: Char -> Word16
charToWord16 = fromIntegral . C.ord

----------------------------------------------------------------

isControl :: Word16 -> Bool
isControl w = _nul <= w && w <= 0x1f
           || _del <= w && w <= 0x9f

isSpace :: Word16 -> Bool
isSpace = C.isSpace . word16ToChar

isLower :: Word16 -> Bool
isLower = C.isLower . word16ToChar

isUpper :: Word16 -> Bool
isUpper = C.isUpper . word16ToChar

isAlpha :: Word16 -> Bool
isAlpha = C.isAlpha . word16ToChar

isAlphaNum :: Word16 -> Bool
isAlphaNum = C.isAlphaNum . word16ToChar

isPrint :: Word16 -> Bool
isPrint = C.isPrint . word16ToChar

isDigit :: Word16 -> Bool
isDigit w = _0 <= w && w <= _9

isOctDigit :: Word16 -> Bool
isOctDigit w = _0 <= w && w <= _7

isHexDigit :: Word16 -> Bool
isHexDigit w = isDigit w
            || _A <= w && w <= _F
            || _a <= w && w <= _f

isLetter :: Word16 -> Bool
isLetter = C.isLetter . word16ToChar

isMark :: Word16 -> Bool
isMark = C.isMark . word16ToChar

isNumber :: Word16 -> Bool
isNumber = C.isNumber . word16ToChar

isPunctuation :: Word16 -> Bool
isPunctuation = C.isPunctuation . word16ToChar

isSymbol :: Word16 -> Bool
isSymbol = C.isSymbol . word16ToChar

isSeparator :: Word16 -> Bool
isSeparator = C.isSeparator . word16ToChar

----------------------------------------------------------------

isAscii :: Word16 -> Bool
isAscii w = _nul <= w && w <= _del

isLatin1 :: Word16 -> Bool
isLatin1 = C.isLatin1 . word16ToChar

isAsciiUpper :: Word16 -> Bool
isAsciiUpper w = _A <= w && w <= _Z

isAsciiLower :: Word16 -> Bool
isAsciiLower w = _a <= w && w <= _z

----------------------------------------------------------------

toUpper :: Word16 -> Word16
-- charToWord16 should be safe here, since C.toUpper doesn't go beyond Word16 maxbound
toUpper = charToWord16 . C.toUpper . word16ToChar

toLower :: Word16 -> Word16
-- charToWord16 should be safe here, since C.toLower doesn't go beyond Word16 maxbound
toLower = charToWord16 . C.toLower . word16ToChar

toTitle :: Word16 -> Word16
toTitle = toUpper

----------------------------------------------------------------

_nul, _tab, _lf, _vt, _np, _cr :: Word16
_nul = 0x00
_tab = 0x09
_lf  = 0x0a
_vt  = 0x0b
_np  = 0x0c
_cr  = 0x0d

_space, _exclam, _quotedbl, _numbersign, _dollar, _percent, _ampersand, _quotesingle, _parenleft, _parenright, _asterisk, _plus, _comma, _hyphen, _period, _slash :: Word16
_space       = 0x20
_exclam      = 0x21
_quotedbl    = 0x22
_numbersign  = 0x23
_dollar      = 0x24
_percent     = 0x25
_ampersand   = 0x26
_quotesingle = 0x27
_parenleft   = 0x28
_parenright  = 0x29
_asterisk    = 0x2a
_plus        = 0x2b
_comma       = 0x2c
_hyphen      = 0x2d
_period      = 0x2e
_slash       = 0x2f

_0, _1, _2, _3, _4, _5, _6, _7, _8, _9 :: Word16
_0 = 0x30
_1 = 0x31
_2 = 0x32
_3 = 0x33
_4 = 0x34
_5 = 0x35
_6 = 0x36
_7 = 0x37
_8 = 0x38
_9 = 0x39

_colon, _semicolon, _less, _equal, _greater, _question, _at :: Word16
_colon      = 0x3a
_semicolon  = 0x3b
_less       = 0x3c
_equal      = 0x3d
_greater    = 0x3e
_question   = 0x3f
_at         = 0x40

_A, _B, _C, _D, _E, _F, _G, _H, _I, _J, _K, _L, _M, _N, _O, _P, _Q, _R, _S, _T, _U, _V, _W, _X, _Y, _Z :: Word16
_A = 0x41
_B = 0x42
_C = 0x43
_D = 0x44
_E = 0x45
_F = 0x46
_G = 0x47
_H = 0x48
_I = 0x49
_J = 0x4a
_K = 0x4b
_L = 0x4c
_M = 0x4d
_N = 0x4e
_O = 0x4f
_P = 0x50
_Q = 0x51
_R = 0x52
_S = 0x53
_T = 0x54
_U = 0x55
_V = 0x56
_W = 0x57
_X = 0x58
_Y = 0x59
_Z = 0x5a

_bracketleft, _backslash, _bracketright, _circum, _underscore, _grave :: Word16
_bracketleft   = 0x5b
_backslash    = 0x5c
_bracketright = 0x5d
_circum       = 0x5e
_underscore   = 0x5f
_grave        = 0x60

_a, _b, _c, _d, _e, _f, _g, _h, _i, _j, _k, _l, _m, _n, _o, _p, _q, _r, _s, _t, _u, _v, _w, _x, _y, _z :: Word16
_a = 0x61
_b = 0x62
_c = 0x63
_d = 0x64
_e = 0x65
_f = 0x66
_g = 0x67
_h = 0x68
_i = 0x69
_j = 0x6a
_k = 0x6b
_l = 0x6c
_m = 0x6d
_n = 0x6e
_o = 0x6f
_p = 0x70
_q = 0x71
_r = 0x72
_s = 0x73
_t = 0x74
_u = 0x75
_v = 0x76
_w = 0x77
_x = 0x78
_y = 0x79
_z = 0x7a

_braceleft, _bar, _braceright, _tilde, _del :: Word16
_braceleft  = 0x7b
_bar        = 0x7c
_braceright = 0x7d
_tilde      = 0x7e
_del        = 0x7f

_nbsp :: Word16
_nbsp = 0xa0

_ordfeminine, _softhyphen, _mu, _ordmasculine :: Word16
_ordfeminine  = 0xaa
_softhyphen   = 0xad
_mu           = 0xb5
_ordmasculine = 0xba

_s2, _s3, _s1, _1'4, _1'2, _3'4  :: Word16
_s2 = 0xb2
_s3 = 0xb3
_s1 = 0xb9
_1'4 = 0xbc
_1'2 = 0xbd
_3'4 = 0xbe

_Agrave, _Odieresis, _Oslash, _Thorn :: Word16
_Agrave    = 0xc0
_Odieresis = 0xd6
_Oslash    = 0xd8
_Thorn     = 0xde

_germandbls, _agrave, _odieresis, _oslash, _thorn, _ydieresis :: Word16
_germandbls = 0xdf
_agrave     = 0xe0
_odieresis  = 0xf6
_oslash     = 0xf8
_thorn      = 0xfe
_ydieresis  = 0xff

