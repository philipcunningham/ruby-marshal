{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------
-- |
-- Module    : Data.Ruby.Marshal.Encoding
-- Copyright : (c) Philip Cunningham, 2015
-- License   : MIT
--
-- Maintainer:  802389+philipcunningham@users.noreply.github.com
-- Stability :  experimental
-- Portability: portable
--
-- Ruby encoding types.
--
--------------------------------------------------------------------

module Data.Ruby.Marshal.Encoding (
    -- * The @RubyStringEncoding@ type
    fromEnc
  , toEnc
  , RubyStringEncoding(..)
) where

import qualified Data.ByteString as BS

-- | ADT representing all supported Ruby encodings.
data RubyStringEncoding = ASCII_8BIT
               | Big5
               | Big5_HKSCS
               | Big5_UAO
               | CP50220
               | CP50221
               | CP51932
               | CP850
               | CP852
               | CP855
               | CP949
               | CP950
               | CP951
               | EUC_JP
               | EUC_JP_2004
               | EUC_KR
               | EUC_TW
               | Emacs_Mule
               | EucJP_ms
               | GB12345
               | GB18030
               | GB1988
               | GB2312
               | GBK
               | IBM437
               | IBM737
               | IBM775
               | IBM852
               | IBM855
               | IBM857
               | IBM860
               | IBM861
               | IBM862
               | IBM863
               | IBM864
               | IBM865
               | IBM866
               | IBM869
               | ISO_2022_JP
               | ISO_2022_JP_2
               | ISO_2022_JP_KDDI
               | ISO_8859_1
               | ISO_8859_10
               | ISO_8859_11
               | ISO_8859_13
               | ISO_8859_14
               | ISO_8859_15
               | ISO_8859_16
               | ISO_8859_2
               | ISO_8859_3
               | ISO_8859_4
               | ISO_8859_5
               | ISO_8859_6
               | ISO_8859_7
               | ISO_8859_8
               | ISO_8859_9
               | KOI8_R
               | KOI8_U
               | MacCentEuro
               | MacCroatian
               | MacCyrillic
               | MacGreek
               | MacIceland
               | MacJapanese
               | MacRoman
               | MacRomania
               | MacThai
               | MacTurkish
               | MacUkraine
               | SJIS_DoCoMo
               | SJIS_KDDI
               | SJIS_SoftBank
               | Shift_JIS
               | Stateless_ISO_2022_JP
               | Stateless_ISO_2022_JP_KDDI
               | TIS_620
               | US_ASCII
               | UTF8_DoCoMo
               | UTF8_KDDI
               | UTF8_MAC
               | UTF8_SoftBank
               | UTF_16
               | UTF_16BE
               | UTF_16LE
               | UTF_32
               | UTF_32BE
               | UTF_32LE
               | UTF_7
               | UTF_8
               | Windows_1250
               | Windows_1251
               | Windows_1252
               | Windows_1253
               | Windows_1254
               | Windows_1255
               | Windows_1256
               | Windows_1257
               | Windows_1258
               | Windows_31J
               | Windows_874
               | UnsupportedEncoding
               deriving (Eq, Ord, Show)

-- | Lifts encoding strings into encoding ADT.
toEnc :: BS.ByteString -> RubyStringEncoding
toEnc "ASCII-8BIT"                 = ASCII_8BIT
toEnc "UTF-8"                      = UTF_8
toEnc "US-ASCII"                   = US_ASCII
toEnc "Big5"                       = Big5
toEnc "Big5-HKSCS"                 = Big5_HKSCS
toEnc "Big5-UAO"                   = Big5_UAO
toEnc "CP949"                      = CP949
toEnc "Emacs-Mule"                 = Emacs_Mule
toEnc "EUC-JP"                     = EUC_JP
toEnc "EUC-KR"                     = EUC_KR
toEnc "EUC-TW"                     = EUC_TW
toEnc "GB18030"                    = GB18030
toEnc "GBK"                        = GBK
toEnc "ISO-8859-1"                 = ISO_8859_1
toEnc "ISO-8859-2"                 = ISO_8859_2
toEnc "ISO-8859-3"                 = ISO_8859_3
toEnc "ISO-8859-4"                 = ISO_8859_4
toEnc "ISO-8859-5"                 = ISO_8859_5
toEnc "ISO-8859-6"                 = ISO_8859_6
toEnc "ISO-8859-7"                 = ISO_8859_7
toEnc "ISO-8859-8"                 = ISO_8859_8
toEnc "ISO-8859-9"                 = ISO_8859_9
toEnc "ISO-8859-10"                = ISO_8859_10
toEnc "ISO-8859-11"                = ISO_8859_11
toEnc "ISO-8859-13"                = ISO_8859_13
toEnc "ISO-8859-14"                = ISO_8859_14
toEnc "ISO-8859-15"                = ISO_8859_15
toEnc "ISO-8859-16"                = ISO_8859_16
toEnc "KOI8-R"                     = KOI8_R
toEnc "KOI8-U"                     = KOI8_U
toEnc "Shift_JIS"                  = Shift_JIS
toEnc "UTF-16BE"                   = UTF_16BE
toEnc "UTF-16LE"                   = UTF_16LE
toEnc "UTF-32BE"                   = UTF_32BE
toEnc "UTF-32LE"                   = UTF_32LE
toEnc "Windows-31J"                = Windows_31J
toEnc "Windows-1251"               = Windows_1251
toEnc "IBM437"                     = IBM437
toEnc "IBM737"                     = IBM737
toEnc "IBM775"                     = IBM775
toEnc "CP850"                      = CP850
toEnc "IBM852"                     = IBM852
toEnc "CP852"                      = CP852
toEnc "IBM855"                     = IBM855
toEnc "CP855"                      = CP855
toEnc "IBM857"                     = IBM857
toEnc "IBM860"                     = IBM860
toEnc "IBM861"                     = IBM861
toEnc "IBM862"                     = IBM862
toEnc "IBM863"                     = IBM863
toEnc "IBM864"                     = IBM864
toEnc "IBM865"                     = IBM865
toEnc "IBM866"                     = IBM866
toEnc "IBM869"                     = IBM869
toEnc "Windows-1258"               = Windows_1258
toEnc "GB1988"                     = GB1988
toEnc "macCentEuro"                = MacCentEuro
toEnc "macCroatian"                = MacCroatian
toEnc "macCyrillic"                = MacCyrillic
toEnc "macGreek"                   = MacGreek
toEnc "macIceland"                 = MacIceland
toEnc "macRoman"                   = MacRoman
toEnc "macRomania"                 = MacRomania
toEnc "macThai"                    = MacThai
toEnc "macTurkish"                 = MacTurkish
toEnc "macUkraine"                 = MacUkraine
toEnc "CP950"                      = CP950
toEnc "CP951"                      = CP951
toEnc "stateless-ISO-2022-JP"      = Stateless_ISO_2022_JP
toEnc "eucJP-ms"                   = EucJP_ms
toEnc "CP51932"                    = CP51932
toEnc "EUC-JP-2004"                = EUC_JP_2004
toEnc "GB2312"                     = GB2312
toEnc "GB12345"                    = GB12345
toEnc "ISO-2022-JP"                = ISO_2022_JP
toEnc "ISO-2022-JP-2"              = ISO_2022_JP_2
toEnc "CP50220"                    = CP50220
toEnc "CP50221"                    = CP50221
toEnc "Windows-1252"               = Windows_1252
toEnc "Windows-1250"               = Windows_1250
toEnc "Windows-1256"               = Windows_1256
toEnc "Windows-1253"               = Windows_1253
toEnc "Windows-1255"               = Windows_1255
toEnc "Windows-1254"               = Windows_1254
toEnc "TIS-620"                    = TIS_620
toEnc "Windows-874"                = Windows_874
toEnc "Windows-1257"               = Windows_1257
toEnc "MacJapanese"                = MacJapanese
toEnc "UTF-7"                      = UTF_7
toEnc "UTF8-MAC"                   = UTF8_MAC
toEnc "UTF-16"                     = UTF_16
toEnc "UTF-32"                     = UTF_32
toEnc "UTF8-DoCoMo"                = UTF8_DoCoMo
toEnc "SJIS-DoCoMo"                = SJIS_DoCoMo
toEnc "UTF8-KDDI"                  = UTF8_KDDI
toEnc "SJIS-KDDI"                  = SJIS_KDDI
toEnc "ISO-2022-JP-KDDI"           = ISO_2022_JP_KDDI
toEnc "stateless-ISO-2022-JP-KDDI" = Stateless_ISO_2022_JP_KDDI
toEnc "UTF8-SoftBank"              = UTF8_SoftBank
toEnc "SJIS-SoftBank"              = SJIS_SoftBank
toEnc _                            = UnsupportedEncoding

-- | Lowers encoding ADT into an encoding string.
fromEnc :: RubyStringEncoding -> BS.ByteString
fromEnc ASCII_8BIT                 = "ASCII-8BIT"
fromEnc UTF_8                      = "UTF-8"
fromEnc US_ASCII                   = "US-ASCII"
fromEnc Big5                       = "Big5"
fromEnc Big5_HKSCS                 = "Big5-HKSCS"
fromEnc Big5_UAO                   = "Big5-UAO"
fromEnc CP949                      = "CP949"
fromEnc Emacs_Mule                 = "Emacs-Mule"
fromEnc EUC_JP                     = "EUC-JP"
fromEnc EUC_KR                     = "EUC-KR"
fromEnc EUC_TW                     = "EUC-TW"
fromEnc GB18030                    = "GB18030"
fromEnc GBK                        = "GBK"
fromEnc ISO_8859_1                 = "ISO-8859-1"
fromEnc ISO_8859_2                 = "ISO-8859-2"
fromEnc ISO_8859_3                 = "ISO-8859-3"
fromEnc ISO_8859_4                 = "ISO-8859-4"
fromEnc ISO_8859_5                 = "ISO-8859-5"
fromEnc ISO_8859_6                 = "ISO-8859-6"
fromEnc ISO_8859_7                 = "ISO-8859-7"
fromEnc ISO_8859_8                 = "ISO-8859-8"
fromEnc ISO_8859_9                 = "ISO-8859-9"
fromEnc ISO_8859_10                = "ISO-8859-10"
fromEnc ISO_8859_11                = "ISO-8859-11"
fromEnc ISO_8859_13                = "ISO-8859-13"
fromEnc ISO_8859_14                = "ISO-8859-14"
fromEnc ISO_8859_15                = "ISO-8859-15"
fromEnc ISO_8859_16                = "ISO-8859-16"
fromEnc KOI8_R                     = "KOI8-R"
fromEnc KOI8_U                     = "KOI8-U"
fromEnc Shift_JIS                  = "Shift_JIS"
fromEnc UTF_16BE                   = "UTF-16BE"
fromEnc UTF_16LE                   = "UTF-16LE"
fromEnc UTF_32BE                   = "UTF-32BE"
fromEnc UTF_32LE                   = "UTF-32LE"
fromEnc Windows_31J                = "Windows-31J"
fromEnc Windows_1251               = "Windows-1251"
fromEnc IBM437                     = "IBM437"
fromEnc IBM737                     = "IBM737"
fromEnc IBM775                     = "IBM775"
fromEnc CP850                      = "CP850"
fromEnc IBM852                     = "IBM852"
fromEnc CP852                      = "CP852"
fromEnc IBM855                     = "IBM855"
fromEnc CP855                      = "CP855"
fromEnc IBM857                     = "IBM857"
fromEnc IBM860                     = "IBM860"
fromEnc IBM861                     = "IBM861"
fromEnc IBM862                     = "IBM862"
fromEnc IBM863                     = "IBM863"
fromEnc IBM864                     = "IBM864"
fromEnc IBM865                     = "IBM865"
fromEnc IBM866                     = "IBM866"
fromEnc IBM869                     = "IBM869"
fromEnc Windows_1258               = "Windows-1258"
fromEnc GB1988                     = "GB1988"
fromEnc MacCentEuro                = "macCentEuro"
fromEnc MacCroatian                = "macCroatian"
fromEnc MacCyrillic                = "macCyrillic"
fromEnc MacGreek                   = "macGreek"
fromEnc MacIceland                 = "macIceland"
fromEnc MacRoman                   = "macRoman"
fromEnc MacRomania                 = "macRomania"
fromEnc MacThai                    = "macThai"
fromEnc MacTurkish                 = "macTurkish"
fromEnc MacUkraine                 = "macUkraine"
fromEnc CP950                      = "CP950"
fromEnc CP951                      = "CP951"
fromEnc Stateless_ISO_2022_JP      = "stateless-ISO-2022-JP"
fromEnc EucJP_ms                   = "eucJP-ms"
fromEnc CP51932                    = "CP51932"
fromEnc EUC_JP_2004                = "EUC-JP-2004"
fromEnc GB2312                     = "GB2312"
fromEnc GB12345                    = "GB12345"
fromEnc ISO_2022_JP                = "ISO-2022-JP"
fromEnc ISO_2022_JP_2              = "ISO-2022-JP-2"
fromEnc CP50220                    = "CP50220"
fromEnc CP50221                    = "CP50221"
fromEnc Windows_1252               = "Windows-1252"
fromEnc Windows_1250               = "Windows-1250"
fromEnc Windows_1256               = "Windows-1256"
fromEnc Windows_1253               = "Windows-1253"
fromEnc Windows_1255               = "Windows-1255"
fromEnc Windows_1254               = "Windows-1254"
fromEnc TIS_620                    = "TIS-620"
fromEnc Windows_874                = "Windows-874"
fromEnc Windows_1257               = "Windows-1257"
fromEnc MacJapanese                = "MacJapanese"
fromEnc UTF_7                      = "UTF-7"
fromEnc UTF8_MAC                   = "UTF8-MAC"
fromEnc UTF_16                     = "UTF-16"
fromEnc UTF_32                     = "UTF-32"
fromEnc UTF8_DoCoMo                = "UTF8-DoCoMo"
fromEnc SJIS_DoCoMo                = "SJIS-DoCoMo"
fromEnc UTF8_KDDI                  = "UTF8-KDDI"
fromEnc SJIS_KDDI                  = "SJIS-KDDI"
fromEnc ISO_2022_JP_KDDI           = "ISO-2022-JP-KDDI"
fromEnc Stateless_ISO_2022_JP_KDDI = "stateless-ISO-2022-JP-KDDI"
fromEnc UTF8_SoftBank              = "UTF8-SoftBank"
fromEnc SJIS_SoftBank              = "SJIS-SoftBank"
fromEnc _                          = "UnsupportedEncoding"
