module Word16Spec where

import qualified Data.Char as C
import Data.Word16
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
    describe "isControl" $ do
        prop "behaves like model" $ \w ->
            isControl w == C.isControl (word16ToChar w)

    describe "isSpace" $ do
        prop "behaves like model" $ \w ->
            isSpace w == C.isSpace (word16ToChar w)

    describe "isLower" $ do
        prop "behaves like model" $ \w ->
            isLower w == C.isLower (word16ToChar w)

    describe "isUpper" $ do
        prop "behaves like model" $ \w ->
            isUpper w == C.isUpper (word16ToChar w)

    describe "isAlpha" $ do
        prop "behaves like model" $ \w ->
            isAlpha w == C.isAlpha (word16ToChar w)

    describe "isAlphaNum" $ do
        prop "behaves like model" $ \w ->
            isAlphaNum w == C.isAlphaNum (word16ToChar w)

    describe "isPrint" $ do
        prop "behaves like model" $ \w ->
            isPrint w == C.isPrint (word16ToChar w)

    describe "isDigit" $ do
        prop "behaves like model" $ \w ->
            isDigit w == C.isDigit (word16ToChar w)

    describe "isOctDigit" $ do
        prop "behaves like model" $ \w ->
            isOctDigit w == C.isOctDigit (word16ToChar w)

    describe "isHexDigit" $ do
        prop "behaves like model" $ \w ->
            isHexDigit w == C.isHexDigit (word16ToChar w)

    describe "isLetter" $ do
        prop "behaves like model" $ \w ->
            isLetter w == C.isLetter (word16ToChar w)

    describe "isMark" $ do
        prop "behaves like model" $ \w ->
            isMark w == C.isMark (word16ToChar w)

    describe "isNumber" $ do
        prop "behaves like model" $ \w ->
            isNumber w == C.isNumber (word16ToChar w)

    describe "isPunctuation" $ do
        prop "behaves like model" $ \w ->
            isPunctuation w == C.isPunctuation (word16ToChar w)

    describe "isSymbol" $ do
        prop "behaves like model" $ \w ->
            isSymbol w == C.isSymbol (word16ToChar w)

    describe "isSeparator" $ do
        prop "behaves like model" $ \w ->
            isSeparator w == C.isSeparator (word16ToChar w)

    describe "isAscii" $ do
        prop "behaves like model" $ \w ->
            isAscii w == C.isAscii (word16ToChar w)

    describe "isLatin1" $ do
        prop "behaves like model" $ \w ->
            isLatin1 w == C.isLatin1 (word16ToChar w)

    describe "isAsciiUpper" $ do
        prop "behaves like model" $ \w ->
            isAsciiUpper w == C.isAsciiUpper (word16ToChar w)

    describe "isAsciiLower" $ do
        prop "behaves like model" $ \w ->
            isAsciiLower w == C.isAsciiLower (word16ToChar w)

    describe "toUpper" $ do
        prop "behaves like model" $ prop_toUpper

    describe "toLower" $ do
        prop "behaves like model" $ \w ->
            word16ToChar (toLower w) == C.toLower (word16ToChar w)

    describe "toTitle" $ do
        prop "behaves like model" $ prop_toTitle

prop_toUpper :: Word16 -> Bool
prop_toUpper w
  | w == _mu        = True
  | w >= _ydieresis = True
  | otherwise       = word16ToChar (toUpper w) == C.toUpper (word16ToChar w)

prop_toTitle :: Word16 -> Bool
prop_toTitle w
  | w == _mu  = True
  | w >= _ydieresis = True
  | otherwise = word16ToChar (toTitle w) == C.toTitle (word16ToChar w)

----------------------------------------------------------------

