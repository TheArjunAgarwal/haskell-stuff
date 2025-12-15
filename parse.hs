import Text.ParserCombinators.ReadP

isVowel :: Char -> Bool
isVowel char =
    any (char ==) "aouei"

vowel :: ReadP Char
vowel =
    satisfy isVowel