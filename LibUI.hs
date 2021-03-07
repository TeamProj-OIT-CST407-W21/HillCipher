module LibUI where

import Data.List as L
import Data.Char as Ch

---ABOUT: Library for formatters in main

formatKeyBasic :: [Char] -> [Char]
formatKeyBasic key = L.map Ch.toLower(removenonLetters key)

---basic code courtesy of stack overflow
---- helper for removing all non-letter ASCII characters from plaintext input 
removenonLetters :: [Char] -> [Char]
removenonLetters message = filter (not . (`elem` ",.<> ?!-@#$%^&*1234567890()_+=|{}[]~:;\"\'")) message

---same but keeps numerical
removeSpecial:: [Char] -> [Char]
removeSpecial message = filter (not . (`elem` ",.<> ?!-@#$%^&*()_+=|{}[]~:;\"\'")) message
