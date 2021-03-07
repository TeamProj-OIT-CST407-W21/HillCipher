module LibUI where

---ABOUT: Library for formatters in main


---basic code courtesy of stack overflow
---- helper for removing all non-letter ASCII characters from plaintext input 
removenonLetters :: [Char] -> [Char]
removenonLetters message = filter (not . (`elem` ",.<> ?!-@#$%^&*1234567890()_+=|{}[]~:;\"\'")) message

---same but keeps numerical
removeSpecial:: [Char] -> [Char]
removeSpecial message = filter (not . (`elem` ",.<> ?!-@#$%^&*()_+=|{}[]~:;\"\'")) message
