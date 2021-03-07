module LibUI where

---ABOUT: Library for formatters in main

removenonLetters :: [Char] -> [Char]
removenonLetters message = filter (not . (`elem` ",.<> ?!-@#$%^&*1234567890()_+=|{}[]~:;\"\'")) message

removeSpecial:: [Char] -> [Char]
removeSpecial message = filter (not . (`elem` ",.<> ?!-@#$%^&*()_+=|{}[]~:;\"\'")) message
