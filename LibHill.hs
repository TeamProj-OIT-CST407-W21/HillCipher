module LibHill where

import Data.List as L
import Data.Tuple as T
import Data.Char as Ch
import GHC.Real as N
import GHC.Enum as EN

----converter to change formatted plaintext to a list of numbers
messageNum :: [Char] -> [Int]
messageNum message = L.map lowercharNum message

----converter to change list of numbers to uppercase ciphertext
numCipher :: [Int] -> [Char]
numCipher list = L.map numUpperchar list

----final character formating : removing non-letter characters, padding if repeat (hello becomes helxlo) and 
----putting plaintext to lowercase
----latter for decrypt may need one for formatting decryption to lowercase plaintext ?
formatMFin :: [Char] -> [Char]
formatMFin message = lengthFormat (formatMone message)

--pad for repetition
charRepeat :: [Char] -> Int -> [Char]
charRepeat message i
  | ((i + 1) == (length message)) = message
  | ((message !! i == message !! (i+1)) && ((i+1) < length message)) = charRepeat (finConcat (message) (i+1)) (i+1)
  | ((message !! i /= message !! (i+1)) && ((i+1) < length message)) = charRepeat (message) (i+1)
  | ((i + 1) > (length message)) = ['E']

----formatter to pad if text list is odd to change to a matrix
lengthFormat :: [Char] -> [Char]
lengthFormat message
 | (N.even (length message)) = message
 | (N.odd (length message)) = L.concat [message, ['x']] 


----helpers for character repetition padding

appendXAT :: [Char] -> Int -> [Char]
appendXAT message i =  L.concat [T.fst (L.splitAt i message), ['x']]

finConcat :: [Char] -> Int -> [Char]
finConcat message i = L.concat [appendXAT message i, T.snd (splitAt i message)]


----helpers for letter to  number conversion

----helper to change lowercase plaintext to number
lowercharNum :: Char -> Int
lowercharNum letter 
  | ((letter >= 'a') && (letter <= 'z')) = (EN.fromEnum letter) - 97
  | (letter < 'a') = (-1)
  | (letter > 'z') = (-1)

-----helper to change from number to uppercase ciphertext 
numUpperchar :: Int -> Char
numUpperchar num 
 | ((num >=0) && (num <= 25)) = EN.toEnum (num + 65)::Char
 | (num < 0) = 'E'
 | (num > 25) = 'E'


----helpers for input formatting

---- intermediate helper
formatMone :: [Char] -> [Char]
formatMone message = charRepeat (messagetoLower (removenonLetters message)) 0

-----change to all lowercase
messagetoLower :: [Char] -> [Char]
messagetoLower message = L.map Ch.toLower message


---basic code courtesy of stack overflow
---- helper for removing characters from plaintext input 
removenonLetters :: [Char] -> [Char]
removenonLetters message = filter (not . (`elem` ",.<> ?!-@#$%^&*1234567890()_+=|{}[]~:;\"\'")) message