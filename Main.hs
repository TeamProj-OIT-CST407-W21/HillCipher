module Main where

import Data.List as L
import LibHillM as LH
import System.Exit as E
import LibUI as UI
import MessageCrypto as MC
import LibDataForm as DF

main :: IO()
main = do
   putStrLn "Enter a message to en/decrypt: "
   initialMsg <- getLine
   let msgMatrix = (LH.formatMFin initialMsg)
   putStrLn "Message after padding: "
   print msgMatrix
   putStrLn "Input a key - the key must be 4 characters long: "
   initialKey <- getLine
   let tempKeyMatrix = (LH.formatMFin initialKey)
   if ((L.length tempKeyMatrix) /= 4)
      then do
         putStrLn "ERROR: Key is improper length, returning to top."
         main
   else do
      ---do decryption key generation
      ---do check for if decryption key returned error
      ---if ((checkMatrix decyrKey))
      ---let decyrKey = calcDKeyTwos (intListtoMatrix (messageNum tempKeyMatrix))
      ---output decryption key --- in matrix form?
      putStrLn "\nWould you like to encrypt('e') or decrypt('d'): "
      choice <- getLine
      if (choice == "e")
         then do
            ---do encryption stuff
            putStrLn "Your encrypted message is: "
            ---output the message
            putStrLn "\nPress y to continue, or any other key to exit: "
            continue <-getLine
            if (continue == "y")
               then main
            else
               E.exitSuccess
      else if (choice == "d")
         then do
            ---do decryption stuff
            putStrLn "Your decrypted message is: "
            ---output the message
            putStrLn "\nPress y to continue, or any other key to exit: "
            continue <-getLine
            if (continue == "y")
               then main
            else
               E.exitSuccess
      else do
         putStrLn " ERROR: Invalid choice, returning to top!"
         main

outputMatrix :: [Char] -> IO()
outputMatrix m = do
  let work = [[m!!0],[m!!1],[m!!2],[m!!3]]
  putStrLn "Your key is: "
  putStrLn ("|" ++ (work!!0) ++ " " ++ (work!!1) ++ "|")
  putStrLn ("|" ++ (work!!2) ++ " " ++ (work!!3) ++ "|")
