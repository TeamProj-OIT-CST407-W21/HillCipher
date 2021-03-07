module Main where

import LibHill as LH
import System.Exit as E

main :: IO()
main = do
   putStrLn "Enter a message to en/decrypt: "
   initialMsg <- getLine
   let msgMatrix = (LH.formatMFin initialMsg)
   putStrLn "Message after padding: "
   print msgMatrix
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