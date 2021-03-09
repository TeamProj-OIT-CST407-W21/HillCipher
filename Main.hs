module Main where

import Data.List as L
import LibHillM as LH
import MessageCrypto as MC
import System.Exit as E
import LibUI as UI
import LibDataForm as DF
import GHC.Show as S

main :: IO()
main = do
   putStrLn "\nInput a key - the key must be 4 characters long (not including spaces or commas): "
   initialKey <- getLine
   let tempKey = (UI.formatKeyBasic initialKey)
   if ((L.length tempKey) /= 4) || (LH.checkMatrix (DF.chartoMatrix tempKey) == False)
      then do
         putStrLn "ERROR: Improper key, returning to top."
         main
   else do
      let basicEKey = DF.chartoMatrix tempKey
      let decyrKey = LH.calcDKeyTwos (basicEKey)
      outputIntMatrix basicEKey 'e'
      outputIntMatrix (scalarModMatrix decyrKey) 'd'
      putStrLn "\nWould you like to encrypt('e') or decrypt('d'): "
      choice <- getLine
      if (choice == "e")
         then do
            putStrLn "Enter a message to encrypt: "
            initialMsg <- getLine
            let msg = (MC.formatMFin initialMsg)
            putStrLn "Message AFTER PADDING: "
            print msg
            let encryptedMsg = MC.numCipher(LH.intBasicMathFullTwos (MC.messageNum msg) basicEKey)
            putStrLn "Your encrypted message is: "
            print encryptedMsg
            putStrLn "\nPress y to continue, or any other key to exit: "
            continue <-getLine
            if (continue == "y")
               then main
            else
               return ()
      else if (choice == "d")
         then do
            putStrLn "Enter a message to decrypt: "
            initialMsg <- getLine
            let msg = (UI.formatKeyBasic initialMsg)
            if ((mod (L.length msg) 2 == 0) && (L.length msg /= 0))
               then do
                  let decryptedMsg = MC.messagetoLower(MC.numCipher(LH.intBasicMathFullTwos (MC.messageNum msg) decyrKey))
                  putStrLn "Your decrypted message is: "
                  print decryptedMsg
                  putStrLn "\nPress y to continue, or any other key to exit: "
                  continue <-getLine
                  if (continue == "y")
                     then main
                  else
                     return ()
            else do
               putStrLn "ERROR: improper message length, returning to top!"
               main
      else do
         putStrLn " ERROR: Invalid choice, returning to top!"
         main

--outputCharMatrix :: [Char] -> IO()
--outputCharMatrix m = do
--  let listM = map show m
--  putStrLn "Your key is: "
--  putStrLn ("|" ++ (removeSpecial (listM!!0)) ++ " " ++ (removeSpecial (listM!!1)) ++ "|")
--  putStrLn ("|" ++ (removeSpecial (listM!!2)) ++ " " ++ (removeSpecial (listM!!3)) ++ "|")

outputIntMatrix :: [[Int]] -> Char -> IO()
outputIntMatrix m keyName = do
   let singleM = [m!!0!!0,m!!0!!1,m!!1!!0,m!!1!!1]
   let charM = L.map S.show singleM
   if (keyName == 'e') 
      then do
         putStrLn "Your Encryption key is: "
         putStrLn ("|" ++ charM!!0 ++ " " ++ charM!!1 ++ "|")
         putStrLn ("|" ++ charM!!2 ++ " " ++ charM!!3 ++ "|")
   else do
         putStrLn "Your Decryption key is: "
         putStrLn ("|" ++ charM!!0 ++ " " ++ charM!!1 ++ "|")
         putStrLn ("|" ++ charM!!2 ++ " " ++ charM!!3 ++ "|")
