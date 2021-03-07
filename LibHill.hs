module LibHill where

import Data.List as L
import Data.Tuple as T
import Data.Char as Ch
import GHC.Real as N

-----calculate K inverse (decryption key)
calcDKeyTwos :: [[Int]] -> [[Int]]
calcDKeyTwos matrix
  | (checkDinverse (calDeterTwobyTwo matrix) == 1) = scalarMultMatrixTwos (euclidInverse (calDeterTwobyTwo matrix) 26) (adjugateTwobyTwo matrix)
  | (checkDinverse (calDeterTwobyTwo matrix) /= 1) = [[-1]] 
  
---check matrix is not one-way and will be able to be decrypted
checkMatrix :: [[Int]] -> Bool
checkMatrix matrix
  | ((checkDinverse (calDeterTwobyTwo matrix) == 1) && (calDeterTwobyTwo matrix /= 13) && (calDeterTwobyTwo matrix /= 2)) = True
  | ((checkDinverse (calDeterTwobyTwo matrix) /= 1) || (calDeterTwobyTwo matrix == 13) || (calDeterTwobyTwo matrix == 2)) = False
  
----checks that determinate*(inverseGCD (determinate mod)) % mod = 1
---returns 1 if true, some other number if false
---due to checks in euclidInverse, will also check gcd(determinate, 26) = 1
checkDinverse :: Int -> Int
checkDinverse deter = mod (deter * (euclidInverse deter 26)) 26 


-----calculate K inverse for a 2x2 matrix
calcDKeyTwos :: [[Int]] -> [[Int]]
calcDKeyTwos matrix
  | (checkDinverse (calDeterTwobyTwo matrix) == 1) = scalarMultMatrixTwos (euclidInverse (calDeterTwobyTwo matrix) 26) (adjugateTwobyTwo matrix)
  | (checkDinverse (calDeterTwobyTwo matrix) /= 1) = [[-1]]
  
 
 ---core encrypt/decrypt for 2x2 matrices, once converted from char, then back to int
intBasicMathFullTwos :: [Int] -> [[Int]] -> [Int]
intBasicMathFullTwos list matrix = changeVecttoList (intMathVectlistThree list matrix) [] 0


---mathematical helpers

--- will return x val in ax + by = 1
---- will return negative x, but after math and mod will still be the correct answer
euclidInverse :: Int -> Int -> Int
euclidInverse num mod
  | ((gcd num mod) /= 1) = (-1)
  | ((gcd num mod) == 1) = T.fst (gcdext num mod)
  
----where n = a, m = b in the Bezout's algo
---- ax + by = 1
----- where the return is (x, y)
----- answers order relative to ax and bx
gcdext :: Int -> Int -> (Int, Int)
gcdext n m = gcdexthelper n m 1 0 0 1 where
  gcdexthelper n m x1 y1 x2 y2 
   | m == 0 = (x1, y1)
   | otherwise = gcdexthelper m r x1p y1p x2p y2p where
     q = div n m
     r = mod n m
     x1p = x2
     y1p = y2
     x2p = x1 - q * x2
     y2p = y1 - q * y2

--computes multiplication of a single parsed 2x2 matrix with a 1x2 vector
--- [a b] * [x]
----[c d]   [y]
---[ax + by]
--- [cx + dy]
-- for a new 1x2 vector
matrixTwosVectMult :: [[Int]] -> [[Int]] -> [[Int]]
matrixTwosVectMult matrix vect = [[((matrix !! 0 !! 0) * (vect !! 0 !! 0) + (matrix !! 0 !! 1) * (vect !! 1 !! 0))], [((matrix !! 1 !! 0) * (vect !! 0 !! 0) + (matrix !! 1 !! 1) * (vect !! 1 !! 0))]]

---find a 2x2 matrix determinate
--- ad - cd in the following:
---- [ a b ]
---- [ c d ]
calDeterTwobyTwo :: [[Int]] -> Int
calDeterTwobyTwo matrix = ((matrix !! 0 !! 0) * (matrix !! 1 !! 1)) - ((matrix !! 0 !! 1) * (matrix !! 1 !! 0))

---computes the adjugate of a 2x2 matrix
---[a b]
---[c d]
---becomes
----[d -b]
----[-c a]
adjugateTwobyTwo :: [[Int]] -> [[Int]]
adjugateTwobyTwo matrix = [[(matrix !! 1 !! 1), (flipPolarity (matrix !! 0 !! 1))], [(flipPolarity (matrix !! 1 !! 0)), (matrix !! 0 !! 0)]] 

--changes -5 to 5 and 5 to -5
flipPolarity :: Int -> Int
flipPolarity x = 0 - x

---data format helpers (see readme)

---char list to matrix
charKeytoMatrix :: [Char] -> [[Int]]
charKeytoMatrix list = intListtoMatrix (messageNum list) 

---will need to be sure is 2x2 or 4 long, but takes an integer list and converts to a 2x2 matrix
intListtoMatrix :: [Int] -> [[Int]]
intListtoMatrix list = [(T.fst (splitAt 2 list)), (T.snd (splitAt 2 list))]

-----basic converter for a list to a list of 1x2 vectors
---example of arg to use: listVect [1,2..14] 1 [[[]]]
listVect :: [Int] -> Int -> [[[Int]]] -> [[[Int]]]
listVect list iter vectors
 | (iter > (div (length list) 2)) = reverse (take ((length vectors) - 1) vectors)
 | (iter <= (div (length list) 2)) = listVect list (iter + 1) ((changeVectAT list iter) : vectors) 
 
 ---basic converter from a list of 1x2 vectors back to a plain list
 ----example of arg to use: changeVecttoList a [] 0
changeVecttoList :: [[[Int]]] -> [Int] -> Int -> [Int]
changeVecttoList vectors newlist iter
 | (iter >= (length vectors)) = newlist
 | (iter < (length vectors)) = changeVecttoList vectors (L.concat [newlist, (L.concat [vectors !! iter !! 0, vectors !! iter !! 1])]) (iter + 1)
 

----helpers for working with vectors

changeVectAT :: [Int] -> Int -> [[Int]]
changeVectAT list iter = [[(getTwoElem list iter) !! 0], [(getTwoElem list iter) !! 1]]

getTwoElem :: [Int] -> Int -> [Int]
getTwoElem list iter
  | (iter == 1) = take 2 list
  | (iter > 1) = drop ((iter * 2) -2) (take (iter * 2) list)
  | (iter < 1) = [-1]


---helpers for core encrypt/decrypt math

intMathVectlistThree :: [Int] -> [[Int]] -> [[[Int]]]
intMathVectlistThree list matrix = mathVectlistTwo (listVect list 1 [[[]]]) matrix


mathVectlistTwo :: [[[Int]]] -> [[Int]] -> [[[Int]]] 
mathVectlistTwo vectors matrix = L.map scalarModVect (mathVectlistOne vectors [[[]]] matrix 0)

mathVectlistOne :: [[[Int]]] -> [[[Int]]] -> [[Int]] -> Int -> [[[Int]]]
mathVectlistOne vectors newvectors matrix iter
  | (iter >= (length vectors)) = reverse (take ((length newvectors) - 1) newvectors)
  | (iter < (length vectors)) = mathVectlistOne vectors (matrixTwosVectMult matrix (vectors !! iter) : newvectors) matrix (iter + 1)

