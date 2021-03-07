module LibHillM where

---ABOUT all core necessary math and checks for the Hill Cipher to work
----the M is for Matrix as this form was done using representations of 2x2 matrixes and 1x2 vectors
----as [[a, b], [c, d]] and vectors [[a], [b]].  However, could be done in pure list form on the back
----end as long as can keep track of the same math (see readme)

import LibDataForm as LD

import Data.List as L
import Data.Tuple as T
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
checkDinverse deter = N.mod (deter * (euclidInverse deter 26)) 26 
  
 
 ---core encrypt/decrypt for 2x2 matrices, once converted from char, then back to int
intBasicMathFullTwos :: [Int] -> [[Int]] -> [Int]
intBasicMathFullTwos list matrix = LD.changeVecttoList (intMathVectlistThree list matrix) [] 0


---mathematical helpers

--- will return x val in ax + by = 1
---- will return negative x, but after math and mod will still be the correct answer
euclidInverse :: Int -> Int -> Int
euclidInverse num modparm
  | ((N.gcd num modparm) /= 1) = (-1)
  | ((N.gcd num modparm) == 1) = T.fst (gcdext num modparm)
  
----where n = a, m = b in the Bezout's algo
---- ax + by = 1
----- where the return is (x, y)
----- answers order relative to ax and bx
gcdext :: Int -> Int -> (Int, Int)
gcdext n m = gcdexthelper n m 1 0 0 1 where
  gcdexthelper n m x1 y1 x2 y2 
   | m == 0 = (x1, y1)
   | otherwise = gcdexthelper m r x1p y1p x2p y2p where
     q = N.div n m
     r = N.mod n m
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

----scalar mod 26 by a matrix where
--- [a, b]
----[c, d] % 26 
scalarModMatrix :: [[Int]] -> [[Int]]
scalarModMatrix matrix = L.map scalarMod matrix

---scalar mod of 1x2 matrix by 26 where
---- [a]
-----[b] % 26
scalarModVect :: [[Int]] -> [[Int]]
scalarModVect list = L.map modAlphaVect list

----helpers for scalar mod matrix
scalarMod :: [Int] -> [Int]
scalarMod list = L.map modAlpha list

modAlpha :: Int -> Int
modAlpha x = N.mod x 26

---scalar multiply 2x2 matrix where
--- [a, b]
--- [c, d] * x
scalarMultMatrixTwos :: Int -> [[Int]] -> [[Int]]
scalarMultMatrixTwos x matrix = [(multList (matrix !! 0) x), (multList (matrix !! 1) x)]

---helper for scalar mult
multList :: [Int] -> Int -> [Int]
multList list x = L.map(x*) list


---helpers for core encrypt/decrypt math

intMathVectlistThree :: [Int] -> [[Int]] -> [[[Int]]]
intMathVectlistThree list matrix = mathVectlistTwo (LD.listVect list 1 [[[]]]) matrix


mathVectlistTwo :: [[[Int]]] -> [[Int]] -> [[[Int]]] 
mathVectlistTwo vectors matrix = L.map scalarModVect (mathVectlistOne vectors [[[]]] matrix 0)

mathVectlistOne :: [[[Int]]] -> [[[Int]]] -> [[Int]] -> Int -> [[[Int]]]
mathVectlistOne vectors newvectors matrix iter
  | (iter >= (L.length vectors)) = L.reverse (L.take ((L.length newvectors) - 1) newvectors)
  | (iter < (L.length vectors)) = mathVectlistOne vectors (matrixTwosVectMult matrix (vectors !! iter) : newvectors) matrix (iter + 1)
