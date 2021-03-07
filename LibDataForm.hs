module LibDataForm where

----ABOUT: Library for representing matrixes (2x2 or 3x3) or vectors (1x2 or 1x3) from linear algebra
----the form of the vector is :
---[a]
---[b]
---[c]

---as a column the data structure of: [[a], [b]] or [[a],[b],[c]] was chosen to maintain the vertical
---representation and a list of vectors is:
---[ [[a1],[b1]], [[a2],[b2]], [[a3],[b3]] ]

---for matrixes the following:
----[[a, b], [c,d]]
----[[a, b, c], [d, e, f]]

----Note: only a few current functions work for 3x3 matrixes and 1x3 vectors, most do not and therefore for now 
----this should only be considered as 2x2 and 1x2 usage only

---data format helpers (see readme)


import Data.List as L
import Data.Tuple as T

---top-level char list to matrix converter
chartoMatrix :: [Char] -> [[Int]]
chartoMatrix list = intTwosMatrix (messageNum list) 

---will need to be sure is 2x2 or 4 long, but takes an integer list and converts to a 2x2 matrix
intTwosMatrix :: [Int] -> [[Int]]
intTwosMatrix list = [(T.fst (splitAt 2 list)), (T.snd (splitAt 2 list))]

-----basic converter for a list of numbers to a list of 1x2 vectors
---example of arg to use: listVect [1,2..14] 1 [[[]]]
listVect :: [Int] -> Int -> [[[Int]]] -> [[[Int]]]
listVect list iter vectors
 | (iter > (div (length list) 2)) = reverse (take ((length vectors) - 1) vectors)
 | (iter <= (div (length list) 2)) = listVect list (iter + 1) ((changeVectAT list iter) : vectors) 
 
 ---basic converter from a list of 1x2 vectors back to a plain list of numbers
 ----example of arg to use: changeVecttoList a [] 0
changeVecttoList :: [[[Int]]] -> [Int] -> Int -> [Int]
changeVecttoList vectors newlist iter
 | (iter >= (length vectors)) = newlist
 | (iter < (length vectors)) = changeVecttoList vectors (L.concat [newlist, (L.concat [vectors !! iter !! 0, vectors !! iter !! 1])]) (iter + 1)
 

----helpers for working with vectors
----different steps in parsing elements together

changeVectAT :: [Int] -> Int -> [[Int]]
changeVectAT list iter = [[(getTwoElem list iter) !! 0], [(getTwoElem list iter) !! 1]]

getTwoElem :: [Int] -> Int -> [Int]
getTwoElem list iter
  | (iter == 1) = take 2 list
  | (iter > 1) = drop ((iter * 2) -2) (take (iter * 2) list)
  | (iter < 1) = [-1]
