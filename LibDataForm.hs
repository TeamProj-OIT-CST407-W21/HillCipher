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