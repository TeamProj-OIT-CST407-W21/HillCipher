# HillCipher 

Chosen implementation uses an internal structure of [[a, b], [c, d]] for matrixes (the key) and [[a],[b]] for the vectors needed in the message
parsing to match the visual representations of the linear algebra used in Hill Cipher.  However, this could have been implemented by doing the
same math, by [a, b, c, d] for matrices, forcing a list length of 4, and parsing a list apart by pairs for the message text. Programmically in the future, this is probably the better way to go, but does show a clearer represation of the data in this format.
