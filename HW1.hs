-- Extract the digit from the highest place holder of an (positive) integer
greatestDigit :: Integer -> Integer
greatestDigit n
  | n < 10    = n
  | otherwise = greatestDigit (n `div` 10)
  
-- Extract the digit from the least place holder of an (positive) integer
leastDigit :: Integer -> Integer
leastDigit n
  | n < 10    = n
  | otherwise = leastDigit (n `mod` 10)

tryReverse :: [Integer] -> [Integer]
tryReverse []      = []
tryReverse (x:[])  = [x]
tryReverse xList   = (last xList) : tryReverse (init xList)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0                  = []
  | (n < 10) && (n >= 1)    = [n] 
  | otherwise               = (leastDigit n) : toDigitsRev ((n - leastDigit n) `div` 10) 

toDigits :: Integer -> [Integer]
toDigits n = tryReverse (toDigitsRev n)

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev []  = []
doubleEveryOtherRev (x:[]) = [x]
doubleEveryOtherRev xlist = (last xlist) : 2 * (last (init xlist)) : (doubleEveryOtherRev (init (init xlist)))

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xlist = tryReverse (doubleEveryOtherRev xlist)

-- This is not the correct function 
{-
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = x + sumDigits xs
-}

-- This one IS the correct function
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum(toDigits x) + sumDigits xs

-- Exercise 4 Check to see if number stream could be valid.
validation :: Integer -> Bool
validation sum
  | sum `mod` 10 == 0   = True
  | sum `mod` 10 /= 0   = False
  
validate :: Integer -> Bool
validate number = validation(sumDigits(doubleEveryOther(toDigits number)))

-- Exercise 5 Tower of Hanoi
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 a b c = []
hanoi 1 a b c = (a, c):(hanoi 0 a b c)
hanoi n a b c = (hanoi (n - 1) a c b) ++ (hanoi 1 a b c) ++ (hanoi (n - 1) b a c)

-- Exercise 6 Four Peg Tower of Hanoi
hanoifour :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoifour 0 a b c d = []

hanoifour 1 a b c d = (a, d):(hanoifour 0 a b c d)

hanoifour 2 a b c d = (hanoifour 1 a b d c) ++ (hanoifour 1 a b c d) ++ (hanoifour 1 c a b d)

hanoifour 3 a b c d = (hanoifour 1 a c d b) ++ (hanoifour 1 a b d c) ++ 
                      (hanoifour 1 a b c d) ++ (hanoifour 1 c a b d) ++
                      (hanoifour 1 b a c d)

hanoifour 4 a b c d = (hanoifour 2 a b d c) ++ (hanoifour 1 a c d b) ++
                      (hanoifour 1 a b c d) ++ (hanoifour 1 b a c d) ++
                      (hanoifour 2 c a b d)

hanoifour n a b c d = (hanoifour (n - 2) a c d b) ++ (hanoifour 1 a b d c) ++ 
                      (hanoifour 1 a b c d) ++ (hanoifour 1 c a b d) ++
                      (hanoifour (n - 2) b a c d)

-- Code that functions but is the wrong approach to Exercise 5
getDisc :: [Integer] -> Integer
getDisc pegList =  (head pegList)

removeTop :: [Integer] -> [Integer]
removeTop pegList = (tail pegList)

addTop :: Integer -> [Integer] -> [Integer]
addTop disk pegList = (disk:pegList)

doMove :: ([Integer], [Integer]) -> ([Integer], [Integer])
doMove (fromList, toList)
  | fromList == []    = (fromList, toList)
  | otherwise         = ((removeTop fromList), (addTop (getDisc fromList) toList))

maxMove :: ([Integer], [Integer]) -> ([Integer], [Integer])
maxMove ([], toList) = ([], toList)
maxMove (fromList, toList) = maxMove (doMove (fromList, toList))

