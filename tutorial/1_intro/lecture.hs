import Control.Exception (assert)

a = 1

b = 2

c = a + b

ff "0" = "1"
ff a = "0"

fact 0 = 1
fact n = n * fact (n - 1)

fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

fibonacciWithGuard n
  | n < 2 && n >= 0 = n
  | n >= 2 = fibonacciWithGuard (n - 1) + fibonacciWithGuard (n - 2)

first (x : _) = x

maax, miin :: Int
maax = maxBound
miin = minBound

reallyBig :: Integer
reallyBig = 2 ^ (2 ^ (2 ^ (2 ^ 2)))

numDigits :: Int
numDigits = length (show reallyBig)

d1, d2 :: Double
d1 = 4.5387
d2 = 6.2831e-4

b1, b2 :: Bool
b1 = True
b2 = False

c1, c2 :: Char
c1 = 'x'
c2 = 'あ'

s :: String
s = "Haskell"

-- hailstone 1 = 1
hailstone n
  | n == 1 = 1
  | even n = div n 2
  | otherwise = 3 * n + 1

fnWithMultipleArgs :: Int -> Int -> Int -> Int
fnWithMultipleArgs x y z = x * y * z

nums, rangeee, rangeeee2 :: [Integer]
nums = [1, 2, 3, 19]
rangeee = [1 .. 1000]
rangeeee2 = [2, 4 .. 100]

hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo [] = []
sumEveryTwo [x] = [x]
sumEveryTwo (x : (y : zs)) = (x + y) : sumEveryTwo zs

main = do
  -- print $ fibonacciWithGuard 10
  -- print [1, 2, 3, 4, 5, 6]
  -- print $ [1, 2, 3, 4, 5, 6] !! 2
  -- print $ [0 .. 6] !! 2
  -- print $ first [12 .. 22]
  -- print $ first "zyx"
  -- print reallyBig
  -- print numDigits
  print maax
  print miin
  print $ hailstone 11
  print $ fnWithMultipleArgs 6 6 6
  print $ ['0', 'w', '0'] == "0w0"
  print $ hailstoneSeq 11
  print $ sumEveryTwo [66, 6, 88, 55, 57]
