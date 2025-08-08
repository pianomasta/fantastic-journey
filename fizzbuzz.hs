import Data.List (nubBy)

primes :: [Int]
primes = nubBy (((2 <=) .) . gcd) [2 ..]

main :: IO ()
-- These are your fuzzies.
fuzzies :: Int -> String
fuzzies 3 = "Fizz"
fuzzies 5 = "Buzz"
fuzzies 7 = "Fuzz"
fuzzies 11 = "Faz"
fuzzies 13 = "Bear"
fuzzies 17 = "Fries"
fuzzies 19 = "Fooz"
fuzzies 23 = "Fears"
fuzzies 29 = "Biz"
fuzzies 31 = "Bang"
fuzzies 37 = "Baz"
fuzzies 41 = "Booz"
fuzzies 43 = "Daz"
fuzzies 47 = "Dooz"

-- These are your nobs.
nobs :: [Int]
nobs = [3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47]
fizzbuzz :: Int -> String
fizzbuzz n
  | [] == proto = show n
  | otherwise = proto
  where proto = (concat . map fuzzies . filter (\nob -> mod n nob == 0)) nobs
-- This is all positive integers fizzbuzzed.
count :: String
count = (unwords . map fizzbuzz) [1 ..]
main = putStr count
