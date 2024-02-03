
-- Kattis problem: The Plank
-- Submission ID: 12850833

{-
    Problem: Determine the number of ways to construct a plank
    of length n, using smaller planks of lengths 1, 2 and 3.

    The answer is computed by adding the number of ways to build
    planks of length n - 1, n - 2, and n - 3.
 -}

build :: Integer -> [Integer]
build 0 = [1]
build 1 = [1, 1]
build 2 = [2, 1, 1]
build n = p : prev
    where
        p    = p' + p'' + p'''
        p'   = prev !! 0
        p''  = prev !! 1
        p''' = prev !! 2
        prev = build $ n - 1

main :: IO ()
main = do
    input <- getLine
    let ans = head . build $ read input :: Integer
    print ans
