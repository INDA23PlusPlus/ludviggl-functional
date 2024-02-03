
-- Kattis problem: Odd A's Even B's
-- Submission ID: 12850329

{-
    Problem: Compute the number of strings of A's and B's of a certain length
    where there are no consecutive even number of A's or odd number of B's.

    This method works by deriving recurrence relations between two functions,
    a(n) and b(n), denoting the number of strings of length n that begin with A and B respecitvely.

    The recurrence relations are:
        a(n) = a(n - 2) + b(n - 1) and
        b(n) = a(n - 2) + b(n - 2),
    with initial conditions
        a(1) = 1, a(2) = 0 and
        b(1) = 0, b(2) = 1.

    The values are computed dynamically by building a list of tuples (a(n), b(n))
    for increasing values of n. The answer is computed with a(n) + b(n).

    According to the assignment, the output should be printed modulo 10^9 + 7.
-}

build :: Int -> [(Int, Int)]
build 1 = [(1, 0)]
build 2 = [(0, 1), (1, 0)]
build n = (a, b) : prev
    where
        a = fst (prev !! 1) + snd (prev !! 0)
        b = fst (prev !! 1) + snd (prev !! 1)
        prev = build (n - 1)

count :: Int -> Int
count n = (fst first) + (snd first) where first = (build n) !! 0

proc_output :: Int -> Int
proc_output n = n `mod` 1000000007

main :: IO ()
main = do
    input <- getLine
    let ans = count (read input :: Int)
    print (proc_output ans)
