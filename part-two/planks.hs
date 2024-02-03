
-- Kattis problem: The Plank
-- Submission ID: 12849824

planks :: Integer -> Integer
planks 0 = 1
planks n | n < 0 = 0
         | n > 0 = (planks (n - 1)) + (planks (n - 2)) + (planks (n - 3))

main :: IO ()
main = do
    input <- getLine
    let ans = planks (read input :: Integer)
    print ans
