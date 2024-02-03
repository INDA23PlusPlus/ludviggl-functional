
-- Fibonacci
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- Reverse list of ints
rev :: [Int] -> [Int]
rev [] = []
rev (head : tail) = (rev tail) ++ [head]

-- Find median of length of words in a list
insert :: Int -> [Int] -> [Int]
insert item [] = [item]
insert item (head : tail) | item <= head = item : (head : tail)
                          | item > head = head : insert item tail

sorted_lengths :: [String] -> [Int]
sorted_lengths [] = []
sorted_lengths (head : tail) = insert (length head) (sorted_lengths tail)

median :: [String] -> Float
median list | len == 0 = 0.0
            | odd len = fromIntegral (sorted !! middle)
            | even len = (fromIntegral (sorted !! middle + sorted !! (middle - 1))) / 2
    where
        len = length list
        middle = len `div` 2
        sorted = sorted_lengths list
