-- (*) Find the number of elements of a list.
--
-- Example in Haskell:
--
-- Prelude> myLength [123, 456, 789]
-- 3
-- Prelude> myLength "Hello, world!"
-- 13

myLength :: (Eq a) => [a] -> Int
myLength xs = length xs

myLength1 :: (Eq a) => [a] -> Integer
myLength1 xs  | xs == []  = 0
              | otherwise = foldr1 (+) $ map (\_ -> 1) xs

myLength2 :: (Eq a) => [a] -> Integer
myLength2 xs  | xs == []  = 0
              | otherwise = foldr (\_ acc -> acc + 1) 0 xs

myLength3 :: (Eq a) => [a] -> Integer
myLength3 xs  | xs == []  = 0
              | otherwise = fst $ last $ zip [1..] xs

