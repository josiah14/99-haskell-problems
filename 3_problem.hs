-- (*) Find the K'th element of a list. The first element in the list is number 1.
--
-- Example:
--
-- * (element-at '(a b c d e) 3)
-- c
-- Example in Haskell:
--
-- Prelude> elementAt [1,2,3] 2
-- 2
-- Prelude> elementAt "haskell" 5
-- 'e'

elementAt :: (Eq a) => [a] -> Int -> Maybe a
elementAt xs n | null xs || not isIndexValid  = Nothing
               | otherwise                    = Just $ (!!) xs $ n - 1
               where isIndexValid = n <= length xs && n > 0

elementAt1 :: (Eq a) => [a] -> Int -> Maybe a
elementAt1 xs n | null xs || not isIndexValid = Nothing
                | length xs == n              = Just $ last xs
                | otherwise                   = elementAt1 (init xs) n
               where isIndexValid = n <= length xs && n > 0

elementAt2 :: (Eq a) => [a] -> Int -> Maybe a
elementAt2 xs n | null xs || not isIndexValid = Nothing
                | length xs == n              = Just $ last xs
                | otherwise                   = elementAt2 (reverse . tail . reverse $ xs) n
               where isIndexValid = n <= length xs && n > 0

elementAt3 :: (Eq a) => [a] -> Int -> Maybe a
elementAt3 xs n | null xs || not isIndexValid = Nothing
                | otherwise                   = Just $ head . drop (n - 1) $ xs
               where isIndexValid = n <= length xs && n > 0

element4 (Eq a) => [a] -> Int -> Maybe a
element4 xs n | null xs || not isIndexValid = Nothing
              | otherwise                   = Just $ head $ foldr (id) xs $ replicate (n - 2) tail
              where isIndexValid = n <= length xs && n > 0

