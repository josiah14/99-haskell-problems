-- (*) Reverse a list
--
-- Example in Haskell:
--
-- Prelude> myReverse "A man, a plan, a canal, panama!"
-- "!amanap ,lanac a ,nalp a ,nam A"
--
-- Prelude> myReverse [1,2,3,4]
-- [4,3,2,1]
--

-- helper functions
middle :: [a] -> [a]
middle = tail . init

myReverse :: [a] -> [a]
myReverse xs = reverse xs

myReverse0 :: [a] -> [a]
myReverse0 xs = case xs of
                  [_] -> xs
                  _   -> (last xs):(myReverse0 $ init xs)

myReverse1 :: [a] -> [a]
myReverse1 xs | null xs || (null . tail) xs = xs
              | otherwise                   = (last xs):concat [myReverse1 $ middle xs, [head xs]]

myReverse2 :: [a] -> [a]
myReverse2 xs | null xs || (null . tail) xs = xs
              | otherwise                   = (last xs):((myReverse2 $ middle xs) ++ [head xs])

myReverse3 :: [a] -> [a]
myReverse3 = foldl (flip (:)) []

myReverse4 :: [a] -> [a]
myReverse4 = reverse' []
  where
    reverse' reversed list  = case list of
                               []     -> reversed
                               (y:ys) -> reverse' (y:reversed) ys

