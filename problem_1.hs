-- Find the last element of a list
-- Example:
--  Prelude> myLast [1,2,3,4]
--  4
--  Prelude> myLast ['x','y','z']
--  'z'

import Data.Maybe

myLast :: (Eq a) => [a] -> Maybe a
myLast xs
  | xs == []  = Nothing
  | otherwise = Just $ head . reverse $ xs

