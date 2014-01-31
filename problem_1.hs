-- Find the last element of a list
-- Example:
--  Prelude> myLast [1,2,3,4]
--  4
--  Prelude> myLast ['x','y','z']
--  'z'

myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast xs = Just $ head . reverse $ xs

