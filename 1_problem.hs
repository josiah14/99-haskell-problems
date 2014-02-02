-- Find the last element of a list
-- Example:
--  Prelude> myLast [1,2,3,4]
--  4
--  Prelude> myLast ['x','y','z']
--  'z'

import Data.Maybe
import qualified Data.List

myLast :: (Eq a) => [a] -> Maybe a
myLast xs
  | null xs   = Nothing
  | otherwise = Just $ head . reverse $ xs

myLast1 :: (Eq a) => [a] -> Maybe a
myLast1 xs
  | null xs   = Nothing
  | otherwise = Just $ last xs

myLast2 :: (Eq a) => [a] -> Maybe a
myLast2 xs
  | null xs   = Nothing
  | otherwise = Just $ xs!!lastIdx
  where lastIdx = length xs - 1

myLast3 :: (Eq a) => [a] -> Maybe a
myLast3 xs
  | null xs   = Nothing
  | otherwise = Just $ foldl1 (curry snd) xs

