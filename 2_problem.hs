-- (*) Find the last but one element of a list.
--
-- (Note that the Lisp transcription of this problem is incorrect.)
--
-- Example in Haskell:
--
-- Prelude> myButLast [1,2,3,4]
-- 3
-- Prelude> myButLast ['a'..'z']
-- 'y'

import Data.Maybe
import qualified Data.List

next2Last :: (Eq a) => [a] -> Maybe a
next2Last xs  | drop 1 xs == [] = Nothing
              | otherwise       = Just $ xs!!next2LastIdx
              where next2LastIdx = length xs - 2

next2Last1 :: (Eq a) => [a] -> Maybe a
next2Last1 xs  | drop 1 xs == [] = Nothing
               | otherwise       = Just $ (reverse xs)!!1

next2Last2 :: (Eq a) => [a] -> Maybe a
next2Last2 xs  | drop 1 xs == [] = Nothing
               | otherwise       = Just $ head $ drop num xs
               where num = length xs - 2

next2Last3 :: (Eq a) => [a] -> Maybe a
next2Last3 xs  | drop 1 xs == [] = Nothing
               | otherwise       = Just $ last $ init xs

