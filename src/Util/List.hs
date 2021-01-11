module Util.List (
    spanLimited,
    breakLimited
) where

import Data.Char (isSpace)

spanLimited :: (a -> Bool) -> Int -> [a] -> ([a], [a])
spanLimited _    _      [] = ([], []) 
spanLimited pred maxLen (c : rest)
    | maxLen <= 0 = ([], c : rest)
    | pred c      = let (f, t) = spanLimited pred (maxLen - 1) rest in (c : f, t)
    | otherwise   = ([], c : rest)

breakLimited :: (a -> Bool) -> Int -> [a] -> ([a], [a])
breakLimited p = spanLimited (fmap not p)