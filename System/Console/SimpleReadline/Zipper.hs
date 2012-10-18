-- |
-- Module      : System.Console.SimpleReadline.Zipper
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- a simple zipper implementation
--
module System.Console.SimpleReadline.Zipper
    where

data Zipper a = Zipper [a] [a]

zipPrev :: Zipper a -> Zipper a
zipPrev z@(Zipper p n)
    | null p    = z
    | otherwise = Zipper (tail p) (head p : n)

zipNext :: Zipper a -> Zipper a
zipNext z@(Zipper p n)
    | null n    = z
    | otherwise = Zipper (head n : p) (tail n)

zipAtHome :: Zipper a -> Zipper a
zipAtHome (Zipper p n) = Zipper [] (reverse p ++ n)

zipAtEnd :: Zipper a -> Zipper a
zipAtEnd (Zipper p n) = Zipper (reverse n ++ p) []

zipInsert :: [a] -> Zipper a -> Zipper a
zipInsert l (Zipper p n) = Zipper (reverse l ++ p) n

zipInit :: [a] -> Zipper a
zipInit l = Zipper [] l

zipToList :: Zipper a -> [a]
zipToList (Zipper p n) = reverse p ++ n

zipHasNext :: Zipper a -> Bool
zipHasNext (Zipper _ n) = not $ null n

zipLengthNext :: Zipper a -> Int
zipLengthNext (Zipper _ n) = length n

zipLengthPrev :: Zipper a -> Int
zipLengthPrev (Zipper p _) = length p
