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

data Zipper a = Zipper Int [a] Int [a]

zipPrev :: Zipper a -> Zipper a
zipPrev z@(Zipper plen p nlen n)
    | null p    = z
    | otherwise = Zipper (plen-1) (tail p) (nlen+1) (head p : n)

zipNext :: Zipper a -> Zipper a
zipNext z@(Zipper plen p nlen n)
    | null n    = z
    | otherwise = Zipper (plen+1) (head n : p) (nlen-1) (tail n)

zipAtHome :: Zipper a -> Zipper a
zipAtHome (Zipper plen p nlen n) = Zipper 0 [] (nlen + plen) (reverse p ++ n)

zipAtEnd :: Zipper a -> Zipper a
zipAtEnd (Zipper plen p nlen n) = Zipper (plen + nlen) (reverse n ++ p) 0 []

zipInsert :: [a] -> Zipper a -> Zipper a
zipInsert l (Zipper plen p nlen n) = Zipper (plen + length l) (reverse l ++ p) nlen n

zipInit :: [a] -> Zipper a
zipInit l = Zipper 0 [] (length l) l

zipDelPrev :: Int -> Zipper a -> Zipper a
zipDelPrev nb z@(Zipper plen p nlen n)
    | nb >= plen = Zipper 0 [] nlen n
    | otherwise  = Zipper (plen - nb) (drop nb p) nlen n

zipDelNext :: Int -> Zipper a -> Zipper a
zipDelNext nb z@(Zipper plen p nlen n)
    | nb >= nlen = zipDelToEnd z
    | otherwise  = Zipper plen p (nlen - nb) (drop nb n)

zipDelToEnd :: Zipper a -> Zipper a
zipDelToEnd (Zipper plen p _ _) = Zipper plen p 0 []

zipToList :: Zipper a -> [a]
zipToList (Zipper _ p _ n) = reverse p ++ n

zipHasNext :: Zipper a -> Bool
zipHasNext (Zipper _ _ _ n) = not $ null n

zipLengthNext :: Zipper a -> Int
zipLengthNext (Zipper _ _ nlen _) = nlen

zipLengthPrev :: Zipper a -> Int
zipLengthPrev (Zipper plen _ _ _) = plen

safeDrop n l
	| null l    = []
	| n == 0    = l
	| otherwise = safeDrop (n-1) (tail l)
