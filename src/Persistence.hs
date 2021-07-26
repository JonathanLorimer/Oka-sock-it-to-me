{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Persistence where

import Data.Maybe
import Prelude hiding (lookup)

-- Exercise 2.1
suffix :: [a] -> [[a]]
suffix [] = [[]]
suffix (a : as) = (a : as) : suffix as

{-

[1][] -> [2][] -> [3][] -> [4][] -> [ ]
 ^        ^        ^        ^        ^
 |        |        |        |        |
[*][] -> [*][] -> [*][] -> [*][] -> [*][] -> [ ]

-}

-- Test
---- $> suffix [1,2,3,4]

-- Exercise 2.2
memberBetterWorstCase :: Ord a => Tree a -> a -> Bool
memberBetterWorstCase Leaf x = False
memberBetterWorstCase t@(Branch l e r) x = go e t x
  where
    go :: Ord a => a -> Tree a -> a -> Bool
    go candidate Leaf x = candidate == x
    go candidate (Branch l e r) x =
        if x < e then go candidate l x else go e r x

-- Test
---- $> memberBetterWorstCase bstExample 'g'

-- Exercise 2.3
betterInsert :: Ord a => Tree a -> a -> Tree a
betterInsert t x =
    fromMaybe t (go t)
  where
    go Leaf = Just (Branch Leaf x Leaf)
    go (Branch l v r)
        | x < v = fmap (\t' -> Branch t' v r) (go l)
        | x > v = fmap (Branch l v) (go r)
        | otherwise = Nothing

-- Exercise 2.4
betterBetterInsert :: Ord a => Tree a -> a -> Tree a
betterBetterInsert Leaf x = Branch Leaf x Leaf
betterBetterInsert t@(Branch l v r) x =
    fromMaybe t (go v t)
  where
    go candidate Leaf =
        if candidate == x
            then Nothing
            else Just (Branch Leaf x Leaf)
    go candidate (Branch l v r)
        | x < v = fmap (\t' -> Branch t' v r) (go candidate l)
        | otherwise = fmap (Branch l v) (go v r)

-- Exercise 2.5

-- a.
complete :: Ord a => a -> Int -> Tree a
complete x 0 = leaf x
complete x n = Branch t x t where t = complete x (n - 1)

-- b.
extendedComplete :: Ord a => a -> Int -> Tree a
extendedComplete x m = fst $ create2 x m
  where
    create2 x 0 = let e = Leaf in (e, Branch e x e)
    create2 x m =
        let (_, tPlusOne) = create2 x (m -1)
            mDiv2 = div m 2
            t' = fst $ create2 x mDiv2
            t'' = fst $ create2 x (mDiv2 + 1)
         in if odd m
                then (tPlusOne, Branch t'' x t')
                else (tPlusOne, Branch t' x t')

-- Exercise 2.6
data UBMap (c :: * -> *) k v = UBMap
    { emptyMap :: c (k, v)
    , bind :: c (k, v) -> k -> v -> c (k, v)
    , lookup :: c (k, v) -> k -> Maybe v
    }

ubMap :: Ord k => UBMap Tree k v
ubMap =
    UBMap
        { emptyMap = Leaf
        , bind = mapInsert
        , lookup = mapLookup
        }
  where
    mapInsert :: Ord k => Tree (k, v) -> k -> v -> Tree (k, v)
    mapInsert Leaf k v = Branch Leaf (k, v) Leaf
    mapInsert t@(Branch l x r) k v =
        fromMaybe t (go x t)
      where
        go candidate Leaf =
            if fst candidate == k
                then Nothing
                else Just (Branch Leaf (k, v) Leaf)
        go candidate (Branch l x' r)
            | k < fst x' = fmap (\t' -> Branch t' x' r) (go candidate l)
            | otherwise = fmap (Branch l x') (go x' r)

    mapLookup :: Ord k => Tree (k, v) -> k -> Maybe v
    mapLookup Leaf x = Nothing
    mapLookup t@(Branch l e r) x = go e t x
      where
        go :: Ord k => (k, v) -> Tree (k, v) -> k -> Maybe v
        go candidate Leaf x = if fst candidate == x then Just $ snd candidate else Nothing
        go candidate (Branch l e r) x =
            if x < fst e then go candidate l x else go e r x

{-----------------------------
          Helpers
------------------------------}

data Tree a where
    Leaf :: Tree a
    Branch :: Tree a -> a -> Tree a -> Tree a

data UBSet (c :: * -> *) a = UBSet
    { emptySet :: c a
    , member :: c a -> a -> Bool
    , insert :: c a -> a -> c a
    }

ubset1 :: Ord a => UBSet Tree a
ubset1 =
    UBSet
        { emptySet = Leaf
        , member = member
        , insert = insert
        }
  where
    member Leaf _ = False
    member (Branch l e r) x =
        if
                | x < e -> member l x
                | x > e -> member r x
                | otherwise -> True

    insert Leaf e = Branch Leaf e Leaf
    insert t@(Branch l e r) x =
        if
                | x < e -> Branch (insert l x) e r
                | x > e -> Branch l e (insert r x)
                | otherwise -> t

leaf :: Ord a => a -> Tree a
leaf a = Branch Leaf a Leaf

bstExample = Branch (Branch (leaf 'a') 'b' (leaf 'c')) 'd' (Branch (leaf 'f') 'g' (leaf 'h'))
