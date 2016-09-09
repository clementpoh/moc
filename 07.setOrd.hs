module SetOrd ( Set(..)
              , empty
              , isEmpty
              , member
              , isSubsetOf
              , insert
              , delete
              , fromList
              , toList
              )
where

-- This module implements sets as ordered lists without duplicates.
-- The implementation is mainly for instructional purposes; a better
-- set implementation (using size balanced binary trees) is available
-- through the Data.Set library.

-- import Data.List (sort)

-- `newtype' is semantically the same as `data'

newtype Set a
  = Set [a]
    deriving (Eq,Ord)

-- We want to make the set type an instance of Show, so that we can
-- print out sets.

instance (Show a) => Show (Set a)
  where
    showsPrec _ (Set s) str = showSet s str

showSet [] str
  = showString "{}" str
showSet (x:xs) str
  = showChar '{' ( shows x ( showl xs str))
    where
      showl []     str = showChar '}' str
      showl (x:xs) str = showChar ',' (shows x (showl xs str))

empty :: Set a
empty
  = Set []

isEmpty :: Set a -> Bool
isEmpty (Set s)
  = Prelude.null s

member :: (Ord a) => a -> Set a -> Bool
member x (Set s)
  = elem x (takeWhile (<= x) s)

isSubsetOf :: (Ord a) => Set a -> Set a -> Bool
isSubsetOf (Set []) _
  = True
isSubsetOf (Set (x:xs)) set
  = (member x set) && (Set xs) `isSubsetOf` set

insert :: (Ord a) => a -> Set a -> Set a
insert x (Set s)
  = Set (insertList x s)
    where
      insertList x []
        = [x]
      insertList x ys@(y:ys')
        = case compare x y of
            GT -> y : insertList x ys'
            EQ -> ys
            _  -> x : ys

delete :: Ord a => a -> Set a -> Set a
delete x (Set s)
  = Set (deleteList x s)
    where
      deleteList x []
        = []
      deleteList x ys@(y:ys')
        = case compare x y of
            GT -> y : deleteList x ys'
            EQ -> ys'
            _  -> ys

fromList :: Ord a => [a] -> Set a
fromList []
  = Set []
fromList (x:xs)
  = insert x (fromList xs)

toList :: Set a -> [a]
toList (Set s)
  = s

-- Exercises
import Data.List (sort)    -- used in powerset (is there a nicer way?)

union :: Ord a => Set a -> Set a -> Set a
union (Set s1) (Set s2)
  = fromList (s1 ++ s2)

intersection :: Ord a => Set a -> Set a -> Set a
intersection (Set s1) (Set s2)
  = Set [x | x <- s1, x `elem` s2]

powerset :: Ord a => Set a -> Set (Set a)
powerset (Set s)
  = Set (sort [fromList xs | xs <- powerlist s])

powerlist :: [a] -> [[a]]
powerlist []
  = [[]]
powerlist (x:xs)
  = (powerlist xs) ++ (map (x:) (powerlist xs))

