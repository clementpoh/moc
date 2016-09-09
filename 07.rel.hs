module Rel
where

-- Some functions to manipulate binary relations, most taken from
-- Doets and Van Eijk.

import SetOrd

-- The type of binary relations:

type Rel a = Set (a,a)

-- The domain and range of a relation:

domR :: Ord a => Rel a -> Set a
domR (Set r)
  = fromList [x | (x,_) <- r]

ranR :: Ord a => Rel a -> Set a
ranR (Set r)
  = fromList [y | (_,y) <- r]

-- The identity relation and the total relation (over a given set):

idR :: Ord a => Set a -> Rel a
idR (Set xs)
  = Set [(x,x) | x <- xs]

totalR :: Set a -> Rel a
totalR (Set xs)
  = Set [(x,y) | x <- xs, y <- xs]

-- The inverse of a relation:

invR :: Ord a => Rel a -> Rel a
invR (Set [])
  = (Set [])
invR (Set ((x,y):r))
  = insert (y,x) (invR (Set r))

-- Membership of a given relation:

inR :: Ord a => Rel a -> (a,a) -> Bool
inR r (x,y)
  = member (x,y) r

-- The complement (relative to a given set) of a given relation:

complR :: Ord a => Set a -> Rel a -> Rel a
complR (Set xs) r
  = Set [(x,y) | x <- xs, y <- xs, not (r `inR` (x,y))]

-- Tests for reflexivity, irreflexitivity, transitivity:

isRefl :: Ord a => Set a -> Rel a -> Bool
isRefl set r
  = (idR set) `isSubsetOf` r

isIrrefl :: Ord a => Set a -> Rel a -> Bool
isIrrefl (Set xs) r
  = all (\ pair -> not (r `inR` pair)) [(x,x) | x <- xs]

isTrans :: Ord a => Rel a -> Bool
isTrans (Set [])
  = True
isTrans (Set s)
  = and [trans pair (Set s) | pair <- s]
    where
      trans (x,y) (Set r)
        = and [member (x,v) (Set r) | (u,v) <- r, u == y]

-- Exercises
sSymm :: Ord a => Rel a -> Bool
isSymm (Set [])
  = True
isSymm (Set ((x,y):pairs))
  | x == y    = isSymm (Set pairs)
  | otherwise = member (y,x) (Set pairs) &&
                isSymm (delete (y,x) (Set pairs))

-- The function composePair is a helper function for composeR.
-- Given a single pair (x,y) and relation r, it builds {(x,z) | r(y,z)}.

composePair :: Ord a => (a,a) -> Rel a -> Rel a
composePair (x,y) (Set [])
  = Set []
composePair (x,y) (Set ((u,z):s))
  | y == u    = insert (x,z) (composePair (x,y) (Set s))
  | otherwise = composePair (x,y) (Set s)

composeR :: Ord a => Rel a -> Rel a -> Rel a
composeR (Set []) _
  = (Set [])
composeR (Set ((x,y):s)) r
  = union (composePair (x,y) r) (composeR (Set s) r)

-- The reflexive transitive closure of r (over set s):

reflTransClosure :: Ord a => Set a -> Rel a -> Rel a
reflTransClosure set rel
  = union (idR set) (transClosure set rel)

transClosure :: Ord a => Set a -> Rel a -> Rel a
transClosure set rel
  = new
    where
      (old,new)         = until stable grow (idR set, rel)
      grow (oldR, newR) = (newR, union newR (composeR rel newR))
      stable (x,y)      = x == y
