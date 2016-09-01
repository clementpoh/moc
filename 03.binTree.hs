module BinTree 
where

data BinTree a 
  = Void | Node a (BinTree a) (BinTree a)
    deriving (Eq, Show)

buildtree :: [a] -> BinTree a
buildtree []
  = Void
buildtree xs
  = Node midval (buildtree as) (buildtree (tail bs))
    where
      halflength = length xs `div` 2
      midval     = xs !! halflength
      (as,bs)    = splitAt halflength xs

test1 :: BinTree Int
test1
  = buildtree [7,2,1,9,4,3,6,5,10,8]
