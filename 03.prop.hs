module Prop 
where

infix 1 ==>

(==>) :: Bool -> Bool -> Bool
x ==> y 
  = (not x) || y

infix 1 <=>

(<=>) :: Bool -> Bool -> Bool
x <=> y 
  = x == y 

infixr 2 <+>

(<+>) :: Bool -> Bool -> Bool
x <+> y 
  = x /= y 

p = True
q = False

-- What's the difference between f1 and f2 below?

f1     
  = (not p) && (p ==> q) <=> not (q && (not p))
f2 p q 
  = (not p) && (p ==> q) <=> not (q && (not p))

valid1 :: (Bool -> Bool) -> Bool
valid1 f 
  = (f True) && (f False)

excluded_middle :: Bool -> Bool
excluded_middle p 
  = p || not p

valid2 :: (Bool -> Bool -> Bool)  -> Bool
valid2 f 
  = (f True True) && (f True False) && 
    (f False True) && (f False False)

valid3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
valid3 f 
  = and [f p q r | p <- [True,False], 
                   q <- [True,False], 
                   r <- [True,False]] 

valid4 :: (Bool -> Bool -> Bool -> Bool -> Bool) -> Bool
valid4 f 
  = and [f p q r s | p <- [True,False], 
                     q <- [True,False], 
                     r <- [True,False], 
                     s <- [True,False]] 

-- Check whether these formulas are valid:

f3 p q 
  = p ==> (q ==> p)
f4 p q 
  = (p ==> q) ==> p

logEquiv1 :: (Bool -> Bool) -> (Bool -> Bool) -> Bool
logEquiv1 f1 f2 
  = (f1 True  <=> f2 True) && (f1 False <=> f2 False) 

logEquiv2 :: (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> Bool
logEquiv2 f1 f2 
  = and [f1 p q <=> f2 p q | p <- [True,False], q <- [True,False]]

logEquiv3 :: (Bool -> Bool -> Bool -> Bool) ->
                 (Bool -> Bool -> Bool -> Bool) -> Bool
logEquiv3 f1 f2 
  = and [f1 p q r <=> f2 p q r | p <- [True,False], 
                                 q <- [True,False], 
                                 r <- [True,False]] 

-- Identify logical equivalences amongst these:

f5 p q 
  = p 
f6 p q 
  = (p <+> q) <+> q
f7 p q 
  = p <=> ((p <+> q) <+> q)

-- Expected equivalences:

test01 = logEquiv1 id (\ p -> not (not p))
test02 = logEquiv1 id (\ p -> p && p) 
test03 = logEquiv1 id (\ p -> p || p) 

test04 = logEquiv2 (\ p q -> p ==> q) 
                   (\ p q -> not p || q)
test05 = logEquiv2 (\ p q -> not (p ==> q)) 
                   (\ p q -> p && not q)
test06 = logEquiv2 (\ p q -> not p ==> not q) 
                   (\ p q -> q ==> p)
test07 = logEquiv2 (\ p q -> p ==> not q) 
                   (\ p q -> q ==> not p)
test08 = logEquiv2 (\ p q -> not p ==> q) 
                   (\ p q -> not q ==> p)
test09 = logEquiv2 (\ p q -> p <=> q) 
                   (\ p q -> (p ==> q) && (q ==> p))
test10 = logEquiv2 (\ p q -> p <=> q) 
                   (\ p q -> (p && q) || (not p && not q))
test11 = logEquiv2 (\ p q -> p && q) 
                   (\ p q -> q && p)
test12 = logEquiv2 (\ p q -> p || q) 
                   (\ p q -> q || p)
test13 = logEquiv2 (\ p q -> not (p && q)) 
                   (\ p q -> not p || not q)
test14 = logEquiv2 (\ p q -> not (p || q)) 
                   (\ p q -> not p && not q)
test15 = logEquiv3 (\ p q r -> p && (q && r)) 
                   (\ p q r -> (p && q) && r)
test16 = logEquiv3 (\ p q r -> p || (q || r)) 
                   (\ p q r -> (p || q) || r)
test17 = logEquiv3 (\ p q r -> p && (q || r)) 
                   (\ p q r -> (p && q) || (p && r))
test18 = logEquiv3 (\ p q r -> p || (q && r)) 
                   (\ p q r -> (p || q) && (p || r))

