module Unif
where

import Data.List

infixr 3 @@      -- composition of substitutions
infix  4 |->     -- used to create bindings

data Term  
  = Var String
  | Fun String [Term]
    deriving (Eq, Show)

data Subst 
  = Fail
  | Sub [(String,Term)]
    deriving (Eq, Show)

-------------------------------------------------------------------------
-------- Substitution functions
-------------------------------------------------------------------------

-------- To apply a substitution to a term:

app :: Subst -> Term -> Term

app (Sub s) (Var x)
  = case lookup x s of
      Nothing -> Var x
      Just t0 -> t0
app (Sub s) (Fun x ts)
  = Fun x (map (app (Sub s)) ts)

app Fail t
  = error "Cannot apply Fail to a term\n"

-------- To create a single binding:

(|->) :: String -> Term -> (String,Term)

x |-> t = (x,t)

-------- To compose two substitutions:

(@@) :: Subst -> Subst -> Subst

Fail @@ _
  = Fail
_ @@ Fail
  = Fail
Sub [] @@ s
  = s
s @@ Sub []
  = s
s @@ Sub ((x,t) : xts)
  | s0 == Fail = Fail
  | otherwise  = Sub ((x, app s0 (Var x)) : s1)
    where
      s0 = unify (app s (Var x)) (app s t)
      (Sub s1) = s @@ Sub xts

-------------------------------------------------------------------------
-------- Variable utilities
-------------------------------------------------------------------------

-------- To extract a list of variables from a type
-------- (removing duplicates and sorting the result):

getVars :: Term -> [String]

getVars (Var v)
  = [v]
getVars (Fun f ts)
  = sort (nub (concat (map getVars ts))) 

-------------------------------------------------------------------------
-------- Unification
-------------------------------------------------------------------------

unify :: Term -> Term -> Subst

unify (Var x) (Var y)
  | x == y                           = Sub []
  | otherwise                        = Sub [x |-> Var y]
unify (Var x) t
  | x `elem` (getVars t)             = Fail
  | otherwise                        = Sub [x |-> t]
unify t (Var x)
  | x `elem` (getVars t)             = Fail
  | otherwise                        = Sub [x |-> t]
unify (Fun f ts) (Fun g us) 
  | f == g && length ts == length us = unifyLists ts us
  | otherwise = Fail

unifyLists :: [Term] -> [Term] -> Subst

unifyLists [] []
  = Sub []
unifyLists (t:ts) (u:us)
  | s0 == Fail = Fail
  | otherwise  = s1 @@ s0
    where
      s0 = unify t u
      s1 = unifyLists (map (app s0) ts) (map (app s0) us)

-------------------------------------------------------------------------
-------- Some expressions for testing
-------------------------------------------------------------------------

s = Sub [("x", Fun "g" [Var "z"])]

t1 = Fun "f" [Fun "a" [], Var "x"]
t2 = Fun "f" [Var "y", Fun "b" []]

app1 = app s t1
app2 = app s t2

u1 = unify t1 t2
u2 = unify app1 app2

f = Sub ["x" |-> Var "y"]
g = Sub ["y" |-> Var "x"]

-- Checking results from tute question 37:

al = Fun "h" [ Fun "f" [Var "x"]
             , Fun "g" [Var "y", Fun "f" [Var "x"]]
             , Var "y"
             ]

ar = Fun "h" [ Fun "f" [Var "u"]
             , Fun "g" [Var "v", Var "v"]
             , Var "u"
             ]

ua = unify al ar   -- Should fail

bl = Fun "h" [ Fun "f" [Fun "g" [Var "x", Var "y"]]
             , Var "y"
             , Fun "g" [Var "y", Var "y"]
             ]

br = Fun "h" [ Fun "f" [Var "u"]
             , Fun "g" [Fun "a" [], Var "v"]
             , Var "u"
             ]

ub = unify bl br

cl = Fun "h" [ Fun "g" [Var "x", Var "x"]
             , Fun "g" [Var "y", Var "z"]
             , Fun "g" [Var "y", Fun "f" [Var "z"]]
             ]

cr = Fun "h" [ Fun "g" [Var "u", Var "v"]
             , Fun "g" [Var "v", Var "u"]
             , Var "v"
             ]

uc = unify cl cr   -- Should fail

dl = Fun "h" [Var "v", Fun "g" [Var "v"], Fun "f" [Var "u", Fun "a" []]]
dr = Fun "h" [Fun "g" [Var "x"], Var "y", Var "x"]

ud = unify dl dr

el = Fun "h" [Fun "f" [Var "x", Var "x"], Var "y", Var "y", Var "x"]
er = Fun "h" [Var "v", Var "v", Fun "f" [Fun "a" [], Fun "b" []], Fun "a" []]

ue = unify el er   -- Should fail

