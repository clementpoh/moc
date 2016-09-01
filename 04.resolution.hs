module Resolution 
where

import Data.List
import Data.Char

data Literal
  = Pos Char | Neg Char
    deriving (Eq, Ord, Show)

type Clause
  = [Literal]

type Formula
  = [Clause]

reduce :: Formula -> Formula
-- Remove duplicates, both duplicate literals within clauses,
-- and duplicate clauses.
reduce f
  = nub (map nub f)

switch_sign :: Literal -> Literal
-- Negate a literal.
switch_sign (Pos c) 
  = Neg c
switch_sign (Neg c) 
  = Pos c

resolvents :: Clause -> Clause -> [Clause]
-- Find all possible resolvents of two clauses.
resolvents c1 c2
  = [resolve_on lit c1 c2 
    | lit <- c1, lit' <- c2 
    , lit' == switch_sign lit
    ]

non_tautology :: Clause -> Bool
non_tautology []
  = True
non_tautology (lit:lits)
  = (switch_sign lit `notElem` lits) && (non_tautology lits)

resolve_on :: Literal -> Clause -> Clause -> Clause
-- resolve_on lit c1 c2 assumes that lit is known to be in c1
-- and its negation is known to be in c2.  It resolves on lit
-- and removes any duplicates in the resolvent.
resolve_on lit c1 c2
  = nub (delete lit c1 ++ delete (switch_sign lit) c2)

next_generation :: (Formula, Formula) -> Formula
-- Given a list S of clauses, produce all possible resolvents
-- of c1 and c2, with c1,c2 in S.
next_generation (old, recent)
  = nub (offspring_of_mixed ++ offspring_of_recent)
    where
      offspring_of_mixed 
        = concat [resolvents c1 c2 | c1 <- old, c2 <- recent]
      offspring_of_recent 
        = concat [resolvents c1 c2 | c1 <- recent, c2 <- recent]
   
evolve :: (Formula, Formula) -> (Formula, Formula)
-- Given a collection (old, young) of clauses, where the `old'
-- collection already is closed under resolution, and `young'
-- are the recently produced resolvents (offspring) of `old', 
-- resolve clauses in `old' against the new clauses in `young'.
-- Also promote the current `young' clauses to achieve the status
-- of `old'.
evolve (old, young) 
  = (population, next_gen)
    where
      population = nub (old ++ young)
      next_gen = nub (next_generation (old, young) \\ old)

unsatisfiable :: Formula -> Bool
-- Keep evolving new generations and let them take part in the
-- resolution process.  The process stops when no new resolvents
-- can be found, or when the empty clause is produced.
unsatisfiable f
  = not (null latest)
    where
      (older, latest) = until finished evolve ([], f)
      finished (old, young) = null young || [] `elem` young

-------------------------------------------------------------------------
--
-- The function translate is there so that it isn't necessary to
-- write the awkward elements of type Formula out.  Instead we use
-- a string representation, with `$' signifying the start of a
-- clause, and `-' standing for negation.  For example,
--
--                         "$A-BC$-A-D$BCD" 
-- translates to
--
-- [[Pos 'A',Neg 'B',Pos 'C'],[Neg 'A',Neg 'D'],[Pos 'B',Pos 'C',Pos 'D']]
--
-- With that convention, "" denotes the empty formula (true) and 
-- "$" denotes the formula consisting of an empty clause (false).
--

translate :: String -> Formula
translate s
  = tr (reverse s) [] []

tr :: String -> Clause -> Formula -> Formula
tr [] _ cstk
  = cstk
tr ('$':s) lstk cstk
  = tr s [] (lstk : cstk)
tr ('-':s) (Pos c : lstk) cstk
  = tr s (Neg c : lstk) cstk
tr (c:s) lstk cstk
  | isUpper c = tr s (Pos c : lstk) cstk
  | otherwise = error ("Illegal input character: " ++ [c])

-------------------------------------------------------------------------
--
-- Testing
--

ex0
  = [ [Pos 'A',Neg 'B',Pos 'C']
    , [Neg 'A',Neg 'D']
    , [Pos 'B',Pos 'C',Pos 'D']
    ]

tr_test
  = translate "$A-BC$-A-D$BCD" == ex0


ex1
  = [ [Pos 'A', Pos 'B']
    , [Neg 'A', Neg 'B']
    , [Neg 'B', Pos 'C', Pos 'D']
    ]

test1
  = not (unsatisfiable ex1)

q21                              -- From tute question 21
  = [ [Pos 'A',Pos 'B',Neg 'C']
    , [Neg 'A']
    , [Pos 'A',Pos 'B',Pos 'C']
    , [Pos 'A',Neg 'B']
    ]

test2
  = unsatisfiable q21

all_tests
  = tr_test && test1 && test2

