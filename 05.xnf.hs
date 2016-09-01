module XNF
where

import Data.List

-- Use a list-of-lists representation for formulas in XNF.
-- Note that `nub' from Data.List removes duplicates from a list.

type Formula  = [Monomial]
type Monomial = [String]

-- Produce a monomial in canonical form.

mkMonomial :: [String] -> [String]
mkMonomial literals
  = sort (nub literals)

-- Produce a polynomial in canonical form.
-- Assume that each monomial is already in canonical form,
-- that is, sorted, and free of duplicates.

mkXNF :: [Monomial] -> [Monomial]
mkXNF monomials
  = monomials      -- <---- A stub; this needs to be fixed

-- Also define the following constants and functions:

myFalse :: Formula                -- XNF representation of false

myTrue :: Formula                 -- XNF representation of true

myLiteral :: String -> Formula    -- Given propositional letter P,
                                  -- myLiteral "P" generates the 
                                  -- representation of formula P

myAnd :: Formula -> Formula -> Formula
myAnd :: Formula -> Formula -> Formula
myAnd f1 f2
  = mkXNF [mkMonomial (m1 ++ m2) | m1 <- f1, m2 <- f2]

myOr :: Formula -> Formula -> Formula

myNand :: Formula -> Formula -> Formula

myNor :: Formula -> Formula -> Formula

myXor :: Formula -> Formula -> Formula

myBiim :: Formula -> Formula -> Formula

myNot :: Formula -> Formula

