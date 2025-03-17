-- literals are represented directly as integers here
-- I'm too lazy to implement a parser but it can just be done on top of this
-- e.g. (a OR b) AND (¬a OR c) <==> [[1, 2], [-1, 3]]

import Data.List (nub)
import Data.Set (fromList, intersection, empty)

type Literal = Int  -- Positive for x, Negative for ¬x
type Clause = [Literal]  -- A clause is a list of literals
type CNFF = [Clause]  -- A CNF formula is a list of clauses
type Assignment = [(Int, Bool)]  -- store variable assignment


-- function to remove duplicate literals 
removeDuplicates :: CNFF -> CNFF
removeDuplicates = map nub


-- auxiliary function to check if a clause contains both x and ¬x (therefore always true as clauses are disjunctions)
isTautology :: Clause -> Bool
isTautology clause = not $ intersection (fromList clause) (fromList (map negate clause)) == empty

-- function to remove tautological clauses
removeTautologies :: CNFF -> CNFF
removeTautologies = filter (not . isTautology)



-- Example
cnfFormula :: CNF
cnfFormula = [[1, 2], [-1, 3], [-2, -3]]