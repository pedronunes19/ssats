-- literals are represented directly as integers here
-- I'm too lazy to implement a parser but it can just be done on top of this
-- e.g. (a OR b) AND (¬a OR c) <==> [[1, 2], [-1, 3]]

import Data.List (nub, partition)
import Data.Set (fromList, toList, intersection, empty, difference, union)
import qualified Data.Set as S (map)

type Literal = Int  -- Positive for x, Negative for ¬x
type Clause = [Literal]  -- A clause is a list of literals
type CNFF = [Clause]  -- A CNF formula is a list of clauses
type Assignment = [(Literal, Bool)]  -- store variable assignment


-- function to remove duplicate literals 
removeDuplicates :: CNFF -> CNFF
removeDuplicates = map nub


-- auxiliary function to check if a clause contains both x and ¬x (therefore always true as clauses are disjunctions)
isTautology :: Clause -> Bool
isTautology clause = not $ intersection (fromList clause) (fromList (map negate clause)) == empty

-- function to remove tautological clauses
removeTautologies :: CNFF -> CNFF
removeTautologies = filter (not . isTautology)


-- function to check for unit clauses
findUnitClause :: CNFF -> Maybe Literal
findUnitClause [] = Nothing
findUnitClause (c:cs)
    | length c == 1 = Just (head c) -- return literal in unit clause
    | otherwise     = findUnitClause cs 

-- function to trim formula after finding unit clause
simplifyCNFF :: CNFF -> Literal -> CNFF
simplifyCNFF cnff lit =
    let (satisfied, remaining) = partition (elem lit) cnff -- get satisfied clauses
    in map (filter (/= -lit)) remaining  -- remove ¬lit from remaining clauses

unitPropagation :: CNFF -> Assignment -> (CNFF, Assignment)
unitPropagation cnff assignment =
    case findUnitClause cnff of
        Nothing     -> (cnff, assignment)  -- done
        Just lit    -> 
            let newCNFF = simplifyCNFF cnff lit
                newAssignment = (lit, True) : assignment
            in unitPropagation newCNFF newAssignment  -- recursively propagate


-- function to find pure literal instances
findPureLiterals :: CNFF -> [Literal]
findPureLiterals cnff =
    let allLits = concat cnff
        pos = fromList [l | l <- allLits, l > 0]
        neg = fromList [l | l <- allLits, l < 0]
        purePos = difference pos (S.map negate neg)
        pureNeg = difference neg (S.map negate pos)
    in toList (union purePos pureNeg)

pureLiteralElimination :: CNFF -> Assignment -> (CNFF, Assignment)
pureLiteralElimination cnff assignment =
    case findPureLiterals cnff of
        [] -> (cnff, assignment)
        pureLits ->
            let newCNFF = filter (not . any (`elem` pureLits)) cnff
                newAssign = [(lit, lit > 0) | lit <- pureLits] ++ assignment
            in pureLiteralElimination newCNFF newAssign



-- Example
---------------------------------------------------------------------
cnfFormula :: CNFF
cnfFormula = [[1], [2, -1], [3, 4], [-3, 5]]

assignment :: Assignment
assignment = []

main :: IO ()
main = do
    let cleanedCNF = removeTautologies (removeDuplicates cnfFormula)
    let (upCNFF, upAssignment) = unitPropagation cleanedCNF assignment
    putStrLn "Final CNF formula after unit propagation:"
    print upCNFF
    let (pleCNFF, pleAssignment) = pureLiteralElimination upCNFF upAssignment
    putStrLn "Final CNF formula after pure literal elimination:"
    print pleCNFF
    putStrLn "Final Assignments:"
    print pleAssignment

---------------------------------------------------------------------