-- Project - cfg-2-cnf
-- Name - Peter Kapicak
-- Login - xkapic02
-- Date - 2021
module Chomsky where

import GrammarParser
import Data.List
import Data.Char

-- Function to get every rule which does not satisfy CNF grammar
getNonCNFRules :: Grammar -> [Rule]
getNonCNFRules g = [rule | rule <- (rules g), length (rightSide rule) >= 2]

-- Function to create rules in CNF from i.e. S -> X_1...X_k
-- It can create also rule a' -> a if needed
createRules :: String -> String -> [Rule]
createRules left (r:right)
    | isLower r = Rule left ((r : "\'") ++ "<" ++ right ++ ">") : [createSingleRule r r]
    | isUpper r = Rule left ( r : "<" ++ right ++ ">") : []

-- Function contains word 'double' because creates rules in CNF from S->AA, S->aA, S->Aa, S->aa
-- It can create also rule a' -> a if needed
createDoubleRules :: String -> Char -> Char -> [Rule]
createDoubleRules left f s 
    | isUpper f && isUpper s = Rule left [f,s] : []
    | isUpper f = Rule left [f, s, '\''] : [createSingleRule s s]
    | isUpper s = Rule left [f, '\'', s] : [createSingleRule f f]
    | otherwise = Rule left [f, '\'', s, '\''] : [createSingleRule s s, createSingleRule f f]

-- Funtion to create type a' -> a of rule in CNF
-- Two functions above use this function
createSingleRule :: Char -> Char -> Rule
createSingleRule left right = Rule (left : "\'") [right]

-- Function which creates every rule in CNF what needed from one rule
createCNFRules :: Rule -> [Rule]
createCNFRules rule
    | length right == 2 && isNew = createDoubleRules newLeft (head right) (last right)
    | length right == 2 = createDoubleRules left (head right) (last right)
    | length right >= 3  && isNew =
        createCNFRules (Rule (tail right) (tail right)) ++ createRules newLeft right
    | length right >= 3 =
        createCNFRules (Rule (tail right) (tail right)) ++ createRules left right
    where left = leftSide rule
          right = rightSide rule
          isNew = length left > 1 && isAlphabetic left
          newLeft = "<" ++ left ++ ">"

-- Function to put together new generated rules in CNF and old rules which satisfy CNF
createNewRules :: Grammar -> [Rule]
createNewRules g = 
    let newRules = concat $ map createCNFRules (getNonCNFRules g)
        oldRules = [rule | rule <- (rules g), length (rightSide rule) == 1]
    in  newRules ++ oldRules

-- Helper funtion to know if left side of the rule can be wrapped into '<>'
isAlphabetic :: [Char] -> Bool
isAlphabetic [s] = if isLetter s then True else False
isAlphabetic (s:str) = if isLetter s then isAlphabetic str else False

-- Funtion to create output CNF grammar
createCNFGrammar :: Grammar -> GrammarCNF
createCNFGrammar g =
    let newRules = nub $ createNewRules g
        newNonTerms = nub $ [(leftSide rule) | rule <- newRules]
    in GrammarCNF newNonTerms (terminals g) (startSymbol g) newRules
