-- Project - cfg-2-cnf
-- Name - Peter Kapicak
-- Login - xkapic02
-- Date - 2021
module SimpleRules where
import GrammarParser
import Data.List
import Data.Set (Set, fromList)
import Data.Char

-- Function to get every simple rule from CFG
getSimpleRules :: [Rule] -> [Rule]
getSimpleRules [] = [] 
getSimpleRules (rule:rules) = 
    if isSimpleRule 
        then [rule] ++ getSimpleRules rules
        else getSimpleRules rules
    where isSimpleRule = length (rightSide rule) == 1 && isUpper (head (rightSide rule))

-- Function to create sets for each nonterminal following simple rules in grammar
-- Recursively searching in simple rules to depth. So set for first nonTerm will be find last 
-- and set for the last find nonTerm will be first.
getSet :: [Char] -> [[Char]] -> [Rule] -> [[Char]]
getSet n ns [] = ns
getSet n ns all@(rule:rules)
    | (leftSide rule) == n = getSet [head (rightSide rule)] ((rightSide rule) : ns) all 
    | (leftSide rule) /= n = getSet n ns rules

-- Function to get nonterminals for which sets need to be find
getNonTermsSet :: [Rule] -> [[Char]]
getNonTermsSet [] = []
getNonTermsSet [rule] = [leftSide rule]
getNonTermsSet (rule:rules) = (leftSide rule) : getNonTermsSet rules

-- Funtion to make pairs (nonTerm, nonTermSet)
makePairSets :: Grammar -> [([Char], [[Char]])]
makePairSets g = 
    [(x, getSet x [x] $ getSimpleRules (rules g)) | x <- nub $ getNonTermsSet (rules g)]

-- Function to find every right side of the rule which is needed to create new rules
getRightSide :: [Rule] -> [[Char]] -> [[Char]]
getRightSide [] n = []
getRightSide (rule:rules) n = 
    if (leftSide rule) `elem` n 
        then (rightSide rule) : (getRightSide rules n) 
        else getRightSide rules n 

-- Function to create rules by algorithm to remove simple rules from CFG
createRules :: ([Char], [[Char]]) -> Grammar -> [Rule]
createRules (n, ns) g = 
    [Rule n x | x <- getRightSide (rules g) ns, length x /= 1 || not (isUpper (head x))]

-- Create grammar with new rules without simple rules. 'GrammarCNF' is only for formatting purpose
createNewGrammar :: Grammar -> GrammarCNF
createNewGrammar g = 
    let newRules = concat $ map (`createRules` g) (makePairSets g)
        nonTermsCNF = [n : "" | n <- (nonTerminals g)]
    in GrammarCNF nonTermsCNF (terminals g) (startSymbol g) newRules
