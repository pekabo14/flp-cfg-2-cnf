-- Project - cfg-2-cnf
-- Name - Peter Kapicak
-- Login - xkapic02
-- Date - 2021
module Main (main)  where

import System.Environment
import System.IO
import Control.Monad
import Text.Parsec
import Data.Char

import GrammarParser
import SimpleRules
import Chomsky

-- Store command line arguments
data CmdLineArgs = CmdLineArgs{ fileName :: String
                              , readInput :: Bool
                              , simpleRules :: Bool
                              , transformToCnf :: Bool} deriving (Show)

-- Function to validation command line arguments combinations
validateArgs :: [String] -> CmdLineArgs
validateArgs ["-i"] = CmdLineArgs "" True False False
validateArgs ["-i", fileName] = CmdLineArgs fileName True False False
validateArgs ["-1"] = CmdLineArgs "" False True False
validateArgs ["-1", fileName] = CmdLineArgs fileName False True False
validateArgs ["-2"] = CmdLineArgs "" False False True
validateArgs ["-2", fileName] = CmdLineArgs fileName False False True
validateArgs [] = error "bkg-2-cnf -i|-1|-2 [filename]"
validateArgs _ = error "bkg-2-cnf -i|-1|-2 [filename]"

-- Function to convert Grammar from input to CNF grammar
fromGrammarToCNF :: Grammar -> GrammarCNF
fromGrammarToCNF g =
    let nonTermsCNF = [n : "" | n <- (nonTerminals g)]
    in GrammarCNF nonTermsCNF (terminals g) (startSymbol g) (rules g)

-- Function to corretly format terminal and nonterminals to output with adding comma between
insertComma :: [String] -> String
insertComma [] = []
insertComma [c] = c
insertComma (s:str) = s ++ "," ++ insertComma str

-- Function to format grammar to String
formatGrammar :: GrammarCNF -> String
formatGrammar g =
    let nonT = (insertComma (nonTerminalsCNF g)) ++ "\n"
        t = (insertComma $ [term : "" | term <- (terminalsCNF g)]) ++ "\n"
        startS = (startSymbolCNF g) : "\n"
        rs = concat $ [(leftSide rule) ++ "->" ++ (rightSide rule) ++ "\n" | rule <- (rulesCNF g)]
    in nonT ++ t ++ startS ++ rs

-- Function to check if algorithm can transform CFG to CNF 
checkSimpleRules :: [Rule] -> Bool
checkSimpleRules [] = True
checkSimpleRules (rule:rules)
    | length (rightSide rule) == 1 && isUpper (head (rightSide rule)) = 
        error "Remove simple rules."
    | otherwise = checkSimpleRules rules

-- Funtion to choose correct algorithm or just pass correct grammat from input
chooseOption :: CmdLineArgs -> Grammar -> GrammarCNF
chooseOption args g
    | readInput args = fromGrammarToCNF g
    | simpleRules args = createNewGrammar g
    | transformToCnf args && checkSimpleRules (rules g) = createCNFGrammar g

main = do 
    args <- getArgs
    let cmdLineArgs = validateArgs args

    content <- if fileName cmdLineArgs == "" then getContents else readFile (fileName cmdLineArgs)

    let g = case (parse grammarParser "" content) of
                Left err -> error (show err)
                Right gr -> gr
    if validate g
        then 
            let newG = chooseOption cmdLineArgs g
            in putStr (formatGrammar newG)
        else error "Invalid Grammar!"
