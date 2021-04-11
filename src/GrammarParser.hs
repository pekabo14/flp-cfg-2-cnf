-- Project - cfg-2-cnf
-- Name - Peter Kapicak
-- Login - xkapic02
-- Date - 2021
{-# LANGUAGE RecordWildCards #-}
module GrammarParser where
import Control.Applicative
import Text.Parsec (Parsec, parse,
        newline, alphaNum, string, char, satisfy, sepBy1, endBy, many1, upper, lower)
import Text.Parsec.String (Parser)
import Data.Char
import Data.Set
import Data.List

-- Stores Rule of CFG. Both sides separately 
data Rule = Rule { leftSide :: [Char]
                 , rightSide :: [Char]} deriving(Show, Eq)

-- Store CFG by definition from TIN Course
data Grammar = Grammar { nonTerminals :: [Char]
                       , terminals :: [Char]
                       , startSymbol :: Char
                       , rules :: [Rule]} deriving(Eq, Show)

-- Same grammar as above but for CNF it's needed strings for NonTerminals (i.e. <AB>)
-- Use only for purpose of store CNF grammar and format it.
data GrammarCNF = GrammarCNF { nonTerminalsCNF :: [[Char]]
                             , terminalsCNF :: [Char]
                             , startSymbolCNF :: Char
                             , rulesCNF :: [Rule]} deriving(Eq, Show)

-- Below are definitions of every part of CFG and its parts

nonTerminal :: Parser Char
nonTerminal = satisfy (isUpper)

nonTerminalsLists :: Parser [Char]
nonTerminalsLists = sepBy1 nonTerminal comma

terminal :: Parser Char
terminal = satisfy (isLower)

terminalsList :: Parser [Char]
terminalsList = sepBy1 terminal comma

rightSideRule :: Parser String
rightSideRule = many1 (terminal <|> nonTerminal)

leftSideRule :: Parser String
leftSideRule = many1 nonTerminal

rule :: Parser Rule
rule = Rule <$> leftSideRule <* arrow <*> rightSideRule

rulesList :: Parser [Rule]
rulesList = endBy rule newline

comma :: Parser Char
comma = char ','

arrow :: Parser String
arrow = string "->"

-- Parser for CFG, uses definitions above to parse every part properly
grammarParser :: Parser Grammar
grammarParser = Grammar <$> nonTerminalsLists <* newline
                        <*> terminalsList <* newline
                        <*> nonTerminal <* newline
                        <*> rulesList

validateRightSideRule :: String -> Grammar -> Bool
validateRightSideRule [] g = True
validateRightSideRule (x:xs) g = 
    if x `elem` (terminals g) || x `elem` (nonTerminals g) 
        then validateRightSideRule xs g 
        else False

checkDuplicties :: [Char] -> Bool
checkDuplicties [] = True
checkDuplicties (x:xs) = x `notElem` xs && checkDuplicties xs 

validate :: Grammar -> Bool
validate grammar@Grammar{..} = if allOK then True else False
    where 
        allOK = startSymbol `elem` nonTerminals
             && checkDuplicties nonTerminals
             && checkDuplicties terminals
             && all ((== 1) . (length . leftSide)) rules
             && all ((`isInfixOf` nonTerminals) . leftSide) rules
             && all ((`validateRightSideRule` grammar) . rightSide) rules