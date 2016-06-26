{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the equality of a pair of pegs
pairPegsEq :: (Peg, Peg) -> Bool
pairPegsEq (x,y) = x == y

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches x y = length $ filter id $ map pairPegsEq $ zip x y

-- Exercise 2 -----------------------------------------

-- Counts occurrences of a color in a code
countColor :: Peg -> Code -> Int
countColor color code = length $ filter (== color) code

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors code = [ countColor c code | c <- colors ]

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches x y = sum $ map (uncurry min) $ zip (countColors x) (countColors y)

-- Exercise 3 -----------------------------------------

-- Count number of matches between number of matches and exact matches
nonExactMatches :: Code -> Code -> Int
nonExactMatches xs ys = matches xs ys - exactMatches xs ys

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove xs ys = Move ys x y
    where x = exactMatches xs ys
          y = nonExactMatches xs ys

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move guess xs ys) guess' = xs == x && ys == y
    where x = exactMatches guess guess'
          y = nonExactMatches guess guess'

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes = undefined

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes = undefined

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
