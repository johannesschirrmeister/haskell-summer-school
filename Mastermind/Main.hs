module Main where

import System.Random  -- for randoms
import System.IO      -- for hFlush

type Row = [Int]
type Guess = Row
type Solution = Row

colors = 6
width  = 4

-- A function that indicates places on which you have to work:
tODO :: a -> a
tODO = id

-- This is the complete main function. It just initializes the
-- game by generating a solution and then calls the game loop.
main :: IO ()
main =
  do
    s <- generateSolution -- initialization
    loop s                -- game loop

-- The following function is given. It generates a random solution of the
-- given width, and using the given number of colors.
generateSolution =
  do
    g <- getStdGen
    let rs = take width (randoms g)
    return (map ((+1) . (`mod` colors)) rs)
    
third :: (a, a, b) -> b
third (_, _, x) = x

-- The loop function is supposed to perform a single interaction. It
-- reads an input, compares it with the solution, produces output to
-- the user, and if the guess of the player was incorrect, loops.
loop :: Solution -> IO ()
loop s =
  do
    g <- input            -- read (and parse) the user input
    gs <- generateSolution
    let c = check gs g
    let r = report c
    putStrLn r
    if not (third c) then loop s else return ()

black, white :: Solution -> Guess -> Int
black [] [] = 0
black solution guess = if (head solution) == (head guess)
        then 1 + (black (tail solution) (tail guess))
        else  (black (tail solution) (tail guess))   
        
white' :: Solution -> Guess -> Int
white' _ [] = 0
white' solution guess = 
        if elem (head guess) solution
        then 1 + (white' (filter (\c -> c /= (head guess)) solution) (tail guess))
        else white' solution (tail guess)

white solution guess = (white' solution guess) - (black solution guess)

check :: Solution -> Guess -> (Int,   -- number of black points,
                               Int,   -- number of white points
                               Bool)  -- all-correct guess?
check solution guess = (black solution guess,
                        white solution guess,
                        if black solution guess == 4 then True else False)

-- report is supposed to take the result of calling check, and
-- produces a descriptive string to report to the player.
report :: (Int, Int, Bool) -> String
report (black, white, correct) = if correct 
        then "Black: " ++ show black ++ ", white: " ++ show white ++ ". Congrats!"
        else "Black: " ++ show black ++ ", white: " ++ show white

-- The function input is supposed to read a single guess from the
-- player. The first three instructions read a line. You're supposed
-- to (primitively) parse that line into a sequence of integers.
input :: IO Guess
input =
  do
    putStr "? "
    hFlush stdout -- ensure that the prompt is printed
    l <- getLine
    (return (map readInt (words l)))

-- The following function |readInt| is given and parses a single
-- integer from a string. It produces |-1| if the parse fails. You
-- can use it in |input|.
readInt :: String -> Int
readInt x =
  case reads x of
    [(n, "")] -> n
    _         -> -1

-- The function valid tests a guess for validity. This is a bonus
-- exercise, so you do not have to implement this function.
valid :: Guess -> Bool
valid guess = tODO True