module Main where
import System( getArgs )
import IO
import Helpers
import Strategy

-- interaction handle
opponentsTurn = do
        app <- getLine
        if app == left then do
                card <- getLine
                slot <- getLine
                return (app, card, slot)
                else if app == right then do
                slot <- getLine
                card <- getLine
                return (app, card, slot)
                else error ("error reading input")
               
makeTurn (app, card, slot) | app == left = do
        putStrLn left
        putStrLn card
        putStrLn slot  
        | app == right = do
        putStrLn right
        putStrLn slot
        putStrLn card  
        
myTurn a = do
        hSetBuffering stdout NoBuffering
        makeTurn (head (a))
        opponentsTurn 
        myTurn (tail (a))
         
testTurn  = myTurn listOfTurns

start "1" = opponentsTurn
start _ = return (left, "I", "0") -- refactor this

main::IO()
main = do
args <- getArgs
start (head args) 
testTurn
