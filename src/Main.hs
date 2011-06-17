module Main where
import System( getArgs )
import Control.Concurrent
import IO
import Helpers

left  = "1"
right = "2"
cards = ["I", "zero", "succ", "dbl", "get",
         "put", "S", "K", "inc", "dec", "attack",
         "help", "copy", "revive", "zombie"
        ]

growSlotUpTo slot n = [(right, "zero", slot), (left, "succ", slot)] ++ times (left, "dbl", slot) n

next [] = []
next (x:xs) = x : next xs
getNext a = head (next a)

opponentsTurn = do
        app <- getLine
        if app == "1" then do
                card <- getLine
                slot <- getLine
                return (app, card, slot)
                else if app == "2" then do
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
         
testTurn  = myTurn (growSlotUpTo "1" 10)

start "1" = opponentsTurn
start _ = return (left, "I", "0") -- refactor this

main::IO()
main = do
args <- getArgs
start (head args) 
testTurn