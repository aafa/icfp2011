module Main where
import System( getArgs )
import IO
import Helpers

left  = "1"
right = "2"
cards = ["I", "zero", "succ", "dbl", "get",
         "put", "S", "K", "inc", "dec", "attack",
         "help", "copy", "revive", "zombie"
        ]

growSlotUpTo host n = let h = show host in [(right, "zero", h), (left, "succ", h)] ++ times [(left, "dbl", h)] n

-- take number from slot #1
-- makeHelp target hero -- is a generic

clean host = [(left, "put", host)]
generateValueNumber host n = let h = show host in [(right, "zero", h)] ++ times [(left, "succ", h)] n
operationTarget target host = let h = show host in times [(left, "K", h), (left, "S", h), (right, "succ", h)] target

make operation target hero host = let h = show host in generateValueNumber host hero ++[(left, operation, h)] ++ operationTarget target host ++ [(right, "zero", h)]
addThirdParam host = let h = show host in [(left, "K", h), (left, "S", h), (right, "get", h), (left, "K", h), (left, "S", h), (right, "succ", h), (right, "zero", h)]

simpleAttack =  make "attack" 0 1 0 ++ addThirdParam 0
simpleHeal = make "help" 1 1 0 ++ addThirdParam 0
elephant = growSlotUpTo 1 10

testExample = elephant ++  simpleAttack

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
         
testTurn  = myTurn testExample

start "1" = opponentsTurn
start _ = return (left, "I", "0") -- refactor this

main::IO()
main = do
args <- getArgs
start (head args) 
testTurn