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

-- 10000 int function
makeElephant host = let h = show host in [(right, "zero", h)] ++ times [(left, "succ", h)] 39 ++ times [(left, "dbl", h)] 8 ++ times [(left, "succ", h)] 16

-- 11112 int function
makeKiller host inBetween = let h = show host in [(right, "zero", h)] ++ times [(left, "succ", h)] 5 ++ times [(left, "dbl", h)] 8 ++ inBetween ++ times [(left, "succ", h)] 109 ++ times [(left, "dbl", h)] 3

makeHelper host = let h = show host in [(right, "zero", h)] ++ times [(left, "succ", h)] 1 ++ times [(left, "dbl", h)] 11

-- take number from slot #1
-- makeHelp target hero -- is a generic

noop = return (left, "I", "0")
clean host = let h = show host in [(left, "put", h)]
incValueNumber h n = [(right, "zero", h)] ++ times [(left, "succ", h)] n
operationTarget target h = times [(left, "K", h), (left, "S", h), (right, "succ", h)] target

-- combos
make operation target hero host = let h = show host in incValueNumber h hero ++[(left, operation, h)] ++ operationTarget target h ++ [(right, "zero", h)]
addThirdParam host = let h = show host in [(left, "K", h), (left, "S", h), (right, "get", h), (left, "K", h), (left, "S", h), (right, "succ", h), (right, "zero", h)]
simpleAttackOn target hero =  make "attack" target hero 0 ++ addThirdParam 0
simpleHeal target hero = make "help" target hero 0 ++ addThirdParam 0
strike n host target = let h = show host in times ([(right, "zero", h)] ++ times [(left, "succ", h)] target ++ [(left, "dec", h)]) n 

makeZombie host = let h = show host in [(right, "zero", h), (left, "zombie", h), (left, "K", h) , (left, "S", h), (right, "get", h), (right, "zero", h)]
makeZombieFunction = make "help" 0 1 0 ++ clean 1 ++ makeElephant 1 ++ [(left, "K", "1"), (left, "K", "0"), (left, "S", "0"), (left, "K", "0"), (left, "S", "0"), (right, "get", "0"), (left, "K", "0"), (left, "S", "0"), (right, "succ", "0"), (right, "zero", "0")]

-- macro combos
-- 1Kill  (158 turns)
firstKill = makeKiller 1 (simpleHeal 2 3) ++ simpleAttackOn 0 2

-- stratages
fastAttack = attack 0 where attack n = simpleAttackOn n (n+2) ++ strike 1001 0 n ++ attack (n + 1)

testExample = firstKill ++ makeZombieFunction ++ clean 1 ++ makeZombie 1 ++ noop

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
