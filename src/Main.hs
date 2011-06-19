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

noop = [(left, "I", "0")]
clean host = let h = show host in [(left, "put", h)]
incValueNumber h n = [(right, "zero", h)] ++ times [(left, "succ", h)] n
operationTarget target h = times [(left, "K", h), (left, "S", h), (right, "succ", h)] target
copyToBaseFrom n = clean 0 ++ incValueNumber "0" n ++ [(left, "get", "0")]

-- combos
make operation target hero host = let h = show host in incValueNumber h hero ++[(left, operation, h)] ++ operationTarget target h ++ [(right, "zero", h)]
addThirdParam host = let h = show host in [(left, "K", h), (left, "S", h), (right, "get", h), (left, "K", h), (left, "S", h), (right, "succ", h), (right, "zero", h)]
simpleAttackOn target hero =  make "attack" target hero 0 ++ addThirdParam 0
simpleHeal target hero = make "help" target hero 0 ++ addThirdParam 0
strike n host target = let h = show host in times ([(right, "zero", h)] ++ times [(left, "succ", h)] target ++ [(left, "dec", h)]) n 

makeZombie host = let h = show host in [(right, "zero", h), (left, "zombie", h), (left, "K", h) , (left, "S", h), (right, "get", h), (right, "zero", h)]
updateZF = [(left, "K", "0"), (left, "S", "0"), (left, "K", "0"), (left, "S", "0"), (right, "get", "0"), (left, "K", "0"), (left, "S", "0"), (right, "succ", "0"), (right, "zero", "0")]
makeZF = clean 1 ++ makeElephant 1 ++ [(left, "K", "1")]

makeFastZF = helpTemplate 5 ++ copyToBaseFrom 5

-- template to make fast "help" to opponent
g3 h = incValueNumber "0" 3 ++ [(left, "K", "0")] ++ [(right, "get", h), (left, "K", h), (left, "S", h), (left, "K", h), (left, "S", h), (right, "get", h), (right, "zero", h)]
operationFold op host = let h = show host in [(right, op, h),(left, "K", h),(left, "S", h),(left, "K", h),(left, "S", h), (right, "get", h),(right, "zero", h)]
finalFolding h = [(left, "S", h), (left, "K", h), (left, "S", h), (right, "get", h), (right, "zero", h)] 
ng3 host = let h = show host in g3 h ++ copyToBaseFrom host ++ clean host ++ operationFold "succ" host
formTemplatePart host func = ng3 host ++ func ++ copyToBaseFrom host ++ clean host
helpTemplate host = let h = show host in formTemplatePart (host-1) (operationFold "help" host) ++ finalFolding h

ig3 host = let h = show host in g3 h 
neutralTemplate host func = ig3 host ++ func ++ copyToBaseFrom host ++ clean host
makeNeutralTemplate host = let h = show host in neutralTemplate (host-1) (operationFold "help" host) ++ [(left, "S", h), (right, "get", h), (right, "zero", h)] 

-- slot 3 is a counter; 5 is a storage for template
flushTemplate = copyToBaseFrom 5 ++ [(right, "I", "0")] ++ updateZF
updateCounter = times [(left, "succ", "3")] 2
initCounter = [(right, "zero", "3")]

-- macro combos
-- 1Kill  (158 turns)
firstKill = makeKiller 1 (simpleHeal 2 3) ++ simpleAttackOn 0 2
overKill = clean 5 ++ makeNeutralTemplate 5 ++ clean 3 ++ initCounter ++ times [(left, "succ", "3")] 2 ++ flushTemplate ++ makeZombie 2

-- stratages
fastAttack = attack 255 where attack n = simpleAttackOn n (n+2) ++ strike 1001 0 n ++ attack (n - 1)
zombiWaves = concat [flushTemplate ++ updateCounter ++ makeZombie 2 | n <- [1..127]] ++ flushTemplate ++ makeZombie 2  -- ++ overKill -- ++ zombiWaves
zombiAttack = makeZF ++ helpTemplate 5 ++ initCounter ++ flushTemplate ++ makeZombie 2 ++ zombiWaves

testExample = firstKill ++ zombiAttack ++ noop

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
