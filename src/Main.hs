module Main where
import System( getArgs )
import Control.Concurrent
import IO

opponentsTurn = do
        app <- getLine
        if app == "1" then do
                card <- getLine
                slot <- getLine
                return ()
                else if app == "2" then do
                slot <- getLine
                card <- getLine
                return ()
                else return ()
               
myTurn = do
        hSetBuffering stdout NoBuffering
        putStrLn "1"
        putStrLn "I"
        putStrLn "0"   
        opponentsTurn
        myTurn
        
         
start :: String -> IO()
start "1" = opponentsTurn
start _ = return()

main::IO()
main = do
args <- getArgs
start (head args) 
myTurn