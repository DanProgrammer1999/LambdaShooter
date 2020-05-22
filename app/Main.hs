module Main where

import System.Environment
import System.Exit
import           Control.Concurrent  (forkIO)
import           Control.Monad       (forever, unless)

import System.Directory
import Server (serverMain)
import Client (clientMain)

import Control.Concurrent.STM

-- main :: IO ()
-- main = do
--     info <- newTVarIO "Zero message."
--     atomically $ writeTVar info "First message."
--     readMsg  <- readTVarIO info
--     readMsg2 <- readTVarIO info
--     putStrLn readMsg
--     putStrLn readMsg2  
--     return ()

main  = do
    args <- getArgs
    if null args
         then do
             putStrLn "Client started."
             clientMain "Unknown"
         else if head args == "server"
            then do
                 putStrLn "Server started."
                 serverMain
            else if length args >= 2 then do
                putStrLn "Client started."
                clientMain (args !! 1)
            else do
                putStrLn "Client started."
                clientMain "Unknown"






