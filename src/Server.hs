{-# LANGUAGE OverloadedStrings #-}
module Server where

import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM, forM_, forever, unless, join)
import Control.Concurrent
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import Data.Aeson
import Data.Maybe
import System.Random

import CommonData
import Constants

type Client = (ClientInfo, WS.Connection)

-- | clients == MVar ServerState
type ServerState = [Client]

newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

isUnique :: ServerState -> ID -> Bool
isUnique ss id  = id `elem` ids where
    ids =  map (_clientID . fst) ss

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)


generateUniqueID :: ServerState -> IO Int
generateUniqueID clients = do
    let n = length clients + 10
    randList <- forM [1 .. n] $ \_i -> randomRIO (1, 100000)
    let cleanList = filter (not . isUnique clients) randList
    return $ head cleanList 


serverMain :: IO ()
serverMain = do
    state <- newMVar newServerState

    _ <- forkIO $ forever $ do
        threadDelay delaySending
        clients <- liftIO $ readMVar state
        let clientsInfo = map fst clients
        let encodedClientsInfo = encode clientsInfo
        print "Current Clients are:"
        print (map _clientID clientsInfo)
        forM_ clients $ \(info, conn) -> WS.sendTextData conn encodedClientsInfo
    
    WS.runServer defaultIP defaultPort $ application state

-- | This method is used when new connection appears.
-- | It assigns unique ID to player and put him to handleClient function.
application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    print "Someone connected.\n"
    -- | Ping connection to make sure it is alive
    WS.forkPingThread conn 30 

    -- | Get current state (all clients)
    clients <- liftIO $ readMVar state

    -- | Generate and Send unique ID
    uniqueID <- generateUniqueID clients :: IO ID    
    WS.sendTextData conn (T.pack $ show uniqueID)
    
    clientInfoMsg <- WS.receiveData conn 
    let clientInfo = decode clientInfoMsg :: Maybe ClientInfo

    -- if fromJust doesn't work then connectionFail and it is ok
    let client = (fromJust clientInfo, conn)
    let disconnect = do
            s <- modifyMVar state $ \s ->
                let s' = removeClient client s in return (s', s')
            return ()
    -- | Update Server State
    flip finally disconnect $ do
        liftIO $ modifyMVar_ state (return.addClient client)
        handleClient conn state client
        

handleClient :: WS.Connection -> MVar ServerState -> Client -> IO ()
handleClient conn state client@(clientInfo, _) = forever $ do
    clientInfoMsg <- WS.receiveData conn 
    let clientID = _clientID clientInfo
    let receivedInfo = decode clientInfoMsg :: Maybe ClientInfo
    case receivedInfo of
            Just newInfo -> do
                liftIO $ modifyMVar_ state (return.addClient (newInfo, conn) . removeClient client)
                return ()
            Nothing   -> do
                putStrLn ("Server talk: Got Nothing from client with id = "
                    ++ show clientID) 
                return ()
                
