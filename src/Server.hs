{-# LANGUAGE OverloadedStrings #-}
module Server where

import Control.Exception (finally)
import Control.Monad (forM, forM_, forever, unless, join)
import Control.Concurrent
import Control.Monad.IO.Class (liftIO)
import Control.Lens

import qualified Network.WebSockets as WS

import Data.Aeson
import Data.Maybe
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import System.Random
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Physics
import CommonData
import Demo
import Constants

type Client = (ID, WS.Connection)

type ServerState = (World, [Client])

newServerState :: ServerState
newServerState = (serverWorld, []) where
    serverWorld = sampleWorld (-100500) "ServerSamplePlayer" (CircleBox 20)

-- clientExists :: Client -> ServerState -> Bool
-- clientExists client = any ((== fst client) . snd)

isUnique :: ServerState -> ID -> Bool
isUnique ss id  = id `elem` ids where
    ids =  map fst $ snd ss

addClient :: (Entity, Client) -> ServerState -> ServerState
addClient (player, newClient) (world, clients) = (world, newClient : clients) where
    newWorld = world{_players = player : (world ^. players)}

removeClient :: (Entity, Client) -> ServerState -> ServerState
removeClient (player, client) (world, clients) = (newWorld, newClients) where
    playerID = fst client 
    newClients = filter ((/= playerID).fst) clients
    newWorld = world {_players = filter ((/= playerID ) . view entityID) (world ^. players)} 

generateUniqueID :: ServerState -> IO Int
generateUniqueID ss = do
    let n = length (snd ss) + 10
    randList <- forM [1 .. n] $ \_i -> randomRIO (1, 100000)
    let cleanList = filter (not . isUnique ss) randList
    return $ head cleanList 

---------- World & Entity updates -----------------------
updateFromClientWorld :: World -> ServerState -> IO ServerState 
updateFromClientWorld clientWorld (oldWorld, clients) = return (newWorld,clients) where
    clientID = clientWorld ^. myPlayer . entityID
    otherPlayers = filter ((clientID /=) . view entityID) (oldWorld ^. players)
    clientPlayer = clientWorld ^. myPlayer
    newPlayers = clientPlayer : otherPlayers
    newWorld = oldWorld
        {_projectiles = (clientWorld  ^. myProjectiles) ++ (oldWorld ^. projectiles),
         _players = newPlayers
        }

-- | TODO IMPORTANT UPDATE KILLS
doDamage :: Entity -> Float -> Entity
doDamage e@(Entity id body eData dir) dmg = e {_entityData = eDataUpdated} where
    newHealth = _health eData - dmg
    oldState = _currentState eData    
    newState = if newHealth <= 0 then Dying else oldState 
    eDataUpdated = eData {
        _health       = newHealth,
        _currentState = newState,
        _statistics   = newStatistics}
    oldStatistics = _statistics eData
    oldDeaths     = _deaths oldStatistics 
    toAddDeaths   = if newHealth <= 0 then 1 else 0 
    newStatistics = oldStatistics{_deaths = oldDeaths + toAddDeaths}

makeAliveIfNeed :: Entity -> Entity
makeAliveIfNeed e@(Entity id body eData dir) = newEntity where
    newEntity = if _currentState eData == Dying then
        e{_entityData = eDataUpdated, _entityBody = bodyUpdated}
        else e
    eDataUpdated = eData{_currentState = Falling, _health = defaultHP}
    bodyUpdated  = body {_bodyPosition = defaultPosition}

-- TODO VERY IMPORTANT:: dmg should come from bullet.
-- not just 20
updateServerWorld :: World -> World
updateServerWorld oldWorld@(World wMap projectiles _ players _ _ _) = newWorld where
    freeProjectiles =
         filter (not . detectEntitiesCollision players . _entityBody) projectiles
    collidedProjectiles =
         filter (detectEntitiesCollision players . _entityBody) projectiles
    newPlayers = map (makeAliveIfNeed . f) players
    f :: Entity -> Entity
    f e = if detectEntitiesCollision collidedProjectiles $ e ^. entityBody
        then doDamage e 20
        else e
    newWorld = oldWorld{_players = newPlayers, _projectiles = freeProjectiles}

-----------------------------------------------------------

broadcastServerWorld :: MVar ServerState -> IO ()
broadcastServerWorld state = forever $ do
    threadDelay delaySending
    -- | Receive state updates from clients
    newState <- liftIO $ readMVar state
    -- | Check for bullets colliding with Player
    -- | And update (server) world accordingly
    let oldWorld = fst newState
    let newWorld = updateServerWorld oldWorld
    let encodedNewWorld = encode newWorld
    -- | Debug Info
    print "Current Clients are: "
    print (map fst (snd newState))
    -- | Send updated (server) world to each client
    let allConnections = map snd (snd newState)
    forM_ allConnections $ \conn -> WS.sendTextData conn encodedNewWorld

serverMain :: IO ()
serverMain = do
    state <- newMVar newServerState
    _ <- forkIO $ broadcastServerWorld state
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
    -- formJust is ok here as it simply will not allow "bad Player" to connect
    let clientPlayer = fromJust $ decode clientInfoMsg :: Entity
    -- if fromJust doesn't work then connectionFail and it is ok
    let clientInfo = (uniqueID, conn)
    let disconnect = do
            s <- modifyMVar state $ \s ->
                let s' = removeClient (clientPlayer, clientInfo) s in return (s', s')
            return ()
    -- | Update Server State
    flip finally disconnect $ do
        liftIO $ modifyMVar_ state (return.addClient (clientPlayer, clientInfo))
        handleClient state clientInfo
        

handleClient :: MVar ServerState -> Client -> IO ()
handleClient state (playerID, conn) = forever $ do
    clientWorldMsg <- WS.receiveData conn 
    let receivedWorld = decode clientWorldMsg :: Maybe World
    case receivedWorld of
            Just newWorld -> do
                -- | We remove this client's old world and add newWorld for this client
                putStrLn ("Server talk: Got Nothing from client with id = "
                    ++ show playerID)
                liftIO $ modifyMVar_ state $ updateFromClientWorld newWorld
                return ()
            Nothing   -> do
                putStrLn ("Server talk: Got Nothing from client with id = "
                    ++ show playerID) 
                return ()
