{-# LANGUAGE OverloadedStrings #-}
module Server where

import Control.Exception (finally)
import Control.Monad (forM, forM_, forever, unless, join)
import Control.Concurrent
import Control.Monad.IO.Class (liftIO)
import Control.Lens

import qualified Network.WebSockets as WS

import Data.Aeson hiding ((.=))
import Data.Maybe
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import System.Random
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Physics
import Logic
import CommonData
import Demo
import Constants
import Constructors

type Client = (ID, WS.Connection)

type ServerState = (World, [Client])

newServerState :: ServerState
newServerState = (serverWorld, []) where
    serverWorld = sampleWorld (-100500) "ServerSamplePlayer"

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
    newWorld = oldWorld &~ do
        projectiles .= (clientWorld  ^. myProjectiles) ++ (oldWorld ^. projectiles)
        players .= newPlayers

makeAliveIfNeed :: Entity -> Entity
makeAliveIfNeed entity@(Entity id body eData dir) = newEntity where
    newEntity = 
        if   fromMaybe Idle (eData ^? currentState) == Dying 
        then entity & entityData .~ eDataUpdated 
                    & entityBody .~ bodyUpdated
        else entity
    eDataUpdated = eData & currentState .~ Falling 
                         & health .~ maxHealth
                         & statistics . deaths +~ 1
    bodyUpdated  = body & bodyPosition .~ defaultPosition

-- | Update kills statistic given the ids of killers
updateKillsStatistic :: [Entity] -> [ID] -> [Entity]
updateKillsStatistic allPlayers killersIds = map updatePlayer allPlayers 
    where
        updatePlayer :: Entity -> Entity
        updatePlayer playerEntity
            | playerEntity ^. entityID `elem` killersIds 
                = playerEntity & entityData . statistics . kills +~ 1
            | otherwise = playerEntity 

updateServerWorld :: World -> World
updateServerWorld oldWorld@(World wMap allProjectiles _ allPlayers _ _ _) 
    = oldWorld &~
    do
        players .= rewardedPlayers 
        projectiles .= freeProjectiles
    where
    freeProjectiles =
         filter (not . detectEntitiesCollision allPlayers . view entityBody) allProjectiles
    collidedProjectiles =
         filter (detectEntitiesCollision allPlayers . view entityBody) allProjectiles
    checkHit = map damageIfHit allPlayers
    newPlayers = map (makeAliveIfNeed . killIfOutOfWorld . fst) checkHit
    killingIds = mconcat $ map (fromMaybe [] . snd) checkHit

    rewardedPlayers = updateKillsStatistic newPlayers killingIds

    damageIfHit :: Entity -> (Entity, Maybe [ID])
    damageIfHit aPlayer
        | null hitBullets = (aPlayer, Nothing)
        | otherwise = (damagedPlayer, killingIds)
        where
            hitBullets = filter (checkEntityCollision aPlayer) collidedProjectiles
            bulletPower = fromMaybe baseBulletPower . preview (entityData . projectilePower)
            damage
                = sum (map bulletPower hitBullets)
            damagedPlayer = damagePlayer aPlayer damage
            killingIds =
                if fromMaybe 0 (damagedPlayer ^? entityData . health) <= 0
                then Just (map (view entityID) hitBullets)
                else Nothing

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
