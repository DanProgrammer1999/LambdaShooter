{-# LANGUAGE OverloadedStrings #-}
module Client where

import           Control.Monad       (forever, unless)
import           Control.Monad.Trans (liftIO)
import           Network.Socket      (withSocketsDo)
import           Data.Text           hiding (map, filter, head, null)
import qualified Data.Text           as T 
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as WS

import           Control.Lens 
import           Control.Concurrent
import           Control.Concurrent.STM
import           Data.Aeson
import           Data.Maybe

import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import           Graphics.Gloss.Interface.Environment
import           Graphics.Gloss.Juicy

import           Animation
import           Constants
import           CommonData
import           Logic
import           Rendering
import           Demo                (sampleWorld)

receiveInfoFromServer :: WS.Connection -> TChan World -> IO ()
receiveInfoFromServer conn otherInfo = forever $ do
    serverInfoMsg <- WS.receiveData conn
    let serverInfo = decode serverInfoMsg :: Maybe World
    case serverInfo of
        Just info -> do 
            atomically $ writeTChan otherInfo info
            return ()
        Nothing   -> do
            putStrLn "Client: Got Nothing from server (probably bad decode)."
            return ()

sendInfoToServer :: WS.Connection -> TChan World -> IO ()
sendInfoToServer conn myInfo = forever $ do
    myInfoCurrent <- atomically $ readTChan myInfo
    WS.sendTextData conn (encode myInfoCurrent)
    return ()

-- | Entry point when connection with server is established
-- | and socket for communication is obtained
app :: Name -> WS.ClientApp ()
app name conn  = do
    putStrLn "Client: Connected!"
    -- | Get unique ID for our player
    uniqueIDMsg <- WS.receiveData conn :: IO Text
    let uniqueID = read $ Data.Text.unpack uniqueIDMsg :: ID
    print $ show uniqueID ++ " <--  that is uniqueID i received from the server."
    -- | Creates player & world
    let world = sampleWorld uniqueID name
    -- | Send our Player Info (first and last time we send Player)
    WS.sendTextData conn (encode $ _myPlayer world)
    -- | Treat TVars for communication
    -- | They are non-blocking variable you can read & modify from different threads
    otherInfo <- newTChanIO :: IO (TChan World)
    myInfo    <- newTChanIO :: IO (TChan World)
    -- | Fork a thread that notifies us about other player's movements
    _ <- forkIO $ receiveInfoFromServer conn otherInfo
    -- | Fork a thread that notifies server about our movements
    _ <- forkIO $ sendInfoToServer conn myInfo
    -- | Load Graphics(Assets) & Start playing
    graphics <- loadGameGraphics
    debug uniqueID otherInfo myInfo (Universe world graphics)
    -- | In case we stop to play (closed game window) we close connection with server
    WS.sendClose conn ("Bye!" :: Text)
    putStrLn "Client: Disconnected"

-- | Driver function
clientMain :: Name -> IO ()
clientMain name = 
    withSocketsDo $ WS.runClient defaultIP defaultPort "/" (app name)

debug :: ID -> TChan World -> TChan World -> Universe -> IO ()
debug playerID otherInfo ourInfo u@(Universe world graphics) = do
    putStrLn "Client: Starting the game..."
    playIO (InWindow "LambdaShooter" defaultWindowSize (0, 0)) white simulationRate
     u renderWorldIO  handleInputIO
      (updateWorldIO playerID otherInfo ourInfo)

renderWorldIO :: Universe -> IO Picture
renderWorldIO = return . renderWorld 

handleInputIO :: Event -> Universe -> IO Universe
handleInputIO event u = return (handleInput event u)

updateWorldIO :: ID -> TChan World -> TChan World -> Float -> Universe -> IO Universe
updateWorldIO myPlayerID otherInfo ourInfo timePassed u@(Universe world graphics) = do
    -- | read the last information server have sent us
    serverWorldTry <- atomically $ tryReadTChan otherInfo
    Universe newWorld newGraphics <- case serverWorldTry of
            Just serverWorld -> do
                let newEntities = serverWorld ^. players
                let newEntitiesWithoutMe =  filter ((myPlayerID /=) . view entityID) newEntities
                let serverPlayerList     =  filter ((myPlayerID ==) . view entityID) newEntities :: [Entity]
                let serverPlayer = if null serverPlayerList
                    then world ^. myPlayer
                    else head serverPlayerList 
                let acceptServerPlayer =
                        _currentState (world ^. myPlayer . entityData) == Dying ||
                        _currentState (serverPlayer     ^. entityData) == Dying
                let updatedPlayer = if acceptServerPlayer then serverPlayer else world ^. myPlayer
                let world' = world &
                        players     .~ newEntitiesWithoutMe &
                        projectiles .~ serverWorld ^. projectiles &
                        myPlayer    .~ updatedPlayer
                return $ updateWorld timePassed (Universe world' graphics)
            Nothing -> return $ updateWorld timePassed (Universe world graphics)
                

    -- | Modify variable which is used to notify server about our player movements
    atomically $ writeTChan ourInfo newWorld
    -- | Clear our projectiles after we have sent them to server 
    return (Universe newWorld{_myProjectiles = []} newGraphics)

    