{-# LANGUAGE OverloadedStrings #-}
module Client where

import           Control.Monad       (forever, unless)
import           Control.Monad.Trans (liftIO)
import           Network.Socket      (withSocketsDo)
import           Data.Text           hiding (map, filter)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as WS

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy
import Control.Lens 
import Control.Concurrent
import Control.Concurrent.STM
import Data.Aeson
import Data.Maybe

import Animation
import Constants
import CommonData
import Logic
import Rendering
import Demo (sampleWorld)

--  TODO: Make client download world from Server(not just load from Demo)
--  TODO: Make non hardcoded name for player (read from args)
-- | Entry point when connection with server is established
-- | and socket for communication is obtained
app :: Name -> WS.ClientApp ()
app name conn  = do
    putStrLn "Client: Connected!"

    -- | Get unique ID for our player
    uniqueIDMsg <- WS.receiveData conn :: IO Text
    let uniqueID = read $ Data.Text.unpack uniqueIDMsg :: ID
    print $ show uniqueID ++ " <--  that is uniqueID i received from the server."
    -- | Creates world
    playerAnimationTable <- loadPlayerAnimations
    let world = sampleWorld blank uniqueID name playerAnimationTable (RectangleBox 40 80)
    let player = _myPlayer world
    -- | send our Info
    WS.sendTextData conn (encode $ clientInfoFromEntity player)
    -- | creates Shared Transactional Memory(STM) we can communicate over
    -- | treat it as non-blocking variable you can read from and write to
    otherInfo <- newTVarIO [] :: IO (TVar [ClientInfo])
    myInfo    <- newTVarIO (clientInfoFromEntity player) :: IO (TVar ClientInfo)

    -- | Fork a thread that notifies us about other player's movements
    _ <- forkIO $ forever $ do
        serverInfoMsg <- WS.receiveData conn
        let serverInfo = decode serverInfoMsg :: Maybe [ClientInfo]
        case serverInfo of
            Just info -> do
                putStrLn "Client: Got information from server."
                atomically $ writeTVar otherInfo info
            Nothing   -> do
                putStrLn "Client: Got Nothing from server (probably bad decode)."
                return ()

    -- | Fork a thread that notifies server about our movements
    _ <- forkIO $ forever $ do
        threadDelay delaySending
        myInfoCurrent <- readTVarIO myInfo
        WS.sendTextData conn (encode myInfoCurrent)

    -- start playing
    debug otherInfo myInfo world playerAnimationTable

    WS.sendClose conn ("Bye!" :: Text)
    putStrLn "Client: Disconnected"

-- | Driver function
clientMain :: Name -> IO ()
clientMain name = 
    withSocketsDo $ WS.runClient defaultIP defaultPort "/" (app name)

-- | forget about release, use debug(it works nicely)
-- release :: IO ()
-- release = do
--     bgPic <- loadPicture backgroundPath
--     playerAnimationTable <- loadPlayerAnimations
--     let world = sampleWorld bgPic playerAnimationTable (RectangleBox 50 50)
--     play FullScreen white simulationRate world renderWorld handleInput updateWorld

debug :: TVar [ClientInfo] -> TVar ClientInfo -> World -> PlayerAnimationTable -> IO ()
debug otherInfo ourInfo world table= do
    putStrLn "Client: Starting the game..."
    playIO (InWindow "LambdaShooter" (1280, 720) (0, 0)) white simulationRate
     world renderWorldIO handleInputIO (updateWorldIO otherInfo ourInfo table)

renderWorldIO :: World -> IO Picture
renderWorldIO = return . renderWorld

handleInputIO :: Event -> World -> IO World
handleInputIO event world = return (handleInput event world)

updateWorldIO :: TVar [ClientInfo] -> TVar ClientInfo -> PlayerAnimationTable
 -> Float -> World  -> IO World
updateWorldIO otherInfo ourInfo playerAnimationTable timePassed world  = do
    -- | read the last information server have sent us
    clientsInfoIO <- readTVarIO otherInfo
    -- | clientsInfo -> Entities
    let newEntities = map (entityFromClientInfo playerAnimationTable) clientsInfoIO :: [Entity]
    let playerID = _entityID $ world ^. myPlayer
    let newEntitiesWithoutMe = filter ((playerID /=). _entityID) newEntities
    let newWorld = updateWorld timePassed world {_entities = newEntitiesWithoutMe}
    -- | Modify variable which is used to notify server about our player movements
    atomically $ writeTVar ourInfo (clientInfoFromEntity $ newWorld ^. myPlayer)
    -- | update new world with entites from the server.
    return newWorld
    