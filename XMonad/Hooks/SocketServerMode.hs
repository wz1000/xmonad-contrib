{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.SocketServerMode
-- Description :  Send commands to a running xmonad process via Unix socket.
-- Copyright   :  (c) 2025
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module provides Unix socket-based command server for XMonad.
-- It allows external clients to send commands and receive responses.
--
-----------------------------------------------------------------------------

module XMonad.Hooks.SocketServerMode
    ( -- * Usage
      -- $usage
      socketServerEventHook
    , socketServerStartup
    ) where

import XMonad
import XMonad.Prelude
import qualified XMonad.Util.ExtensibleState as ES

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as BS
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (forever, void, when)
import Control.Exception (bracket, catch, SomeException, finally)
import System.Directory (removeFile, doesFileExist)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, hFlush, stderr)
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

-- $usage
-- You can use this module with the following in your @xmonad.hs@:
--
-- > import XMonad.Hooks.SocketServerMode
--
-- Then add to your config:
--
-- > main = xmonad def
-- >   { startupHook = socketServerStartup <> startupHook def
-- >   , handleEventHook = socketServerEventHook handleCommand <> handleEventHook def
-- >   }
-- >
-- > handleCommand :: [String] -> X String
-- > handleCommand ["workspace", "next"] = moveTo Next ... >> return "OK"
-- > handleCommand xs = return $ "Unknown command: " ++ unwords xs

-- | Pending command with response MVar
data PendingCommand = PendingCommand
    { cmdArgs :: [String]
    , cmdResponse :: MVar String
    }

-- | Extensible state to hold pending commands
newtype PendingCommands = PendingCommands { getPending :: MVar (Maybe PendingCommand) }

instance ExtensionClass PendingCommands where
    initialValue = error "PendingCommands not initialized"

-- | Get the socket path based on DISPLAY environment variable
getSocketPath :: IO FilePath
getSocketPath = do
    displayEnv <- fromMaybe ":0" <$> lookupEnv "DISPLAY"
    let displayNum = filter (`elem` ['0'..'9']) displayEnv
    return $ "/tmp/xmonad-" ++ displayNum ++ "-socket"

-- | Remove socket file if it exists
cleanupSocket :: FilePath -> IO ()
cleanupSocket path = do
    exists <- doesFileExist path
    when exists $ removeFile path `catch` \(_ :: SomeException) -> return ()

-- | Send a custom X event to wake up the event loop
sendWakeupEvent :: Display -> Window -> IO ()
sendWakeupEvent dpy rootWin = do
    atom <- internAtom dpy "XMONAD_SOCKET_COMMAND" False
    atomName <- getAtomName dpy atom
    hPutStrLn stderr $ "Sending wakeup event with atom: " ++ show atomName ++ " (id=" ++ show atom ++ ")"
    allocaXEvent $ \e -> do
        setEventType e clientMessage
        setClientMessageEvent e rootWin atom 32 0 currentTime
        sendEvent dpy rootWin False structureNotifyMask e
        sync dpy False
    hPutStrLn stderr "Wakeup event sent and synced"

-- | Startup hook to initialize the socket server
socketServerStartup :: X ()
socketServerStartup = do
    -- Create MVar for pending commands
    pendingMVar <- io $ newMVar Nothing
    ES.put $ PendingCommands pendingMVar

    io $ do
        sockPath <- getSocketPath

        -- Clean up any existing socket
        cleanupSocket sockPath

        -- Create the Unix socket
        sock <- socket AF_UNIX Stream defaultProtocol
        bind sock (SockAddrUnix sockPath)
        listen sock 5

        hPutStrLn stderr $ "XMonad socket server listening on: " ++ sockPath

        -- Start the server thread (it will open its own Display connection)
        void $ forkIO $ serverLoop sock pendingMVar

-- | Main server loop that accepts connections
serverLoop :: Socket -> MVar (Maybe PendingCommand) -> IO ()
serverLoop sock pendingMVar = forever $ do
    hPutStrLn stderr "Waiting for connection..." >> hFlush stderr
    (clientSock, _) <- accept sock
    hPutStrLn stderr "Connection accepted, forking handler" >> hFlush stderr
    void $ forkIO $ handleClient clientSock pendingMVar
  `catch` \(e :: SomeException) ->
    hPutStrLn stderr ("Socket server error: " ++ show e) >> hFlush stderr

-- | Handle a single client connection
handleClient :: Socket -> MVar (Maybe PendingCommand) -> IO ()
handleClient clientSock pendingMVar =
  (flip finally (close clientSock) $ do
    hPutStrLn stderr "handleClient started, about to recv" >> hFlush stderr
    -- Receive command from client
    msg <- recv clientSock 4096
    hPutStrLn stderr ("recv returned, got: " ++ show (BS.length msg) ++ " bytes") >> hFlush stderr
    let cmdStr = BS.unpack msg

    when (not $ null cmdStr) $ do
        let args = words cmdStr

        hPutStrLn stderr ("Received command: " ++ cmdStr) >> hFlush stderr

        -- Create response MVar
        responseMVar <- newEmptyMVar

        -- Store the command in the pending MVar (swap to avoid blocking)
        void $ swapMVar pendingMVar (Just $ PendingCommand args responseMVar)

        -- Open a separate Display connection for sending events
        bracket (openDisplay "") closeDisplay $ \dpy -> do
            let rootWin = defaultRootWindow dpy

            -- Send X event to wake up the event loop
            sendWakeupEvent dpy rootWin

            -- Wait for response
            response <- takeMVar responseMVar

            -- Send response back to client
            sendAll clientSock (BS.pack $ response ++ "\n")
  ) `catch` \(e :: SomeException) ->
    hPutStrLn stderr $ "Error handling client: " ++ show e

-- | Event hook to process commands
socketServerEventHook :: ([String] -> X String) -> Event -> X All
socketServerEventHook handler ClientMessageEvent {ev_message_type = mt} = do
    dpy <- asks display
    atom <- io $ internAtom dpy "XMONAD_SOCKET_COMMAND" False

    mtName <- io $ getAtomName dpy mt
    atomName <- io $ getAtomName dpy atom

    io $ hPutStrLn stderr $ "Event hook triggered, received atom: " ++ show mtName ++ " (id=" ++ show mt ++ "), expected: " ++ show atomName ++ " (id=" ++ show atom ++ "), match: " ++ show (mt == atom)

    when (mt == atom) $ do
        io $ hPutStrLn stderr "Processing socket command event"

        -- Get the pending command
        PendingCommands pendingMVar <- ES.get
        mCmd <- io $ tryTakeMVar pendingMVar

        io $ hPutStrLn stderr $ "Retrieved command from MVar: " ++ show (isJust mCmd)

        case mCmd of
            Just (Just cmd) -> do
                io $ hPutStrLn stderr $ "Executing command: " ++ unwords (cmdArgs cmd)

                -- Execute the command handler
                response <- handler (cmdArgs cmd) `catchX` return "Error executing command"

                io $ hPutStrLn stderr $ "Command executed, response: " ++ response

                -- Send response back
                io $ putMVar (cmdResponse cmd) response

                -- Clear the pending slot
                io $ putMVar pendingMVar Nothing

            _ -> io $ hPutStrLn stderr "No command in MVar"

    return (All True)

socketServerEventHook _ _ = return (All True)
