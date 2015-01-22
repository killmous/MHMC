module MHMC
(
    mhmc
) where

import MHMC.Display
import MHMC.Event
import MHMC.MPD
import MHMC.RWS

import Graphics.Vty hiding (Cursor(..))
import Graphics.Vty.Prelude

import qualified Network.MPD as MPD

import Control.Applicative
import Control.Concurrent
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.Lazy
import Data.Char
import Data.Default
import Data.Either
import Data.Maybe

-- | command to run the program
mhmc :: IO ()
mhmc = do
    vty <- mkVty def
    currentEvent <- newEmptyMVar
    let reader = MHMCReader vty currentEvent
    let state = MHMCState {
        getScreen = Help,
        getCursor = 0,
        getMaxCursor = 0
    }
    forkIO $ eventLoop reader
    execRWST (setScreen Playlist >> loop) reader state >> return ()

loop :: MHMC ()
loop = do
    status <- lift $ MPD.withMPD MPD.status
    e <- asks getEvent >>= lift . tryTakeMVar
    vty <- asks getVty
    (width, height) <- lift $ displayBounds $ outputIface vty
    pic <- display (width, height)
    screen <- gets getScreen
    lift $ update vty pic
    case e of
        Just (EvKey (KChar '1') []) -> setScreen Help >> loop
        Just (EvKey (KChar '2') []) -> setScreen Playlist >> loop
        Just (EvKey (KChar '3') []) -> setScreen Browse >> loop
        Just (EvKey (KChar '4') []) -> setScreen Search >> loop
        Just (EvKey (KChar '5') []) -> setScreen Library >> loop
        Just (EvKey (KChar '6') []) -> setScreen PlaylistEditor >> loop
        Just (EvKey (KChar '7') []) -> setScreen MusicDir >> loop
        Just (EvKey (KChar '8') []) -> setScreen Outputs >> loop
        Just (EvKey (KChar '9') []) -> setScreen Visualizer >> loop
        Just (EvKey (KChar '0') []) -> setScreen Clock >> loop
        Just (EvKey (KChar 'q') []) -> lift $ shutdown vty
        Just (EvKey KDown [])       -> case screen of
            Help      -> incCursor >> loop
            Playlist  -> incCursor >> loop
            otherwise -> loop
        Just (EvKey KUp [])         -> case screen of
            Help      -> decCursor >> loop
            Playlist  -> decCursor >> loop
            otherwise -> loop
        Just (EvKey KLeft [])       -> (lift $ decVolume status) >> loop
        Just (EvKey KRight [])      -> (lift $ incVolume status) >> loop
        otherwise                   -> loop

incCursor :: MHMC ()
incCursor = do
    screen <- gets getScreen
    cursor <- gets getCursor
    maxcursor <- gets getMaxCursor
    put $ MHMCState {
        getScreen = screen,
        getCursor = min (cursor + 1) maxcursor,
        getMaxCursor = maxcursor
    }

decCursor :: MHMC ()
decCursor = do
    screen <- gets getScreen
    cursor <- gets getCursor
    maxcursor <- gets getMaxCursor
    put $ MHMCState {
        getScreen = screen,
        getCursor = max (cursor - 1) 0,
        getMaxCursor = maxcursor
    }