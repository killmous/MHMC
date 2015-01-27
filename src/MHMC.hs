module MHMC
(
    mhmc
) where

import MHMC.Display
import MHMC.Display.Browse
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
    let state = MHMCState Help 0 0 0 0 Nothing
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
    scroll <- gets getScroll
    cursor <- gets getCursor
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
        Just (EvKey (KChar 'c') []) -> (lift clearQueue) >> setScreen Playlist >> loop
        Just (EvKey (KChar 'q') []) -> lift $ shutdown vty
        Just (EvKey (KChar 'P') []) -> (lift $ togglePlaying status) >> loop
        Just (EvKey (KChar 's') []) -> (lift stopPlaying) >> loop
        Just (EvKey KLeft [])       -> (lift $ decVolume status) >> loop
        Just (EvKey KRight [])      -> (lift $ incVolume status) >> loop
        Just (EvKey KDown [])       -> case screen of
            Help      -> incCursor >> loop
            Playlist  -> incCursor >> loop
            Browse    -> incCursor >> loop
            otherwise -> loop
        Just (EvKey KUp [])         -> case screen of
            Help      -> decCursor >> loop
            Playlist  -> decCursor >> loop
            Browse    -> decCursor >> loop
            otherwise -> loop
        Just (EvKey KEnter [])      -> case screen of
            Playlist  -> (lift $ playSong $ scroll + cursor) >> loop
            Browse    -> do
                path <- gets getPath
                dir <- lift $ getDirectory path
                changeDirectory dir $ scroll + cursor
                loop
            otherwise -> loop
        Just (EvKey KDel [])        -> case screen of
            Playlist  -> (lift $ removeSong $ scroll + cursor) >> loop
            otherwise -> loop
        Just (EvKey (KChar ' ') []) -> case screen of
            Browse    -> loop
            otherwise -> loop
        otherwise                   -> loop

incCursor :: MHMC ()
incCursor = do
    state <- get
    cursor <- gets getCursor
    maxcursor <- gets getMaxCursor
    scroll <- gets getScroll
    maxscroll <- gets getMaxScroll
    put $ state {
        getCursor = min (cursor + 1) maxcursor,
        getScroll = if cursor == maxcursor then min (scroll + 1) maxscroll else scroll
    }

decCursor :: MHMC ()
decCursor = do
    state <- get
    cursor <- gets getCursor
    scroll <- gets getScroll
    put $ state {
        getCursor = max (cursor - 1) 0,
        getScroll = if cursor == 0 then max (scroll - 1) 0 else scroll
    }