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
import Control.Monad.IO.Class
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
    forkIO $ eventLoop vty currentEvent
    let reader = MHMCReader vty currentEvent
    let state = MHMCState Playlist 0
    execRWST loop reader state >> return ()

loop :: MHMC ()
loop = do
    status <- liftIO $ MPD.withMPD MPD.status
    e <- asks getEvent >>= liftIO . tryTakeMVar
    vty <- asks getVty
    (width, height) <- liftIO $ displayBounds $ outputIface vty
    pic <- display (width, height)
    liftIO $ update vty pic
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
        Just (EvKey (KChar 'q') []) -> liftIO $ shutdown vty
        otherwise                   -> loop