{-# LANGUAGE OverloadedStrings #-}

module MHMC
(
    mhmc
) where

import MHMC.Display
import MHMC.Event
import MHMC.MPD

import Graphics.Vty
import Graphics.Vty.Prelude

import qualified Network.MPD as MPD

import Control.Concurrent
import Data.Char
import Data.Default
import Data.Either

-- | command to run the program
mhmc :: IO ()
mhmc = do
    vty <- mkVty def
    currentEvent <- newEmptyMVar
    forkIO $ eventLoop vty currentEvent
    loop Help vty currentEvent

loop :: Screen -> Vty -> MVar Event -> IO ()
loop screen vty currentEvent = do
    status <- MPD.withMPD MPD.status
    (width, height) <- displayBounds $ outputIface vty
    update vty $ display (width, height) screen status
    e <- tryTakeMVar currentEvent
    case e of
        Just (EvKey (KChar '1') []) -> loop Help vty currentEvent
        Just (EvKey (KChar '2') []) -> loop Playlist vty currentEvent
        Just (EvKey (KChar '3') []) -> loop Browse vty currentEvent
        Just (EvKey (KChar '4') []) -> loop Search vty currentEvent
        Just (EvKey (KChar '5') []) -> loop Library vty currentEvent
        Just (EvKey (KChar '6') []) -> loop PlaylistEditor vty currentEvent
        Just (EvKey (KChar '7') []) -> loop MusicDir vty currentEvent
        Just (EvKey (KChar '8') []) -> loop Outputs vty currentEvent
        Just (EvKey (KChar '9') []) -> loop Visualizer vty currentEvent
        Just (EvKey (KChar '0') []) -> loop Clock vty currentEvent
        Just (EvKey (KChar 'q') _) -> shutdown vty
        Just (EvKey KLeft _) -> do
            MPD.withMPD $ MPD.setVolume $ if (getVolume status - 1) < 0 then 0 else (getVolume status - 1)
            loop screen vty currentEvent
        Just (EvKey KRight _) -> do
            MPD.withMPD $ MPD.setVolume $ if (getVolume status + 1) > 100 then 100 else (getVolume status + 1)
            loop screen vty currentEvent
        otherwise -> loop screen vty currentEvent