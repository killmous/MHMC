{-# LANGUAGE OverloadedStrings #-}

module MHMC
(
    mhmc
) where

import MHMC.Display
import MHMC.Event
import MHMC.MPD

import Graphics.Vty hiding (Cursor(..))
import Graphics.Vty.Prelude

import qualified Network.MPD as MPD

import Control.Concurrent
import Data.Char
import Data.Default
import Data.Either
import Data.Maybe

-- | command to run the program
mhmc :: IO ()
mhmc = do
    vty <- mkVty def
    status <- MPD.withMPD MPD.status
    currentEvent <- newEmptyMVar
    forkIO $ eventLoop vty currentEvent
    loop Playlist vty currentEvent (Cursor $ fromMaybe 0 (either (\_ -> Just 0) MPD.stSongPos status))

loop :: Screen -> Vty -> MVar Event -> Cursor -> IO ()
loop screen vty currentEvent cursor = do
    status <- MPD.withMPD MPD.status
    (width, height) <- displayBounds $ outputIface vty
    (pic, newcursor) <- display (width, height) screen cursor
    update vty pic
    e <- tryTakeMVar currentEvent
    case e of
        Just (EvKey (KChar '1') [])     -> loop Help vty currentEvent (Cursor 0)
        Just (EvKey (KChar '2') [])     -> loop Playlist vty currentEvent (Cursor $ fromMaybe 0 (either (\_ -> Just 0) MPD.stSongPos status))
        Just (EvKey (KChar '3') [])     -> loop Browse vty currentEvent (Cursor 0)
        Just (EvKey (KChar '4') [])     -> loop Search vty currentEvent (Cursor 0)
        Just (EvKey (KChar '5') [])     -> loop Library vty currentEvent (Cursor 0)
        Just (EvKey (KChar '6') [])     -> loop PlaylistEditor vty currentEvent (Cursor 0)
        Just (EvKey (KChar '7') [])     -> loop MusicDir vty currentEvent (Cursor 0)
        Just (EvKey (KChar '8') [])     -> loop Outputs vty currentEvent (Cursor 0)
        Just (EvKey (KChar '9') [])     -> loop Visualizer vty currentEvent (Cursor 0)
        Just (EvKey (KChar '0') [])     -> loop Clock vty currentEvent (Cursor 0)
        Just (EvKey (KChar 'q') [])     -> shutdown vty
        Just (EvKey (KChar 'P') [])     -> do
            MPD.withMPD $ togglePlaying status
            loop screen vty currentEvent newcursor
        Just (EvKey (KChar 's') [])     -> do
            MPD.withMPD $ MPD.stop
            loop screen vty currentEvent newcursor
        Just (EvKey KBS [])             -> do
            MPD.withMPD $ MPD.play $ Just 0
            loop screen vty currentEvent newcursor
        Just (EvKey KLeft [])           -> do
            MPD.withMPD $ MPD.setVolume $ if (getVolume status - 1) < 0 then 0 else (getVolume status - 1)
            loop screen vty currentEvent newcursor
        Just (EvKey KRight [])          -> do
            MPD.withMPD $ MPD.setVolume $ if (getVolume status + 1) > 100 then 100 else (getVolume status + 1)
            loop screen vty currentEvent newcursor
        Just (EvKey KDown [])           -> case screen of
            Help      -> loop screen vty currentEvent (Cursor (val newcursor + 1))
            Playlist  -> loop screen vty currentEvent (Cursor (val newcursor + 1))
            otherwise -> loop screen vty currentEvent (Cursor 0)
        Just (EvKey KUp [])             -> case screen of
            Help      -> loop screen vty currentEvent (Cursor (if val newcursor == 0 then 0 else val newcursor - 1))
            Playlist  -> loop screen vty currentEvent (Cursor (if val newcursor == 0 then 0 else val newcursor - 1))
            otherwise -> loop screen vty currentEvent (Cursor 0)
        otherwise                       -> loop screen vty currentEvent newcursor