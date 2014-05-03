{-# LANGUAGE OverloadedStrings #-}

module MHMC
(
    mhmc
) where

import MHMCEvent

import Graphics.Vty
import Graphics.Vty.Prelude

import qualified Network.MPD as MPD

import Control.Concurrent
import Data.Default
import Data.Either

data Screen = Help
            | Playlist
            | Browse
            | Search
            | Library
            | PlaylistEditor
            | MusicDir
            | Outputs
            | Visualizer
            | Clock

instance Show Screen where
    show Help = "Help"
    show Playlist = "Playlist"
    show Browse = "Browse"
    show Search = "Search Engine"
    show Library = "Media Library"
    show PlaylistEditor = "Playlist Editor"
    show MusicDir = "Music Directory"
    show Outputs = "Outputs"
    show Visualizer = "Music Visualizer"
    show Clock = "Clock"

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
        Just (EvKey (KChar 'q') _) -> shutdown vty
        Just (EvKey KLeft _) -> do
            MPD.withMPD $ MPD.setVolume $ (getVolume status - 1)
            loop screen vty currentEvent
        Just (EvKey KRight _) -> do
            MPD.withMPD $ MPD.setVolume $ (getVolume status + 1)
            loop screen vty currentEvent
        otherwise -> loop screen vty currentEvent

display :: (Int, Int) -> Screen -> MPD.Response MPD.Status -> Picture
display (width, height) screen status =
    header width screen status

header :: Int -> Screen -> MPD.Response MPD.Status -> Picture
header width screen status =
    let title = string (def `withForeColor` brightBlack) $ show screen
        volume = string (def `withForeColor` blue) ("Volume: " ++ (show $ getVolume status) ++ "%")
        bar = string (def `withForeColor` brightBlack) $ take width $ repeat '―'
    in addToTop (picForImage title) $ displayRight volume <-> bar
    where displayRight image = translateX (width - imageWidth image) image

getVolume :: MPD.Response MPD.Status -> Int
getVolume = either (\_ -> 0) MPD.stVolume