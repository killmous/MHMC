{-# LANGUAGE OverloadedStrings #-}

module MHMC
(
    mhmc
) where

import Graphics.Vty
import Graphics.Vty.Prelude

import qualified Network.MPD as MPD

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
    eventLoop Help vty

eventLoop :: Screen -> Vty -> IO ()
eventLoop screen vty = do
    status <- MPD.withMPD MPD.status
    (width, height) <- displayBounds $ outputIface vty
    update vty $ display (width, height) screen status
    e <- nextEvent vty
    case e of
        EvKey (KChar 'q') _ -> shutdown vty
        EvKey KLeft _ -> do
            MPD.withMPD $ MPD.setVolume $ (getVolume status - 1)
            eventLoop screen vty
        EvKey KRight _ -> do
            MPD.withMPD $ MPD.setVolume $ (getVolume status + 1)
            eventLoop screen vty
        otherwise -> eventLoop screen vty

display :: (Int, Int) -> Screen -> MPD.Response MPD.Status -> Picture
display (width, height) screen status =
    header width screen status
display _ _ _ = emptyPicture

header :: Int -> Screen -> MPD.Response MPD.Status -> Picture
header width screen status =
    let title = string (def `withForeColor` brightBlack) $ show screen
        volume = string (def `withForeColor` blue) ("Volume: " ++ (show $ getVolume status) ++ "%")
    in addToTop (picForImage title) $ displayRight volume
    where displayRight image = translateX (width - imageWidth image) image

getVolume :: MPD.Response MPD.Status -> Int
getVolume = either (\_ -> 0) MPD.stVolume