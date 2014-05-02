module MHMC
(
    mhmc
) where

import Graphics.Vty
import Graphics.Vty.Prelude

import qualified Network.MPD as MPD

import Data.Default

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
    (width, height) <- displayBounds $ outputIface vty
    update vty $ display (width, height) Help
    e <- nextEvent vty
    shutdown vty

display :: (Int, Int) -> Screen -> Picture
display (width, height) Help =
    let line0left = string (def `withForeColor` brightBlack) "Help"
        line0right = string (def `withForeColor` blue) "Volume: 79%"
    in addToTop (picForImage line0) $ translateX (width - imageWidth line0right) line0right
display vty _ = emptyPicture