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
    header width Help
display vty _ = emptyPicture

header :: Int -> Screen -> Picture
header width screen =
    let title = string (def `withForeColor` brightBlack) $ show screen
        volume = string (def `withForeColor` blue) "Volume: 79%"
    in addToTop (picForImage title) $ displayRight volume
    where displayRight image = translateX (width - imageWidth image) image