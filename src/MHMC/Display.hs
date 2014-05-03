module MHMC.Display
(
	Screen(..),
	display
) where

import MHMC.MPD
import qualified Network.MPD as MPD
import Graphics.Vty

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
            deriving (Enum)

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