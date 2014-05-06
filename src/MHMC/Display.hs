{-# LANGUAGE QuasiQuotes #-}
module MHMC.Display
(
    Screen(..),
    display
) where

import MHMC.MPD
import MHMC.Clock
import qualified Network.MPD as MPD
import Graphics.Vty

import Data.Default
import Data.String.QQ
import Text.Printf

import System.Time

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
            deriving (Eq, Enum)

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

display :: (Int, Int) -> Screen -> IO Picture
display (width, height) screen = do
    status <- MPD.withMPD MPD.status
    displayContents <- contents (width, height) screen status
    let img = header width screen status <->
            displayContents <->
            footer width screen status
    return $ picForImage img

header :: Int -> Screen -> MPD.Response MPD.Status -> Image
header width screen status =
    let title = string (def `withForeColor` white) $ show screen
        volume = string (def `withForeColor` blue) ("Volume: " ++ (show $ getVolume status) ++ "%")
        bar = string (def `withForeColor` white) $ take width $ repeat '―'
    in title <|> (translateX (0 - imageWidth title) $ displayRight volume) <-> bar
    where displayRight image = translateX (width - imageWidth image) image

contents :: (Int, Int) -> Screen -> MPD.Response MPD.Status -> IO Image
contents (width, height) Help status = return $ cropBottom (height - 4)
    $ pad 0 0 0 height
    $ foldl1 (<->)
    $ map (string (def `withForeColor` white))
    $ lines helpinfo
    where helpinfo = [s|

    Keys - Movement
--------------------------------
        Up          : Move Cursor up
        Down        : Move Cursor down

        1           : Help screen
        2           : Playlist screen
        3           : Browse screen
        4           : Search engine
        5           : Media library
        6           : Playlist editor
        7           : Tag editor
        8           : Outputs
        9           : Music visualizer
        0           : Clock screen

        @           : MPD server info

    Keys - Global
--------------------------------
        s           : Stop
        P           : Pause/Play
        Backspace   : Play current track from the beginning

|]
contents (width, height) Clock _ = do
    time <- getClockTime >>= toCalendarTime
    let img = clock time
    return $ cropBottom (height - 4)
        $ pad 0 0 0 height
        $ translate ((width - imageWidth img - 2) `div` 2) ((height - imageHeight img - 2) `div` 2) img
contents (width, height) _ _ =  return $ cropBottom (height - 4) $ pad 0 0 0 height emptyImage

footer :: Int -> Screen -> MPD.Response MPD.Status -> Image
footer width screen status =
    let (currentTime, totalTime) = currentSongTime status
        secondRowLeft = "[" ++ getState status ++ "]"
        secondRowRight = case (currentTime, totalTime) of
            (0,0) -> ""
            (a,b) -> "[" ++ (minutes $ fromEnum a) ++ "/" ++ minutes b ++ "]"
        firstRow = take width $ repeat '―'
        secondRow = string (def `withForeColor` white) secondRowLeft
            <|> (translateX (0 - length secondRowLeft)
            $ (displayRight $ string (def `withForeColor` white) secondRowRight))
    in string (def `withForeColor` yellow) firstRow <-> secondRow
    where displayRight image = translateX (width - imageWidth image) image
          minutes time = (show $ div time 60) ++ ":" ++ (printf "%.2d" $ mod time 60)