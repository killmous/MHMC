{-# LANGUAGE QuasiQuotes #-}
module MHMC.Display
(
    Screen(..),
    Cursor(..),
    display
) where

import MHMC.MPD
import MHMC.Clock
import qualified Network.MPD as MPD
import Graphics.Vty hiding (Cursor(..))

import Data.Default
import Data.Maybe
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

data Cursor = Cursor {val :: Int}

display :: (Int, Int) -> Screen -> Cursor -> IO (Picture, Cursor)
display (width, height) screen cursor = do
    status <- MPD.withMPD MPD.status
    (displayContents, realcursor) <- contents (width, height) screen status cursor
    let img = header width screen status <->
            displayContents <->
            footer width screen status
    return (picForImage img, realcursor)

header :: Int -> Screen -> MPD.Response MPD.Status -> Image
header width screen status =
    let title = string (def `withForeColor` white) $ show screen
        volume = string (def `withForeColor` blue) ("Volume: " ++ (show $ getVolume status) ++ "%")
        bar = string (def `withForeColor` white) $ take width $ repeat '―'
    in title <|> (translateX (0 - imageWidth title) $ displayRight volume) <-> bar
    where displayRight image = translateX (width - imageWidth image) image

contents :: (Int, Int) -> Screen -> MPD.Response MPD.Status -> Cursor -> IO (Image, Cursor)
contents (width, height) Help status (Cursor cursor) =
    let maxcursor = (length $ lines helpinfo) - (height - 4)
        realcursor = if cursor > maxcursor then maxcursor else cursor
    in return (cropBottom (height - 4)
        $ pad 0 0 0 height
        $ foldl1 (<->)
        $ map (string (def `withForeColor` white))
        $ drop realcursor
        $ lines helpinfo,
        Cursor realcursor)
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
contents (width, height) Playlist status (Cursor cursor) = do
    hash <- getPlaylist
    let cursorString = (take cursor $ repeat "") ++ ["->"]
        cursorImg = foldl1 (<->) $ map (string (def `withForeColor` blue)) cursorString
        artists = map (lookup MPD.Artist) hash
        artistsString = map (MPD.toString . (!! 0) . fromMaybe []) artists
        artistsImg = foldl1 (<->) $ map (string (def `withForeColor` red)) str
            where str = if null artistsString then ["Empty Playlist"] else artistsString
        titles = map (lookup MPD.Title) hash
        titlesString = map (MPD.toString . (!! 0) . fromMaybe []) titles
        titlesImg = foldl1 (<->) $ map (string (def `withForeColor` blue)) str
            where str = if null titlesString then ["Empty Playlist"] else titlesString
        albums = map (lookup MPD.Album) hash
        albumsString = map (MPD.toString . (!! 0) . fromMaybe []) albums
        albumsImg = foldl1 (<->) $ map (string (def `withForeColor` green)) str
            where str = if null albumsString then ["Empty Playlist"] else albumsString

    let maxcursor = getPlaylistLength status - 1
        realcursor = if cursor > maxcursor then maxcursor else cursor
    return (cropBottom (height - 4) $ pad 0 0 0 height (cursorImg <|> (sizeof 0.25 artistsImg) <|> (sizeof 0.5 titlesImg) <|> (sizeof 0.25 albumsImg)), Cursor realcursor)
    where sizeof frac image = cropRight (fromEnum (fromIntegral width * frac)) $ pad 0 0 (fromEnum (fromIntegral width * frac)) 0 image
contents (width, height) Clock _ _ = do
    time <- getClockTime >>= toCalendarTime
    let img = clock time
    return (cropBottom (height - 4)
        $ pad 0 0 0 height
        $ translate ((width - imageWidth img - 2) `div` 2) ((height - imageHeight img - 2) `div` 2) img
        , Cursor 0)
contents (width, height) _ _ _ = return (cropBottom (height - 4) $ pad 0 0 0 height emptyImage, Cursor 0)

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

reverseColors :: Attr -> Attr
reverseColors attr = attr { attrBackColor = attrForeColor attr,
                            attrForeColor = attrBackColor attr}