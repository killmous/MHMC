{-# LANGUAGE QuasiQuotes #-}
module MHMC.Display
(
    Screen(..),
    Cursor(..),
    display
) where

import MHMC.MPD
import MHMC.Clock
import MHMC.RWS
import qualified Network.MPD as MPD
import Graphics.Vty hiding (Cursor(..))

import Control.Monad.IO.Class
import Control.Monad.Trans.RWS.Lazy
import Data.Default
import Data.Map (toList)
import Data.Maybe
import Data.String.QQ
import Text.Printf

import System.Time

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

display :: (Int, Int) -> MHMC Picture
display (width, height) = do
    status <- liftIO $ MPD.withMPD MPD.status
    displayContents <- contents (width, height) status
    song <- liftIO $ nowPlaying
    screen <- get
    let img = header width screen status <->
            displayContents <->
            footer width status song
    return $ picForImage img

header :: Int -> Screen -> MPD.Response MPD.Status -> Image
header width screen status =
    let title = string (def `withForeColor` white) $ show screen
        volume = string (def `withForeColor` blue) ("Volume: " ++ (show $ getVolume status) ++ "%")
        bar = string (def `withForeColor` white) $ take width $ repeat '―'
    in title <|> (translateX (0 - imageWidth title) $ displayRight volume) <-> bar
    where displayRight image = translateX (width - imageWidth image) image

contents :: (Int, Int) -> MPD.Response MPD.Status -> MHMC Image
contents (width, height) _ = do
    screen <- get
    case screen of
        Help        -> return $ cropBottom (height - 4)
            $ pad 0 0 0 height
            $ foldl1 (<->)
            $ map (string (def `withForeColor` white))
            $ lines helpinfo
        otherwise   -> return $ cropBottom (height - 4) $ pad 0 0 0 height emptyImage
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

footer :: Int -> MPD.Response MPD.Status -> Maybe MPD.Song -> Image
footer width status song =
    let (currentTime, totalTime) = currentSongTime status
        secondRowLeft = "[" ++ getState status ++ "]"
        secondRowMid = case song of
            Just name -> (string (def `withForeColor` red) $ MPD.toString $ (!! 0) $ fromMaybe [] $ lookup MPD.Artist songTags)
                <|> (string (def `withForeColor` white) " - ")
                <|> (string (def `withForeColor` blue) $ MPD.toString $ (!! 0) $ fromMaybe [] $ lookup MPD.Title songTags)
                <|> (string (def `withForeColor` white) " - ")
                <|> (string (def `withForeColor` green) $ MPD.toString $ (!! 0) $ fromMaybe [] $ lookup MPD.Album songTags)
                where songTags = toList $ MPD.sgTags name
            Nothing   -> emptyImage
        secondRowRight = case (currentTime, totalTime) of
            (0,0) -> ""
            (a,b) -> "[" ++ (minutes $ fromEnum a) ++ "/" ++ minutes b ++ "]"
        firstRow = take width $ repeat '―'
        secondRow = string (def `withForeColor` white) secondRowLeft
            <|> (cropRight (width - length secondRowRight - length secondRowLeft)
                $ pad 0 0 width 0
                $ secondRowMid)
            <|> string (def `withForeColor` white) secondRowRight
    in string (def `withForeColor` yellow) firstRow <-> secondRow
    where minutes time = (show $ div time 60) ++ ":" ++ (printf "%.2d" $ mod time 60)

reverseColors :: Attr -> Attr
reverseColors attr = attr { attrBackColor = attrForeColor attr,
                            attrForeColor = attrBackColor attr}