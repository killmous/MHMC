module MHMC.Display
(
    Screen(..),
    display
) where

import MHMC.MPD
import MHMC.Display.Clock
import MHMC.Display.Help
import MHMC.RWS
import qualified Network.MPD as MPD
import Graphics.Vty

import Control.Monad.IO.Class
import Control.Monad.Trans.RWS.Lazy
import Data.Default
import Data.Map (toList)
import Data.Maybe
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

display :: (Int, Int) -> MHMC Picture
display (width, height) = do
    status <- liftIO $ MPD.withMPD MPD.status
    displayContents <- contents (width, height) status
    song <- liftIO $ nowPlaying
    screen <- gets getScreen
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
    screen <- gets getScreen
    case screen of
        Help        -> return $ help height
        Clock       -> liftIO $ fmap (clock (width, height)) $ getClockTime >>= toCalendarTime
        otherwise   -> return $ cropBottom (height - 4) $ pad 0 0 0 height emptyImage

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