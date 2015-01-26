module MHMC.Display
(
    Screen(..),
    setScreen,
    display
) where

import MHMC.MPD
import MHMC.Display.Help
import MHMC.Display.Playlist
import MHMC.Display.Browse
import MHMC.Display.Clock
import MHMC.RWS

import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.Lazy
import Data.Default
import Data.Map (toList)
import Data.Maybe
import Graphics.Vty
import qualified Network.MPD as MPD
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
    status <- lift $ MPD.withMPD MPD.status
    displayContents <- contents (width, height) status
    song <- lift $ nowPlaying
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
contents (width, height) status = do
    screen <- gets getScreen
    cursor <- gets getCursor
    scroll <- gets getScroll
    case screen of
        Help        -> return $ help height scroll
        Playlist    -> do
            hash    <- lift $ fmap (drop scroll) getPlaylist
            return $ playlist (width, height) hash status cursor (currentSongPos status - scroll)
        Browse      -> lift $ fmap (browse (width, height) cursor) $ fmap (drop scroll) $ getDirectory Nothing
        Clock       -> lift $ fmap (clock (width, height)) $ getClockTime >>= toCalendarTime
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

setScreen :: Screen -> MHMC ()
setScreen Help = do
    vty <- asks getVty
    (width, height) <- lift $ displayBounds $ outputIface vty
    put $ MHMCState {
        getScreen = Help,
        getCursor = 0,
        getMaxCursor = 0,
        getScroll = 0,
        getMaxScroll = (length $ lines helpinfo) - (height - 4)
    }

setScreen Playlist = do
    vty <- asks getVty
    (width, height) <- lift $ displayBounds $ outputIface vty
    status <- lift $ MPD.withMPD MPD.status
    let pos = currentSongPos status
    let maxcursor = min (height - 4) (getPlaylistLength status) - 1
    put $ MHMCState {
        getScreen = Playlist,
        getCursor = min pos maxcursor,
        getMaxCursor = maxcursor,
        getScroll = max 0 (pos - maxcursor),
        getMaxScroll = max 0 ((getPlaylistLength status) - (height - 4))
    }

setScreen Browse = do
    vty <- asks getVty
    (width, height) <- lift $ displayBounds $ outputIface vty
    status <- lift $ MPD.withMPD MPD.status
    dirLength <- lift $ fmap length $ getDirectory Nothing
    let maxcursor = min (height - 4) dirLength - 1
    put $ MHMCState {
        getScreen = Browse,
        getCursor = 0,
        getMaxCursor = maxcursor,
        getScroll = 0,
        getMaxScroll = dirLength - (height - 4)
    }

setScreen screen = put $ MHMCState screen 0 0 0 0