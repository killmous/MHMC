module MHMC.MPD
(
    Playlist,
    getVolume,
    getState,
    playSong,
    removeSong,
    togglePlaying,
    nowPlaying,
    stopPlaying,
    currentSongPos,
    currentSongTime,
    getPlaylist,
    getPlaylistLength,
    incVolume,
    decVolume,
    clearQueue,
    getDirectory
) where

import Network.MPD
import Data.Map hiding (delete, map)
import Data.Maybe
import Data.String

type Playlist = [[(Metadata, [Value])]]

getVolume :: Response Status -> Int
getVolume = either (\_ -> -1) (fromMaybe (-1) . stVolume)

getState :: Response Status -> String
getState = either (\_ -> "Error") (show . stState)

playSong :: Int -> IO (Response ())
playSong = withMPD . play . Just

removeSong :: Int -> IO (Response ())
removeSong = withMPD . delete

togglePlaying :: Response Status -> IO (Response ())
togglePlaying status = withMPD $ do
    if getState status == "Playing"
        then pause True
        else play Nothing

nowPlaying :: IO (Maybe Song)
nowPlaying = (withMPD currentSong) >>= either (\_ -> return Nothing) return

stopPlaying :: IO (Response ())
stopPlaying = withMPD stop

currentSongPos :: Response Status -> Int
currentSongPos = either (\_ -> 0) (fromMaybe 0 . stSongPos)

currentSongTime :: Response Status -> (Double, Seconds)
currentSongTime = either (\_ -> (0,0)) (fromMaybe (0,0) . stTime)

getPlaylist :: IO Playlist
getPlaylist = (withMPD $ playlistInfo Nothing) >>= either (\_ -> return []) (return . map sgTags) >>= return . map toList

getPlaylistLength :: Response Status -> Int
getPlaylistLength = either (\_ -> 0) (fromInteger . stPlaylistLength)

incVolume :: Response Status -> IO ()
incVolume status = (withMPD $ setVolume $ if (getVolume status + 1) > 100 then 100 else (getVolume status + 1)) >> return ()

decVolume :: Response Status -> IO ()
decVolume status = (withMPD $ setVolume $ if (getVolume status - 1) < 0 then 0 else (getVolume status - 1)) >> return ()

clearQueue :: IO (Response())
clearQueue = withMPD clear

getDirectory :: Maybe String -> IO [LsResult]
getDirectory Nothing = (withMPD $ lsInfo (fromString "")) >>= either (\_ -> return []) return
getDirectory (Just path) = (withMPD $ lsInfo (fromString path)) >>= either (\_ -> return []) return