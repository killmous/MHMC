module MHMC.MPD
(
    getVolume,
    getState,
    togglePlaying,
    nowPlaying,
    currentSongTime,
    getPlaylist,
    getPlaylistLength,
    incVolume,
    decVolume,
    Playlist
    --getDirectory
) where

import Network.MPD
import Data.Map hiding (map)
import Data.Maybe

type Playlist = [[(Metadata, [Value])]]

getVolume :: Response Status -> Int
getVolume = either (\_ -> -1) (fromMaybe (-1) . stVolume)

getState :: Response Status -> String
getState = either (\_ -> "Error") (show . stState)

togglePlaying :: (MonadMPD m) => Response Status -> m ()
togglePlaying status = do
    if getState status == "Playing"
        then pause True
        else play Nothing

nowPlaying :: IO (Maybe Song)
nowPlaying = (withMPD currentSong) >>= either (\_ -> return Nothing) return

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

{-getDirectory :: Maybe String -> IO [Path]
getDirectory Nothing = (withMPD $ listAll (Path "")) >>= either (\_ -> return []) return
getDirectory (Just path) = (withMPD $ listAll (Path path)) >>= either (\_ -> return []) return-}