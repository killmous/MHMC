module MHMC.MPD
(
    getVolume,
    getState,
    togglePlaying,
    nowPlaying,
    currentSongTime,
    getPlaylist,
    getPlaylistLength,
    --getDirectory
) where

import Network.MPD
import Data.Map hiding (map)
import Data.Maybe

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

getPlaylist :: IO [[(Metadata, [Value])]]
getPlaylist = (withMPD $ playlistInfo Nothing) >>= either (\_ -> return []) (return . map sgTags) >>= return . map toList

getPlaylistLength :: Response Status -> Int
getPlaylistLength = either (\_ -> 0) (fromInteger . stPlaylistLength)

{-getDirectory :: Maybe String -> IO [Path]
getDirectory Nothing = (withMPD $ listAll (Path "")) >>= either (\_ -> return []) return
getDirectory (Just path) = (withMPD $ listAll (Path path)) >>= either (\_ -> return []) return-}