module MHMC.MPD
(
	getVolume,
	getState,
	togglePlaying
) where

import qualified Network.MPD as MPD

getVolume :: MPD.Response MPD.Status -> Int
getVolume = either (\_ -> -1) MPD.stVolume

getState :: MPD.Response MPD.Status -> String
getState = either (\_ -> "Error") (show . MPD.stState)

togglePlaying :: (MPD.MonadMPD m) => MPD.Response MPD.Status -> m ()
togglePlaying status = do
	if getState status == "Playing"
		then MPD.pause True
		else MPD.play Nothing