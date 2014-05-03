module MHMC.MPD
(
	getVolume
) where

import qualified Network.MPD as MPD

getVolume :: MPD.Response MPD.Status -> Int
getVolume = either (\_ -> 0) MPD.stVolume