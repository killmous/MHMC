module MHMC.RWS
(
    Screen(..),
    MHMCReader(..),
    MHMC
) where

import Control.Concurrent
import Control.Monad.Trans.RWS.Lazy
import Graphics.Vty

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

data MHMCReader = MHMCReader {
    getVty :: Vty,
    getEvent :: MVar Event
}

type MHMC = RWST MHMCReader () Screen IO