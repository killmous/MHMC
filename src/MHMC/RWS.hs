module MHMC.RWS
(
    Screen(..),
    MHMCReader(..),
    MHMCState(..),
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

data MHMCState = MHMCState {
    getScreen :: Screen,
    getCursor :: Int,
    getMaxCursor :: Int,
    getScroll :: Int,
    getMaxScroll :: Int,
    getPath :: Maybe String
}

type MHMC = RWST MHMCReader () MHMCState IO