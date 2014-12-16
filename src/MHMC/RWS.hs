module MHMC.RWS
(
    Screen(..),
    MHMCReader(..),
    MHMCState(..),
    MHMC,
    setScreen,
    incCursor,
    decCursor
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
    getCursor :: Int
}

setScreen :: Screen -> MHMC ()
setScreen screen = do
    cursor <- gets getCursor
    put $ MHMCState screen cursor

incCursor :: MHMC ()
incCursor = do
    cursor <- gets getCursor
    screen <- gets getScreen
    put $ MHMCState screen (cursor + 1)

decCursor :: MHMC ()
decCursor = do
    cursor <- gets getCursor
    screen <- gets getScreen
    put $ MHMCState screen (cursor - 1)

type MHMC = RWST MHMCReader () MHMCState IO