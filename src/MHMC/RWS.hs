module MHMC.RWS
(
    MHMC,
    MHMCReader(..)
) where

import Control.Concurrent
import Control.Monad.Trans.RWS.Lazy
import Graphics.Vty
import MHMC.Display

data MHMCReader = MHMCReader {
    getVty :: Vty,
    getEvent :: MVar Event
}

type MHMC = RWST MHMCReader () Screen IO