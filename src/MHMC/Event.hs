module MHMC.Event
(
    eventLoop
) where

import Control.Concurrent
import Graphics.Vty
import MHMC.RWS

eventLoop :: MHMCReader -> IO ()
eventLoop reader = do
    let vty = getVty reader
    let currentEvent = getEvent reader
    e <- nextEvent vty
    tryPutMVar currentEvent e
    eventLoop reader

