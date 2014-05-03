module MHMCEvent
(
	eventLoop
) where

import Control.Concurrent
import Graphics.Vty

eventLoop :: Vty -> MVar Event -> IO ()
eventLoop vty currentEvent = do
    e <- nextEvent vty
    tryPutMVar currentEvent e
    eventLoop vty currentEvent