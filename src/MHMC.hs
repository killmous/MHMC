{-# LANGUAGE OverloadedStrings #-}

module MHMC
(
    mhmc
) where

import MHMC.Display
import MHMC.Event
import MHMC.MPD
import MHMC.RWS

import Graphics.Vty hiding (Cursor(..))
import Graphics.Vty.Prelude

import qualified Network.MPD as MPD

import Control.Applicative
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Trans.RWS.Lazy
import Data.Char
import Data.Default
import Data.Either
import Data.Maybe

-- | command to run the program
mhmc :: IO ()
mhmc = do
    vty <- mkVty def
    currentEvent <- newEmptyMVar
    forkIO $ eventLoop vty currentEvent
    let state = MHMCReader vty currentEvent
    (\_ -> ()) <$> execRWST loop state Playlist

loop :: MHMC ()
loop = do
    status <- liftIO $ MPD.withMPD MPD.status
    e <- asks getEvent >>= liftIO . tryTakeMVar
    vty <- asks getVty
    (width, height) <- liftIO $ displayBounds $ outputIface vty
    pic <- display (width, height)
    liftIO $ update vty pic
    case e of
        Just (EvKey (KChar 'q') []) -> liftIO $ shutdown vty
        otherwise                   -> loop