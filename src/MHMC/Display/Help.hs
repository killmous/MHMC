{-# LANGUAGE QuasiQuotes #-}

module MHMC.Display.Help
(
    helpinfo,
    help
) where

import Control.Monad.Trans.RWS.Lazy
import Data.Default
import Data.String.QQ
import Graphics.Vty
import MHMC.RWS

help :: Int -> Int -> Image
help height cursor = cropBottom (height - 4)
    $ pad 0 0 0 height
    $ foldl1 (<->)
    $ map (string (def `withForeColor` white))
    $ drop cursor
    $ lines helpinfo

helpinfo = [s|

    Keys - Movement
--------------------------------
        Up          : Move Cursor up
        Down        : Move Cursor down

        1           : Help screen
        2           : Playlist screen
        3           : Browse screen
        4           : Search engine
        5           : Media library
        6           : Playlist editor
        7           : Tag editor
        8           : Outputs
        9           : Music visualizer
        0           : Clock screen

        @           : MPD server info

    Keys - Global
--------------------------------
        s           : Stop
        P           : Pause/Play
        Backspace   : Play current track from the beginning

|]