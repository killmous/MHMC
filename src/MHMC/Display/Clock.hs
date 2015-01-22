module MHMC.Display.Clock
(
    clock
) where

import Control.Monad.Trans.RWS.Lazy
import Data.Default
import Graphics.Vty
import MHMC.RWS
import System.Time
import Text.Printf

clock :: (Int, Int) -> CalendarTime -> Image
clock (width, height) time =
    let top = string (def `withForeColor` white)   "┌───────────────────┐"
        mid = string (def `withForeColor` white) $ "│       " ++ now ++ "       │"
        bot = string (def `withForeColor` white)   "└───────────────────┘"
        img = top <-> mid <-> bot
    in cropBottom (height - 4)
     $ pad 0 0 0 height
     $ translate ((width - imageWidth img - 2) `div` 2) ((height - imageHeight img - 2) `div` 2) img
    where now = (printf "%.2d" $ ctHour time) ++ ":" ++ (printf "%.2d" $ ctMin time)