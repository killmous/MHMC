module MHMC.Clock
(
    clock
) where

import Graphics.Vty
import Data.Default
import System.Time
import Text.Printf

clock :: CalendarTime -> Image
clock time =
    let top = string (def `withForeColor` white)   "┌───────────────────┐"
        mid = string (def `withForeColor` white) $ "│       " ++ now ++ "       │"
        bot = string (def `withForeColor` white)   "└───────────────────┘"
    in top <-> mid <-> bot
    where now = (printf "%.2d" $ ctHour time) ++ ":" ++ (printf "%.2d" $ ctMin time)