module MHMC.Display.Browse
(
    browse
) where

import Data.Default
import Graphics.Vty
import MHMC.MPD
import Network.MPD (LsResult(..), toString)

browse :: (Int, Int) -> Int -> [LsResult] -> Image
browse (width, height) cursor res =
    let cursorString = (take cursor $ repeat "") ++ ["->"] ++ (take height $ repeat "")
        cursorImg = vertCat $ map (string (def `withForeColor` white)) cursorString
    in cropBottom (height - 4)
        $ pad 0 0 0 height
        $ cursorImg <|> (vertCat
        $ map (string (def `withForeColor` white)) $ display res)

display :: [LsResult] -> [String]
display = map $ \val -> case val of
        LsDirectory dir -> "[" ++ (toString dir) ++ "]"
        otherwise       -> "..."