module MHMC.Display.Browse
(
    browse,
    changeDirectory
) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.Lazy
import Data.Default
import Graphics.Vty
import MHMC.MPD
import MHMC.RWS
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
        LsPlaylist name -> toString name
        otherwise       -> "IT'S A SONG"

changeDirectory :: [LsResult] -> Int -> MHMC ()
changeDirectory res pos = do
        state <- get
        vty <- asks getVty
        let path = map (\val -> case val of
                            LsDirectory dir -> Just $ toString dir
                            otherwise -> Nothing)
                        res !! pos
        (width, height) <- lift $ displayBounds $ outputIface vty
        dirLength <- lift $ fmap length $ getDirectory path
        let maxcursor = min (height - 4) dirLength - 1
        put $ state {
            getCursor = 0,
            getMaxCursor = maxcursor,
            getScroll = 0,
            getMaxScroll = max 0 $ dirLength - (height - 4),
            getPath = path
        }