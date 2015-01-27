module MHMC.Display.Browse
(
    browse,
    changeDirectory
) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.Lazy
import Data.Default
import qualified Data.Map as M
import Data.Maybe
import Graphics.Vty
import MHMC.MPD
import MHMC.RWS
import qualified Network.MPD as MPD

browse :: (Int, Int) -> Int -> [MPD.LsResult] -> Image
browse (width, height) cursor res =
    let cursorString = (take cursor $ repeat "") ++ ["->"] ++ (take height $ repeat "")
        cursorImg = vertCat $ map (string (def `withForeColor` white)) cursorString
    in cropBottom (height - 4)
        $ pad 0 0 0 height
        $ cursorImg <|> (vertCat
        $ map (string (def `withForeColor` white)) $ display res)

display :: [MPD.LsResult] -> [String]
display = map $ \val -> case val of
        MPD.LsDirectory dir -> "[" ++ (MPD.toString dir) ++ "]"
        MPD.LsPlaylist name -> MPD.toString name
        MPD.LsSong song     -> fromMaybe "" $ fmap (MPD.toString . (!! 0)) $ M.lookup MPD.Title $ MPD.sgTags song

changeDirectory :: [MPD.LsResult] -> Int -> MHMC ()
changeDirectory res pos = do
        state <- get
        vty <- asks getVty
        let path = map (\val -> case val of
                            MPD.LsDirectory dir -> Just $ MPD.toString dir
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