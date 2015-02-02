module MHMC.Display.Playlist
(
    playlist
) where

import Data.Default
import Data.Foldable (toList)
import Data.Maybe
import qualified Data.Sequence as S
import Graphics.Vty
import MHMC.MPD
import qualified Network.MPD as MPD

playlist :: (Int, Int) -> Playlist -> MPD.Response MPD.Status -> Int -> Int -> Image
playlist (width, height) hash status cursor songpos =
    let cursorString = (take cursor $ repeat "") ++ ["->"] ++ (take height $ repeat "")
        cursorImg = vertCat $ map (string (def `withForeColor` white)) cursorString
        artists = map (lookup MPD.Artist) hash
        artistsString = S.fromList $ map (fromMaybe [] . fmap (MPD.toString . (!! 0))) artists
        artistsImg = vertCat . toList $ S.mapWithIndex
            (\line str -> string (test line (def `withForeColor` white)) str) formatStr
            where formatStr = if S.null artistsString then S.singleton "Empty Playlist" else artistsString
        titles = map (lookup MPD.Title) hash
        titlesString = S.fromList $ map (fromMaybe [] . fmap (MPD.toString . (!! 0))) titles
        titlesImg = vertCat . toList $ S.mapWithIndex
            (\line str -> string (test line (def `withForeColor` white)) str) formatStr
            where formatStr = if S.null artistsString then S.singleton "Empty Playlist" else titlesString
        albums = map (lookup MPD.Album) hash
        albumsString = S.fromList $ map (fromMaybe [] . fmap (MPD.toString . (!! 0))) albums
        albumsImg = vertCat . toList $ S.mapWithIndex
            (\line str -> string (test line (def `withForeColor` white)) str) formatStr
            where formatStr = if S.null artistsString then S.singleton "Empty Playlist" else albumsString
    in cropBottom (height - 4)
        $ pad 0 0 0 height (cursorImg
            <|> (sizeof 0.25 artistsImg)
            <|> (string def " ")
            <|> (sizeof 0.5 titlesImg)
            <|> (string def " ")
            <|> (sizeof 0.25 albumsImg))
    where sizeof frac image = cropRight (fromEnum (fromIntegral width * frac)) $ pad 0 0 (fromEnum (fromIntegral width * frac)) 0 image
          test line = if line == songpos then \attr -> withStyle attr bold else id

reverseColors :: Attr -> Attr
reverseColors attr = attr { attrBackColor = attrForeColor attr,
                            attrForeColor = attrBackColor attr}