module Main where 

import qualified Sound.TagLib as TagLib
import Data.Maybe
import Control.Monad
import System

main = do
    args <- getArgs 
    mapM showFile args

withMaybe :: (Maybe j) -> (j -> IO ()) -> IO () 
withMaybe mebbe action = do
    case mebbe of 
        Just x -> do action x 
                     return ()
        Nothing -> return ()

showFile filename = do
    t <- TagLib.openTagFile filename
    withMaybe t showTagFile

showTagFile tagFile = do
    t <- TagLib.openTag tagFile
    withMaybe t showTag

showTag tag = do 
    artist <- TagLib.artist tag
    album <- TagLib.album tag
    title <- TagLib.title tag
    comment <- TagLib.comment tag
    year <- TagLib.year tag
    track <- TagLib.track tag
    print (artist, album, title, year, track)
    TagLib.setArtist tag "Blah"

--    setArtist tag "blah"
--    setAlbum tag album
--    setTitle tag title
--    setComment tag comment
--        saveTagFile file
--        p <- getAudioProperties file
--        case p of 
--            Nothing -> return () 
--            Just props -> do
--                bitrate <- getBitrate props
--                length <- getLength props
--                samplerate <- getSampleRate props
--                channels <- getChannels props
--                print bitrate
--                print length
--                print channels
--                print samplerate

