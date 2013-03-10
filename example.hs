module Main where 

import qualified Sound.TagLib as TagLib
import Control.Monad (mapM)
import System.Environment (getArgs)

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
    t <- TagLib.open filename
    withMaybe t showTagFile

showTagFile :: TagLib.TagFile -> IO ()
showTagFile tagFile = do
    t <- TagLib.tag tagFile
    withMaybe t showTag
    p <- TagLib.audioProperties tagFile
    withMaybe p showAudioProperties 

showTag :: TagLib.Tag -> IO ()
showTag tag = do 
    artist <- TagLib.artist tag
    album <- TagLib.album tag
    title <- TagLib.title tag
    comment <- TagLib.comment tag
    year <- TagLib.year tag
    track <- TagLib.track tag
    print (artist, album, title, year, track)
    {--
    TagLib.setArtist tag "Blah"
    TagLib.setArtist tag "blah"
    TagLib.setAlbum tag album
    TagLib.setTitle tag title
    TagLib.setComment tag comment
    --}

showAudioProperties :: TagLib.AudioProperties -> IO ()
showAudioProperties props = do
    bitrate <- TagLib.bitRate props
    length <- TagLib.duration props
    samplerate <- TagLib.sampleRate props
    channels <- TagLib.channels props
    print (bitrate, length, channels, samplerate)

