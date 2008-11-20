module Main where 
import HTagLib
import Data.Maybe
import Control.Monad
import System

main = do
    args <- getArgs 
    mapM showFile args

showFile filename = do
    f <- openTagFile filename
    case f of 
        Nothing -> return () 
        Just file -> do 
            t <- getTag file
            case t of 
                Just tag -> do artist <- getArtist tag
                               album <- getAlbum tag
                               title <- getTitle tag
                               comment <- getComment tag
                               year <- getYear tag
                               track <- getTrack tag
                               print (artist, album, title, year, track)
                               setArtist tag "blah"
                               setAlbum tag album
                               setTitle tag title
                               setComment tag comment
            saveTagFile file
            p <- getAudioProperties file
            case p of 
                Nothing -> return () 
                Just props -> do
                    bitrate <- getBitrate props
                    length <- getLength props
                    samplerate <- getSampleRate props
                    channels <- getChannels props
                    print bitrate
                    print length
                    print channels
                    print samplerate
    return ()

