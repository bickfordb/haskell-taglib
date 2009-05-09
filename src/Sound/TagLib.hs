
--------------------------------------------------------------------------------
-- |
-- Module     : Sound.TagLib
-- Copyright  : (c) Brandon Bickford 2008
-- License    : LGPL v3
-- Maintainer: Brandon Bickford <bickfordb@gmail.com>
-- Stability  : experimental
-- Portability  : only tested with GHC
--
-- High level interface to read and write ID3 tag fields (album, artist,
-- comment, genre, title, track number, year) and get audio properties (length,
-- bit rate, sample rate, channels)
-- 
--------------------------------------------------------------------------------
module Sound.TagLib (
    -- * Data Types
    AudioProperties,
    Tag,
    TagFile,

    -- * TagFile operations
    open,
    save,
    
    -- * Tag Operations
    tag,
    album,
    artist, 
    comment,
    genre,
    setAlbum,
    setArtist,
    setComment,
    setGenre,
    setTitle,
    setTrack,
    setYear,
    title,
    track,
    year,

    -- * AudioProperties Operations
    audioProperties,
    bitRate,
    channels,
    duration,
    sampleRate

    -- * Example
    -- $example
) where

import Foreign
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import System.IO
import System.IO.Unsafe
import qualified Codec.Binary.UTF8.String as UTF8
import Control.Monad

import qualified Data.ByteString as B
import Foreign.Marshal.Array (withArray)

type Void = Word8
type TagFile = ForeignPtr Void
type TagFileRef = Ptr Void 
data AudioProperties = AudioProperties TagFile AudioPropertiesRef

type AudioPropertiesRef = Ptr Void
type TagRef = Ptr Void
data Tag = Tag TagFile TagRef
type UTF8String = Ptr Word8

foreign import ccall unsafe "taglib/tag_c.h taglib_file_new" taglib_file_new :: UTF8String -> IO TagFileRef
foreign import ccall unsafe "taglib/tag_c.h &taglib_file_free" taglib_file_free :: FunPtr (TagFileRef -> IO ())
foreign import ccall unsafe "taglib/tag_c.h taglib_file_tag" taglib_file_tag :: TagFileRef -> IO TagRef

{- Tag Getters -}
foreign import ccall unsafe "taglib/tag_c.h taglib_tag_artist" taglib_tag_artist :: TagRef -> IO UTF8String
foreign import ccall unsafe "taglib/tag_c.h taglib_tag_album" taglib_tag_album :: TagRef -> IO UTF8String
foreign import ccall unsafe "taglib/tag_c.h taglib_tag_title" taglib_tag_title :: TagRef -> IO UTF8String
foreign import ccall unsafe "taglib/tag_c.h taglib_tag_comment" taglib_tag_comment :: TagRef -> IO UTF8String
foreign import ccall unsafe "taglib/tag_c.h taglib_tag_genre" taglib_tag_genre :: TagRef -> IO UTF8String
foreign import ccall unsafe "taglib/tag_c.h taglib_tag_year" taglib_tag_year :: TagRef -> IO CUInt
foreign import ccall unsafe "taglib/tag_c.h taglib_tag_track" taglib_tag_track :: TagRef -> IO CUInt
-- We manage our strings, so we don't need this guy
--foreign import ccall unsafe "taglib/tag_c.h taglib_tag_free_strings" taglib_tag_free_strings :: IO ()
foreign import ccall unsafe "taglib/tag_c.h taglib_file_save" taglib_file_save :: TagFileRef -> IO CInt
foreign import ccall unsafe "taglib/tag_c.h taglib_set_string_management_enabled" taglib_set_string_management_enabled :: CInt -> IO ()

{- Audio properties -}
foreign import ccall unsafe "taglib/tag_c.h taglib_file_audioproperties" taglib_file_audioproperties :: TagFileRef -> IO AudioPropertiesRef
foreign import ccall unsafe "taglib/tac_c.h taglib_audioproperties_length" taglib_audioproperties_length :: AudioPropertiesRef -> IO CInt
foreign import ccall unsafe "taglib/tac_c.h taglib_audioproperties_bitrate" taglib_audioproperties_bitrate :: AudioPropertiesRef -> IO CInt 
foreign import ccall unsafe "taglib/tac_c.h taglib_audioproperties_samplerate" taglib_audioproperties_samplerate :: AudioPropertiesRef -> IO CInt 
foreign import ccall unsafe "taglib/tac_c.h taglib_audioproperties_channels" taglib_audioproperties_channels :: AudioPropertiesRef -> IO CInt 

{- Tag Setters -}
foreign import ccall unsafe "taglib/tac_c.h taglib_tag_set_track" taglib_tag_set_track :: TagRef -> CUInt -> IO () 
foreign import ccall unsafe "taglib/tac_c.h taglib_tag_set_year" taglib_tag_set_year :: TagRef -> CUInt -> IO () 
foreign import ccall unsafe "taglib/tac_c.h taglib_tag_set_genre" taglib_tag_set_genre :: TagRef -> UTF8String -> IO () 
foreign import ccall unsafe "taglib/tac_c.h taglib_tag_set_comment" taglib_tag_set_comment :: TagRef -> UTF8String -> IO () 
foreign import ccall unsafe "taglib/tac_c.h taglib_tag_set_album" taglib_tag_set_album :: TagRef -> UTF8String -> IO () 
foreign import ccall unsafe "taglib/tac_c.h taglib_tag_set_title" taglib_tag_set_title :: TagRef -> UTF8String -> IO () 
foreign import ccall unsafe "taglib/tac_c.h taglib_tag_set_artist" taglib_tag_set_artist :: TagRef -> UTF8String -> IO () 
foreign import ccall unsafe "taglib/tac_c.h taglib_id3v2_set_default_text_encoding" taglib_id3v2_set_default_text_encoding :: CUInt -> IO () 

withUTF8String :: String -> (UTF8String -> IO a) -> IO a
withUTF8String s f = do 
    let s' = (UTF8.encode s) ++ [nullByte]
    withArray s' f

nullByte :: Word8
nullByte = 0 

peekUTF8String :: UTF8String -> IO String
peekUTF8String utf = do
    bytes <- peekArray0 nullByte utf
    return $ UTF8.decode bytes

no = 0
yes = 1

-- |Open a filename and possibly get a TagFile
open :: String -> IO (Maybe TagFile)
open filename = do 
    taglib_set_string_management_enabled no
    ptr <- withUTF8String filename taglib_file_new  
    if ptr == nullPtr 
        then return Nothing
        else do tagFile <- newForeignPtr taglib_file_free ptr
                return $ Just tagFile

-- |Save changes to a tag
save :: TagFile -> IO Integer 
save tagFile = liftM fromIntegral $ withForeignPtr tagFile taglib_file_save

-- |Get a Tag from a TagFile, if it has one
tag :: TagFile -> IO (Maybe Tag)
tag tagFile = do 
    tagPtr <- withForeignPtr tagFile taglib_file_tag 
    return  
        (if tagPtr == nullPtr 
            then Nothing
            else Just (Tag tagFile tagPtr))

-- |Get an artist string from a Tag
artist :: Tag -> IO String
artist = extractTagString taglib_tag_artist

-- |Get an album string from a Tag
album :: Tag -> IO String
album = extractTagString taglib_tag_album

-- |Get a title string from a Tag
title :: Tag -> IO String
title = extractTagString taglib_tag_title

-- |Get the comment string from a Tag
comment :: Tag -> IO String
comment = extractTagString taglib_tag_comment

-- |Get the comment string from a Tag
genre :: Tag -> IO String
genre = extractTagString taglib_tag_genre

extractTagString :: (Ptr Void -> IO UTF8String) -> Tag -> IO String
extractTagString taglib_cfunc (Tag tagFile tagPtr) = do
    cs <- taglib_cfunc tagPtr
    s <- peekUTF8String cs
    free cs
    return s

-- |Get the year from a Tag.  Empty values will be 0
year :: Tag -> IO Integer
year (Tag tagFile tagPtr) = liftM fromIntegral (taglib_tag_year tagPtr)

-- |Get the track number from a Tag.  Empty values will be 0
track :: Tag -> IO Integer
track (Tag tagFile tagPtr) = liftM fromIntegral (taglib_tag_track $ tagPtr)

{- Tag Setters -}
setTagString taglib_cfunc (Tag _ tagPtr) val = withUTF8String val $ taglib_cfunc tagPtr

{- Tag Setters -}
setTagInt :: (TagRef -> CUInt -> IO ()) -> Tag -> Integer -> IO ()
setTagInt taglib_cfunc (Tag _ tagRef) val = taglib_cfunc tagRef (fromIntegral val)

-- |Set the title of a tag
setTitle :: Tag -> String -> IO () 
setTitle = setTagString taglib_tag_set_title 

-- |Set the album of a tag
setAlbum :: Tag -> String -> IO () 
setAlbum = setTagString taglib_tag_set_album

-- |Set the artist of a tag
setArtist :: Tag -> String -> IO () 
setArtist = setTagString taglib_tag_set_artist

-- |Set the comment of a tag
setComment :: Tag -> String -> IO () 
setComment = setTagString taglib_tag_set_comment

-- |Set the genre of a tag
setGenre :: Tag -> String -> IO () 
setGenre = setTagString taglib_tag_set_genre

-- |Set the year of a tag
setYear :: Tag -> Integer -> IO () 
setYear = setTagInt taglib_tag_set_year

-- |Set the track of a tag
setTrack :: Tag -> Integer -> IO () 
setTrack = setTagInt taglib_tag_set_track

-- |Get the AudioProperties from a TagFile
audioProperties :: TagFile -> IO (Maybe AudioProperties)
audioProperties tagFile = do 
    ap <- withForeignPtr tagFile taglib_file_audioproperties 
    return $ (if ap == nullPtr 
                    then Nothing
                    else Just $ AudioProperties tagFile ap)

{- |
Get the duration (in seconds) from AudioProperties 
In TagLib, this is named length.  This is renamed so that it doesn't conflict with the Prelude length
-}
duration :: AudioProperties -> IO Integer
duration (AudioProperties _ prop) = liftM fromIntegral $ taglib_audioproperties_length prop

-- |Get the bitRate from AudioProperties
bitRate :: AudioProperties -> IO Integer
bitRate (AudioProperties _ prop) = liftM fromIntegral $ taglib_audioproperties_bitrate prop

-- |Get the number of channels from AudioProperties
channels :: AudioProperties -> IO Integer
channels (AudioProperties _ prop) = liftM fromIntegral $ taglib_audioproperties_channels prop

-- |Get the sampleRate from AudioProperties
sampleRate :: AudioProperties -> IO Integer
sampleRate (AudioProperties _ prop) = liftM fromIntegral $ taglib_audioproperties_samplerate prop

-----------
-- $example
-- 
-- > module Main where 
-- > 
-- > import qualified Sound.TagLib as TagLib
-- > import Data.Maybe
-- > import Control.Monad
-- > import System
-- > 
-- > main = do
-- >     args <- getArgs 
-- >     mapM showFile args
-- > 
-- > withMaybe :: (Maybe j) -> (j -> IO ()) -> IO () 
-- > withMaybe mebbe action = do
-- >     case mebbe of 
-- >         Just x -> do action x 
-- >                      return ()
-- >         Nothing -> return ()
-- > 
-- > showFile filename = do
-- >     t <- TagLib.open filename
-- >     withMaybe t showTagFile
-- > 
-- > showTagFile :: TagLib.TagFile -> IO ()
-- > showTagFile tagFile = do
-- >     t <- TagLib.tag tagFile
-- >     withMaybe t showTag
-- >     p <- TagLib.audioProperties tagFile
-- >     withMaybe p showAudioProperties 
-- > 
-- > showTag :: TagLib.Tag -> IO ()
-- > showTag tag = do 
-- >     artist <- TagLib.artist tag
-- >     album <- TagLib.album tag
-- >     title <- TagLib.title tag
-- >     comment <- TagLib.comment tag
-- >     year <- TagLib.year tag
-- >     track <- TagLib.track tag
-- >     print (artist, album, title, year, track)
-- > 
-- > showAudioProperties :: TagLib.AudioProperties -> IO ()
-- > showAudioProperties props = do
-- >     bitrate <- TagLib.bitRate props
-- >     length <- TagLib.duration props
-- >     samplerate <- TagLib.sampleRate props
-- >     channels <- TagLib.channels props
-- >     print (bitrate, length, channels, samplerate)
-- > 
