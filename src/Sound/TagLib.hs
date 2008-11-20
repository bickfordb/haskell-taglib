module Sound.TagLib (
    AudioProperties,
    Tag,
    TagFile,
    album,
    artist, 
    audioProperties,
    bitRate,
    channels,
    comment,
    duration,
    open,
    sampleRate,
    setArtist,
    tag,
    title,
    track,
    year
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

foreign import ccall unsafe "taglib/tag_c.h taglib_file_new" taglib_file_new :: UTF8String -> IO (Ptr Void)
foreign import ccall unsafe "taglib/tag_c.h &taglib_file_free" taglib_file_free :: FunPtr (Ptr Void -> IO ())
foreign import ccall unsafe "taglib/tag_c.h taglib_file_tag" taglib_file_tag :: (Ptr Void) -> IO (Ptr Void)

{- Tag Getters -}
foreign import ccall unsafe "taglib/tag_c.h taglib_tag_artist" taglib_tag_artist :: (Ptr Void) -> IO UTF8String
foreign import ccall unsafe "taglib/tag_c.h taglib_tag_album" taglib_tag_album :: (Ptr Void) -> IO UTF8String
foreign import ccall unsafe "taglib/tag_c.h taglib_tag_title" taglib_tag_title :: (Ptr Void) -> IO UTF8String
foreign import ccall unsafe "taglib/tag_c.h taglib_tag_comment" taglib_tag_comment :: (Ptr Void) -> IO UTF8String
foreign import ccall unsafe "taglib/tag_c.h taglib_tag_genre" taglib_tag_genre :: (Ptr Void) -> IO UTF8String
foreign import ccall unsafe "taglib/tag_c.h taglib_tag_year" taglib_tag_year :: (Ptr Void) -> IO CUInt
foreign import ccall unsafe "taglib/tag_c.h taglib_tag_track" taglib_tag_track :: (Ptr Void) -> IO CUInt
--foreign import ccall unsafe "taglib/tag_c.h taglib_tag_free_strings" taglib_tag_free_strings :: IO ()
foreign import ccall unsafe "taglib/tag_c.h taglib_file_save" taglib_file_save :: (Ptr Void) -> IO CInt
foreign import ccall unsafe "taglib/tag_c.h taglib_set_string_management_enabled" taglib_set_string_management_enabled :: CInt -> IO ()

{- Audio properties -}
foreign import ccall unsafe "taglib/tag_c.h taglib_file_audioproperties" taglib_file_audioproperties :: (Ptr Void) -> IO (Ptr Void)
foreign import ccall unsafe "taglib/tac_c.h taglib_audioproperties_length" taglib_audioproperties_length :: (Ptr Void) -> IO CInt
foreign import ccall unsafe "taglib/tac_c.h taglib_audioproperties_bitrate" taglib_audioproperties_bitrate :: (Ptr Void) -> IO CInt 
foreign import ccall unsafe "taglib/tac_c.h taglib_audioproperties_samplerate" taglib_audioproperties_samplerate :: (Ptr Void) -> IO CInt 
foreign import ccall unsafe "taglib/tac_c.h taglib_audioproperties_channels" taglib_audioproperties_channels :: (Ptr Void) -> IO CInt 

{- Tag Setters -}
foreign import ccall unsafe "taglib/tac_c.h taglib_tag_set_track" taglib_tag_set_track :: (Ptr Void) -> CUInt -> IO () 
foreign import ccall unsafe "taglib/tac_c.h taglib_tag_set_year" taglib_tag_set_year :: (Ptr Void) -> CUInt -> IO () 
foreign import ccall unsafe "taglib/tac_c.h taglib_tag_set_genre" taglib_tag_set_genre :: (Ptr Void) -> UTF8String -> IO () 
foreign import ccall unsafe "taglib/tac_c.h taglib_tag_set_comment" taglib_tag_set_comment :: (Ptr Void) -> UTF8String -> IO () 
foreign import ccall unsafe "taglib/tac_c.h taglib_tag_set_album" taglib_tag_set_album :: (Ptr Void) -> UTF8String -> IO () 
foreign import ccall unsafe "taglib/tac_c.h taglib_tag_set_title" taglib_tag_set_title :: (Ptr Void) -> UTF8String -> IO () 
foreign import ccall unsafe "taglib/tac_c.h taglib_tag_set_artist" taglib_tag_set_artist :: (Ptr Void) -> UTF8String -> IO () 
foreign import ccall unsafe "taglib/tac_c.h taglib_id3v2_set_default_text_encoding" taglib_id3v2_set_default_text_encoding :: CUInt -> IO () 

type TagFile = ForeignPtr Void

data Tag = Tag TagFile (Ptr Void)
    deriving (Show)

type UTF8String = Ptr Word8

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

open :: String -> IO (Maybe TagFile)
open filename = do 
    taglib_set_string_management_enabled no
    ptr <- withUTF8String filename taglib_file_new  
    if ptr == nullPtr 
        then return Nothing
        else do tagFile <- newForeignPtr taglib_file_free ptr
                return $ Just tagFile

save :: TagFile -> IO Int 
save tagFile = liftM fromIntegral $ withForeignPtr tagFile taglib_file_save

tag :: TagFile -> IO (Maybe Tag)
tag tagFile = do 
    tagPtr <- withForeignPtr tagFile taglib_file_tag 
    return  
        (if tagPtr == nullPtr 
            then Nothing
            else Just (Tag tagFile tagPtr))

artist :: Tag -> IO String
artist = extractTagString taglib_tag_artist

album :: Tag -> IO String
album = extractTagString taglib_tag_album

title :: Tag -> IO String
title = extractTagString taglib_tag_title

comment :: Tag -> IO String
comment = extractTagString taglib_tag_comment

extractTagString :: (Ptr Void -> IO UTF8String) -> Tag -> IO String
extractTagString taglib_cfunc (Tag tagFile tagPtr) = do
    cs <- taglib_cfunc tagPtr
    s <- peekUTF8String cs
    free cs
    return s

year :: Tag -> IO Int
year (Tag tagFile tagPtr) = liftM fromIntegral (taglib_tag_year tagPtr)

track :: Tag -> IO Int
track (Tag tagFile tagPtr) = liftM fromIntegral (taglib_tag_track $ tagPtr)

{- Tag Setters -}
setTagString taglib_cfunc (Tag _ tagPtr) val = withUTF8String val $ taglib_cfunc tagPtr

setTitle :: Tag -> String -> IO () 
setTitle = setTagString taglib_tag_set_title 

setAlbum :: Tag -> String -> IO () 
setAlbum = setTagString taglib_tag_set_album

setArtist :: Tag -> String -> IO () 
setArtist = setTagString taglib_tag_set_artist

setComment :: Tag -> String -> IO () 
setComment = setTagString taglib_tag_set_comment

{- Properties -}
data AudioProperties = AudioProperties TagFile (Ptr Void)

audioProperties :: TagFile -> IO (Maybe AudioProperties)
audioProperties tagFile = do 
    ap <- withForeignPtr tagFile taglib_file_audioproperties 
    return $ (if ap == nullPtr 
                    then Nothing
                    else Just $ AudioProperties tagFile ap)

-- 'duration' instead of length so that we don't conflict with the Prelude
duration :: AudioProperties -> IO Int
duration (AudioProperties _ prop) = liftM fromIntegral $ taglib_audioproperties_length prop

bitRate :: AudioProperties -> IO Int
bitRate (AudioProperties _ prop) = liftM fromIntegral $ taglib_audioproperties_bitrate prop

channels :: AudioProperties -> IO Int
channels (AudioProperties _ prop) = liftM fromIntegral $ taglib_audioproperties_channels prop

sampleRate :: AudioProperties -> IO Int
sampleRate (AudioProperties _ prop) = liftM fromIntegral $ taglib_audioproperties_samplerate prop
