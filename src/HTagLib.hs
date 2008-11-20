{-# options -fglasgow-exts -ffi -L/opt/local/lib -ltag_c #-}
module HTagLib where

import Foreign
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import System.IO
import System.IO.Unsafe

import Control.Monad

import qualified Data.ByteString as B

type Void = Word8

foreign import ccall unsafe "taglib/tag_c.h taglib_file_new" taglib_file_new :: CString -> IO (Ptr Void)
foreign import ccall unsafe "taglib/tag_c.h &taglib_file_free" taglib_file_free :: FunPtr (Ptr Void -> IO ())
foreign import ccall unsafe "taglib/tag_c.h taglib_file_tag" taglib_file_tag :: (Ptr Void) -> IO (Ptr Void)

{- Tag Getters -}
foreign import ccall unsafe "taglib/tag_c.h taglib_tag_artist" taglib_tag_artist :: (Ptr Void) -> IO CString
foreign import ccall unsafe "taglib/tag_c.h taglib_tag_album" taglib_tag_album :: (Ptr Void) -> IO CString
foreign import ccall unsafe "taglib/tag_c.h taglib_tag_title" taglib_tag_title :: (Ptr Void) -> IO CString
foreign import ccall unsafe "taglib/tag_c.h taglib_tag_comment" taglib_tag_comment :: (Ptr Void) -> IO CString
foreign import ccall unsafe "taglib/tag_c.h taglib_tag_genre" taglib_tag_genre :: (Ptr Void) -> IO CString
foreign import ccall unsafe "taglib/tag_c.h taglib_tag_year" taglib_tag_year :: (Ptr Void) -> IO CUInt
foreign import ccall unsafe "taglib/tag_c.h taglib_tag_track" taglib_tag_track :: (Ptr Void) -> IO CUInt
foreign import ccall unsafe "taglib/tag_c.h taglib_tag_free_strings" taglib_tag_free_strings :: IO ()
foreign import ccall unsafe "taglib/tag_c.h taglib_file_save" taglib_file_save :: (Ptr Void) -> IO CInt

{- Audio properties -}
foreign import ccall unsafe "taglib/tag_c.h taglib_file_audioproperties" taglib_file_audioproperties :: (Ptr Void) -> IO (Ptr Void)
foreign import ccall unsafe "taglib/tac_c.h taglib_audioproperties_length" taglib_audioproperties_length :: (Ptr Void) -> IO CInt
foreign import ccall unsafe "taglib/tac_c.h taglib_audioproperties_bitrate" taglib_audioproperties_bitrate :: (Ptr Void) -> IO CInt 
foreign import ccall unsafe "taglib/tac_c.h taglib_audioproperties_samplerate" taglib_audioproperties_samplerate :: (Ptr Void) -> IO CInt 
foreign import ccall unsafe "taglib/tac_c.h taglib_audioproperties_channels" taglib_audioproperties_channels :: (Ptr Void) -> IO CInt 

{- Tag Setters -}

foreign import ccall unsafe "taglib/tac_c.h taglib_tag_set_track" taglib_tag_set_track :: (Ptr Void) -> CUInt -> IO () 
foreign import ccall unsafe "taglib/tac_c.h taglib_tag_set_year" taglib_tag_set_year :: (Ptr Void) -> CUInt -> IO () 
foreign import ccall unsafe "taglib/tac_c.h taglib_tag_set_genre" taglib_tag_set_genre :: (Ptr Void) -> CString -> IO () 
foreign import ccall unsafe "taglib/tac_c.h taglib_tag_set_comment" taglib_tag_set_comment :: (Ptr Void) -> CString -> IO () 
foreign import ccall unsafe "taglib/tac_c.h taglib_tag_set_album" taglib_tag_set_album :: (Ptr Void) -> CString -> IO () 
foreign import ccall unsafe "taglib/tac_c.h taglib_tag_set_title" taglib_tag_set_title :: (Ptr Void) -> CString -> IO () 
foreign import ccall unsafe "taglib/tac_c.h taglib_tag_set_artist" taglib_tag_set_artist :: (Ptr Void) -> CString -> IO () 

data FileWithTag = FileWithTag {
    fName :: String,
    fPtr :: ForeignPtr Void
} deriving (Show)

data Tag = Tag { 
    tFile :: FileWithTag,
    tPtr :: Ptr Void
} deriving (Show)

openTagFile filename = do 
    ptr <- withCString filename taglib_file_new  
    if ptr == nullPtr 
        then return Nothing
        else do fptr <- newForeignPtr taglib_file_free ptr
                return $ Just $ FileWithTag filename fptr

saveTagFile :: FileWithTag -> IO Int 
saveTagFile file_with_tag = liftM fromIntegral $ withForeignPtr (fPtr file_with_tag) taglib_file_save

getTag file = do 
    ptr <- withForeignPtr (fPtr file) taglib_file_tag 
    if ptr == nullPtr 
        then return Nothing
        else return $ Just $ Tag { 
            tPtr = ptr,
             tFile = file
        }

getArtist = getTagStr taglib_tag_artist
getAlbum = getTagStr taglib_tag_album
getTitle = getTagStr taglib_tag_title
getComment = getTagStr taglib_tag_comment

getTagStr f tag = do 
    s <- f $ tPtr tag
    peekCString s

getYear tag = liftM fromIntegral $ taglib_tag_year $ tPtr tag

getTrack tag = liftM fromIntegral $ taglib_tag_track $ tPtr tag

{- Tag Setters -}

setTagStr f tag val = withCString val $ f $ tPtr tag

setTitle :: Tag -> String -> IO () 
setTitle = setTagStr taglib_tag_set_title 

setAlbum :: Tag -> String -> IO () 
setAlbum = setTagStr taglib_tag_set_album

setArtist :: Tag -> String -> IO () 
setArtist = setTagStr taglib_tag_set_artist

setComment :: Tag -> String -> IO () 
setComment = setTagStr taglib_tag_set_comment

{- Properties -}


data AudioProperties = AudioProperties { 
    apPtr :: Ptr Void
}

getAudioProperties :: FileWithTag -> IO (Maybe AudioProperties)
getAudioProperties file = do 
    ap <- withForeignPtr (fPtr file) taglib_file_audioproperties 
    return $ (if ap == nullPtr 
                    then Nothing
                    else Just $ AudioProperties ap)

getLength :: AudioProperties -> IO Int
getLength prop = liftM fromIntegral  $ taglib_audioproperties_length $ apPtr prop

getBitrate :: AudioProperties -> IO Int
getBitrate prop = liftM fromIntegral $ taglib_audioproperties_bitrate $ apPtr prop

getChannels :: AudioProperties -> IO Int
getChannels prop = liftM fromIntegral $ taglib_audioproperties_channels $ apPtr prop

getSampleRate :: AudioProperties -> IO Int
getSampleRate prop = liftM fromIntegral $ taglib_audioproperties_samplerate $ apPtr prop


