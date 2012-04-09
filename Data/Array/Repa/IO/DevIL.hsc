{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module    : Data/Array/Repa/IO/DevIL.hs
-- Copyright : (c) Don Stewart 2011, Raphael Javaux 2012
--
-- License   : BSD3
--
-- Maintainer: Don Stewart <dons00@gmail.com>
--           , Raphael Javaux <raphaeljavaux@gmail.com
-- Stability : provisional
-- Portability: Repa interface to the DevIL image loading library.
--
-- Read and write images in many formats, representing them in Haskell
-- as a 3-dimensional /repa/ array. Image parsing and decoding is done
-- by the /Developers Image Library/, DevIL.
--
-- * Many formats are supported, including .png, .bmp, .jpg, .tif
--
-- * Image format parsing is determined by the filepath extension type.
--
-- Example: read a .png file into a 3D repa array, and write it out as a .jpg
--
-- > main = runIL $ do
-- >          x <- readImage "/tmp/y.png" 
-- >          writeImage "/tmp/x.jpg" x
--
-- Note that as DevIL is stateful, we ensure the library is initialized
-- by running image manipulation functions in the /IL/ monad, a wrapper
-- over IO that ensures the library has been initialized. It is a type
-- error to call image functions outside of the /IL/ monad.
--

module Data.Array.Repa.IO.DevIL (
    -- * The Image array type 
      Image
    
    -- * The IL monad
    , IL, runIL

    -- * Image IO 
    , readImage{-, writeImage-}
    ) where

import Debug.Trace

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))

import Data.Int
import Data.Word

import Foreign.C.String (CString, withCString)
import Foreign.ForeignPtr (FinalizerPtr)
import Foreign.Concurrent (newForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, FunPtr, freeHaskellFunPtr)
import Foreign.Storable (peek)
import Foreign.Marshal.Utils (with)

import Data.Array.Repa (Array (..), Z (..), (:.) (..), DIM2, DIM3)
import Data.Array.Repa.Repr.ForeignPtr (F, fromForeignPtr, toForeignPtr)

#include "IL/il.h"

type ILuint    = #type ILuint
type ILsizei   = #type ILsizei
type ILboolean = #type ILboolean
type ILenum    = #type ILenum
type ILint     = #type ILint
type ILubyte   = #type ILubyte

-- DevIL uses unsigned integers as names for each image in processing.
newtype ImageName = ImageName { fromImageName :: ILuint }

-- ----------------------------------------------------------------------

-- | The RGBA and RGB images are 3D repa arrays where indices are 
-- /Z :. row :. column :. color channel/. Grey images are 2D repa arrays.
-- 
-- The origin (/Z :. 0 :. 0/) is on the lower left point of the image.
data Image = RGBA (Array F DIM3 Word8)
           | RGB (Array F DIM3 Word8)
           | Grey (Array F DIM2 Word8)

-- | The IL monad. Provides statically-guaranteed access to an initialized IL
-- context.
newtype IL a = IL { unIL :: IO a }
    deriving (Monad, MonadIO)

-- | Running code in the /IL/ monad. This is a simple wrapper over /IO/
-- that guarantees the DevIL library has been initialized before you run
-- functions on it.
runIL :: IL a -> IO a
runIL (IL a) = ilInit >> a
{-# INLINE runIL #-}

test (RGB i) = "RGB"
test (RGBA i) = "RGBA"
test (Grey i) = "Grey"

-- | Reads an image into an RGBA array. It uses directly the C array using the
-- repa\'s foreign arrays wrapper.
-- 
-- Example:
-- 
-- > main = do
-- >    x <- runIL $ readImage "/tmp/x.png"
-- >    .. operations on x ..
-- 
-- /Note:/ The image input type is determined by the filename extension. 
readImage  :: FilePath -> IL Image
readImage f = liftIO $ do
    name <- ilGenImageName
    ilBindImage name
    success <- ilLoadImage f
    when (not success) $
       error "Unable to load the image."
    toRepa name
{-# INLINE readImage #-}

-- -- | Writes an RGBA array to a file. Indices are the row, column, and color-channel.
-- -- 
-- -- /Note:/ The image output type is determined by the filename extension. 
-- writeImage :: FilePath -> R.Array DIM3 Word8 -> IL ()
-- writeImage f a = do
--     liftIO $ D.writeImage f (fromRepa a)
-- {-# INLINE writeImage #-}

-- ----------------------------------------------------------------------

foreign import ccall "ilInit" ilInitC :: IO ()
foreign import ccall "ilOriginFunc" ilOriginFuncC :: ILenum -> IO ILboolean
foreign import ccall "ilEnable" ilEnableC :: ILenum -> IO ILboolean

-- | Initialize the library.
ilInit :: IO ()
ilInit = do
    ilInitC
    -- By default, origin is undefined and depends on the image type
    ilOriginFuncC (#const IL_ORIGIN_LOWER_LEFT)
    ilEnableC (#const IL_ORIGIN_SET)
    return ()
{-# INLINE ilInit #-}
    
foreign import ccall "ilGenImages" ilGenImagesC
  :: ILsizei -> Ptr ILuint -> IO ()

-- | Allocates a new image name.
ilGenImageName :: IO ImageName
ilGenImageName = do
    alloca $ \pName -> do
        ilGenImagesC 1 pName
        name <- peek pName
        return $! ImageName name
{-# INLINE ilGenImageName #-}

foreign import ccall "ilBindImage" ilBindImageC :: ILuint -> IO ()

-- | Sets the image name as the current image for processing.
ilBindImage :: ImageName -> IO ()
ilBindImage (ImageName name) = ilBindImageC name
{-# INLINE ilBindImage #-}

foreign import ccall "ilLoadImage" ilLoadImageC :: CString -> IO ILboolean

-- | Loads the image as the current DevIL image name.
ilLoadImage :: FilePath -> IO Bool
ilLoadImage f = (0 /=) <$> withCString f ilLoadImageC
{-# INLINE ilLoadImage #-}

foreign import ccall "ilGetInteger" ilGetIntegerC :: ILenum -> IO ILint

il_RGB = (#const IL_RGB) :: ILint
il_RGBA = (#const IL_RGBA) :: ILint
il_LUMINANCE = (#const IL_LUMINANCE) :: ILint

il_IMAGE_HEIGHT = (#const IL_IMAGE_HEIGHT) :: ILenum
il_IMAGE_WIDTH  = (#const IL_IMAGE_WIDTH)  :: ILenum
il_UNSIGNED_BYTE = (#const IL_UNSIGNED_BYTE) :: ILenum
il_IMAGE_FORMAT  = (#const IL_IMAGE_FORMAT)  :: ILenum

foreign import ccall "ilGetData" ilGetDataC :: IO (Ptr ILubyte)

-- | Puts the current image inside an repa array.
toRepa :: ImageName -> IO Image
toRepa name = do
    width' <- ilGetIntegerC il_IMAGE_WIDTH
    height' <- ilGetIntegerC il_IMAGE_HEIGHT
    let (width, height) = (fromIntegral width', fromIntegral height')
    format <- ilGetIntegerC il_IMAGE_FORMAT
    pixels <- ilGetDataC
    
    -- Destroys the image when the array will be garbage collected
    managedPixels <- newForeignPtr pixels (imageFinalizer name) 
    
    print format
    print il_RGB
    print il_RGBA
    print il_LUMINANCE
    
    case format of
        _ | format == il_RGB       -> do
            print 2
            let arr = fromForeignPtr (Z :. height :. width :. 3) managedPixels
            return $! RGB arr
          | format == il_RGBA      -> do
            print 1
            let arr = fromForeignPtr (Z :. height :. width :. 4) managedPixels
            return $! RGBA arr
          | format == il_LUMINANCE -> do
            let arr = fromForeignPtr (Z :. height :. width) managedPixels
            return $! Grey arr
          | otherwise              -> error "Unsupported image format."

foreign import ccall "ilDeleteImages" ilDeleteImagesC
    :: ILsizei -> Ptr ILuint -> IO ()

-- | Releases the image when the repa array has been garbage collected.
imageFinalizer :: ImageName -> IO ()
imageFinalizer (ImageName name) = do
    trace ("Free " ++ show name) $ return ()
    with name $ \pName ->
        ilDeleteImagesC 1 pName