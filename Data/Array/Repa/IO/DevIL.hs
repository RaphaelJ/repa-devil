{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module    : Data/Array/Repa/IO/DevIL.hs
-- Copyright : (c) Don Stewart 2011
--
-- License   : BSD3
--
-- Maintainer: Don Stewart <dons00@gmail.com>
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

    -- * The IL monad
    runIL, IL,

    -- * Image IO 
    readImage, writeImage
    
    ) where

-- transformers
import Control.Monad.IO.Class (MonadIO(..))

-- codec-image-devil
import qualified Codec.Image.DevIL as D

-- repa
import qualified Data.Array.Repa as R
import Data.Array.Repa

-- base / arrays
import Data.Array.Base
import Data.Word

------------------------------------------------------------------------

-- | The IL monad. Provides statically-guaranteed access to an initialized IL context.
newtype IL a = IL { unIL :: IO a }
    deriving (Monad, MonadIO)

-- | Running code in the /IL/ monad. This is a simple wrapper over /IO/
-- that guarantees the DevIL library has been initialized before you run
-- functions on it.
--
runIL :: IL a -> IO a
runIL (IL a) = do
    D.ilInit
    a

-- | Reads an image into an RGBA array. Indices are (row,column,color-channel).
--
-- Example:
--
-- > main = do
-- >    x <- runIL $ readImage "/tmp/x.png"
-- >    .. operations on x ..
--
--
-- /Note:/ The image input type is determined by the filename extension. 
--
readImage  :: FilePath -> IL (R.Array DIM3 Word8)
readImage f = do
    uarr <- liftIO (D.readImage f) :: IL (UArray (Int, Int, Int) Word8)
    return $! toRepa uarr
{-# INLINE readImage #-}

-- | Writes an RGBA array to a file. Indices are the row, column, and color-channel.
--
-- /Note:/ The image output type is determined by the filename extension. 
--
writeImage :: FilePath -> R.Array DIM3 Word8 -> IL ()
writeImage f a = do
    liftIO $ D.writeImage f (fromRepa a)
{-# INLINE writeImage #-}

------------------------------------------------------------------------

-- lazily stream an unboxed (3d) array to a repa array
toRepa :: UArray (Int,Int,Int) Word8 -> R.Array DIM3 Word8
toRepa uarr =
    let ((ni,nj,nk),(mi, mj, mk)) = bounds uarr
        (i,j,k) = (1 + mi - ni, 1 + mj - nj, 1 + mk - nk)
        es      = elems uarr
    in R.fromList (R.Z :. i :. j :. k) es
{-# INLINE toRepa #-}

-- lazily stream a repa array to an unboxed (3d) array
fromRepa :: R.Array DIM3 Word8 -> UArray (Int,Int,Int) Word8
fromRepa arr = 
    let es = R.toList arr
        (R.Z :. i :. j :. k) = extent arr
    in listArray ((0,0,0), (i-1, j-1, k-1)) es
{-# INLINE fromRepa #-}

{-

TODO: O(1) conversion:

toRepa uarr@(UArray l u n arr) = undefined

    -- data UArray i e = UArray !i !i !Int ByteArray#

    -- data Vector a = Vector {-# UNPACK #-} !Int
                              {-# UNPACK #-} !Int
                              {-# UNPACK #-} !ByteArray
-}

