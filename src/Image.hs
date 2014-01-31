module Image (
    resize,
    loadImage,
    Paper(Paper),
    ImageSize(ImageSize)
) where

import Data.ByteString
import Codec.Picture
import Codec.Picture.Types
import Graphics.Rendering.Cairo
import Data.Vector.Storable (unsafeWith)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import GHC.Word (Word8, Word32)
import System.IO.Temp
import GHC.Float
import GHC.IO.Handle (hSeek, SeekMode(AbsoluteSeek))
import Foreign.Storable
import Data.Bits

data Paper = Paper !Float !Float
data ImageSize = ImageSize !Float !Float

ptrmap :: Storable a => Ptr a -> Int -> (a -> a) -> IO(Ptr a)
ptrmap ptr size f = impl ptr size
    where
        impl p left = 
          if left == 0 
            then (return ptr) 
            else do
              v <- peek p
              poke p (f v)
              impl (plusPtr p (sizeOf v)) (left - 1)

ensureRGBA8 :: DynamicImage -> Either String (Image PixelRGBA8)
ensureRGBA8 (ImageY8 img)     = Right $ promoteImage img
ensureRGBA8 (ImageYA8 img)    = Right $ promoteImage img
ensureRGBA8 (ImageRGB8 img)   = Right $ promoteImage img
ensureRGBA8 (ImageRGBA8 img)  = Right img
ensureRGBA8 (ImageYCbCr8 img) = Right $ promoteImage ((convertImage img) :: Image PixelRGB8)
ensureRGBA8 (ImageRGBF _)     = Left "Not compatible"
ensureRGBA8 (ImageYF _)       = Left "Not compatible"
ensureRGBA8 _                 = Left "Not compatible"

loadImage :: FilePath -> IO ( Either String (Image PixelRGBA8) )
loadImage b = do
    img <- readImage b
    case img of
        Left xxx -> return $ Left xxx
        Right imag -> return $ ensureRGBA8 imag

image2surface :: Image PixelRGBA8 -> IO (Surface)
image2surface img8 = toSurface img8
  where
    toSurface :: Image PixelRGBA8 -> IO Surface
    toSurface img = unsafeWith (imageData img) (ptrToSurface img)
    flipColor :: Word32 -> Word32
    flipColor c =
         (0xff000000 .&. c) .|.
         ((0x00ff0000 .&. c) `shiftR` 16) .|.
         (0x0000ff00 .&. c) .|.
         ((0x000000ff .&. c) `shiftL` 16)
    ptrToSurface :: Image PixelRGBA8 -> Ptr Word8 -> IO Surface
    ptrToSurface img ptr = do
        _ <- ptrmap ((castPtr ptr) :: (Ptr Word32)) pixs flipColor
        src <- createImageSurfaceForData (castPtr ptr) FormatARGB32 w h stride
        surf <- createImageSurface FormatARGB32 w h
        renderWith surf $ do
                setSourceSurface src 0.0 0.0
                paint
        return surf
        where
            w = imageWidth img
            h = imageHeight img
            stride = w * componentCount (undefined :: PixelRGBA8)
            pixs = w * h

cm2pt :: Float -> Double
cm2pt cm = (float2Double cm) * 72.0 / 2.54;
px2pt px = float2Double px

resize :: Paper -> ImageSize -> (Image PixelRGBA8) -> IO (ByteString)
resize (Paper paperWidth paperHeight) (ImageSize imageWidth imageHeight) image = do
    withSystemTempFile "img" $ \fpath hand -> do
        withPDFSurface fpath pdfWidth pdfHeight render
        hSeek hand AbsoluteSeek 0
        hGetContents hand
    where
        pdfWidth = cm2pt paperWidth
        pdfHeight = cm2pt paperHeight
        imgH = cm2pt imageHeight
        imgW = cm2pt imageWidth
        render surface = do
            imgSurf <- image2surface image
            origW <- imageSurfaceGetWidth imgSurf
            origH <- imageSurfaceGetHeight imgSurf
            renderWith surface $ do
                save
                translate (pdfWidth/2.0) (pdfHeight/2.0)
                scale (imgW/(fromIntegral origW)) (imgH/(fromIntegral origH))
                translate (-(fromIntegral origW) / 2.0) (-(fromIntegral origH) / 2.0)
                setSourceSurface imgSurf 0 0
                paint
            return surface

