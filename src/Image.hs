module Image (
    resize,
    DPI(DPI),
    Paper(Paper),
    ImageSize(ImageSize)
) where

import Data.ByteString
import Codec.Picture
import Codec.Picture.Types
import Graphics.Rendering.Cairo
import Data.Vector.Storable (unsafeWith)
import Foreign.Ptr (Ptr, castPtr)
import GHC.Word (Word8)
import System.IO.Temp
import GHC.Float
import GHC.IO.Handle (hSeek, SeekMode(AbsoluteSeek))

newtype DPI = DPI Int
data Paper = Paper Float Float
data ImageSize = ImageSize Float Float

ensureRGBA8 :: DynamicImage -> Either String (Image PixelRGBA8)
ensureRGBA8 (ImageY8 img)     = Right $ promoteImage img
ensureRGBA8 (ImageYA8 img)    = Right $ promoteImage img
ensureRGBA8 (ImageRGB8 img)   = Right $ promoteImage img
ensureRGBA8 (ImageRGBA8 img)  = Right img
ensureRGBA8 (ImageYCbCr8 img) = Right $ promoteImage ((convertImage img) :: Image PixelRGB8)
ensureRGBA8 (ImageRGBF _)     = Left "Not compatible"
ensureRGBA8 (ImageYF _)       = Left "Not compatible"
ensureRGBA8 _                 = Left "Not compatible"

loadImage :: ByteString -> Either String (Image PixelRGBA8)
loadImage b = do
    img <- decodeImage b
    ensureRGBA8 img

image2surface :: Image PixelRGBA8 -> IO (Surface)
image2surface img8 = toSurface img8
  where
    toSurface :: Image PixelRGBA8 -> IO Surface
    toSurface img = unsafeWith (imageData img) (ptrToSurface img)
    
    ptrToSurface :: Image PixelRGBA8 -> Ptr Word8 -> IO Surface
    ptrToSurface img ptr = 
        createImageSurfaceForData (castPtr ptr) FormatARGB32 w h stride
        where
            w = imageWidth img
            h = imageHeight img
            stride = w * componentCount (undefined :: PixelRGBA8)

pt2cm :: Float -> Double
pt2cm cm = (float2Double cm) * 72.0 / 2.54;

resize :: DPI -> Paper -> ImageSize -> (Image PixelRGBA8) -> IO (ByteString)
resize dpi (Paper paperWidth paperHeight) imageSize image = do
    withSystemTempFile "img" (\ fpath hand -> do
        withPDFSurface fpath pdfWidth pdfHeight render
        hSeek hand AbsoluteSeek 0
        hGetContents hand
        )
    where
        pdfWidth = pt2cm paperWidth
        pdfHeight = pt2cm paperHeight
        render surface = do
            imgSurf <- image2surface image
            renderWith surface (do
                setSourceSurface imgSurf 0.0 0.0
                paint
                )
            return surface

