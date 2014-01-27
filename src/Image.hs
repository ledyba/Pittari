module Image where

import Data.ByteString
import Codec.Picture
import Codec.Picture.Types
import Graphics.Rendering.Cairo
import Data.Vector.Storable (unsafeWith)
import Foreign.Ptr (Ptr, castPtr)
import GHC.Word (Word8)

newtype DPI = DPI Int
data Paper = Paper (Float, Float)
data ImageSize = ImageSize (Float, Float)

ensureRGBA8 :: DynamicImage -> Either String (Image PixelRGBA8)
ensureRGBA8 (ImageY8 img)     = Right $ promoteImage img
ensureRGBA8 (ImageYA8 img)    = Right $ promoteImage img
ensureRGBA8 (ImageRGB8 img)   = Right $ promoteImage img
ensureRGBA8 (ImageRGBA8 img)  = Right $ img
ensureRGBA8 (ImageYCbCr8 img) = Right $ promoteImage ((convertImage img) :: Image PixelRGB8)
ensureRGBA8 (ImageRGBF _)     = Left "Not compatible"
ensureRGBA8 (ImageYF _)       = Left "Not compatible"
ensureRGBA8 _                 = Left "Not compatible"

load :: ByteString -> IO (Either String Surface)
load binary =
  do
    img <- return $ toRGBA8 binary
    case img of
        Left msg -> return (Left msg)
        Right img8 -> do
            surf <- toSurface img8
            return (Right surf)
  where
    toRGBA8 :: ByteString -> Either String (Image PixelRGBA8)
    toRGBA8 img = do
        img_ <- decodeImage img
        rgba8 <- ensureRGBA8 img_
        return rgba8
    
    toSurface :: (Image PixelRGBA8) -> IO Surface
    toSurface img = do
        surf <- unsafeWith (imageData img) (ptrToSurface img)
        return surf
    
    ptrToSurface :: (Image PixelRGBA8) -> (Ptr Word8) -> IO Surface
    ptrToSurface img ptr = 
        createImageSurfaceForData (castPtr ptr) FormatARGB32 w h stride
        where
            w = imageWidth img
            h = imageHeight img
            stride = (componentCount (undefined :: PixelRGBA8)) * w

-- resize :: DPI -> Paper -> ImageSize -> DynamicImage -> DynamicImage
