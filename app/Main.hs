module Main where

import Codec.Picture( PixelRGBA8( .. ), writePng )
import Graphics.Rasterific
import Graphics.Rasterific.Texture

import Geometry as G
import Control.Monad as M

main :: IO ()
main = do
    let white = PixelRGBA8 255 255 255 255
        black = PixelRGBA8 0 0 0 255
        img = renderDrawing 1000 1000 white $
            withTexture (uniformTexture black) $ do
                mconcat $ fmap (\b -> stroke 1 JoinRound (CapRound, CapRound) b) G.scaledImage
    
    writePng "test.png" img