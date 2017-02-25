module Geometry
    ( Image (..)
    , scaledImage
    ) where

import Prelude hiding (flip, cycle)
import Graphics.Rasterific

type Image = [CubicBezier]

applyB f (CubicBezier p1 p2 p3 p4) = CubicBezier (f p1) (f p2) (f p3) (f p4) 

applyI :: (Point -> Point) -> Image -> Image
applyI f = fmap (applyB f)

flip :: Image -> Image
flip = applyI (\(V2 x y) -> V2 (1.0 - x) y)

besideS :: Float -> Float -> Image -> Image -> Image
besideS lr rr l r = sl ++ sr
    where sl = applyI (\(V2 x y) -> V2 (x * lr) y) l
          sr = applyI (\(V2 x y) -> V2 (x * rr + lr) y) r

beside :: Image -> Image -> Image
beside = besideS 0.5 0.5

aboveS :: Float -> Float -> Image -> Image -> Image
aboveS tr br t b = st ++ sb
    where st = applyI (\(V2 x y) -> V2 x (y * tr)) t
          sb = applyI (\(V2 x y) -> V2 x (y * br + tr)) b

above :: Image -> Image -> Image
above = aboveS 0.5 0.5

rot :: Image -> Image
rot = applyI (\(V2 x y) -> V2 y (1.0 - x))

rot45 :: Image -> Image
rot45 = applyI (\(V2 x y) -> V2 ((x + y) / 2) ((y - x) / 2))

over :: Image -> Image -> Image
over i1 i2 = i1 ++ i2

quartet :: Image -> Image -> Image -> Image -> Image
quartet p q r s = above (beside p q) (beside r s)

cycle :: Image -> Image
cycle i = quartet (rot i) i (rot (rot i)) (rot (rot (rot i)))

blank = [] :: Image

fish = [ (CubicBezier (V2 0.00 0.00) (V2 0.08 0.02) (V2 0.22 0.18) (V2 0.29 0.28))
       , (CubicBezier (V2 0.29 0.28) (V2 0.30 0.36) (V2 0.29 0.43) (V2 0.30 0.50))
       , (CubicBezier (V2 0.30 0.50) (V2 0.34 0.60) (V2 0.43 0.68) (V2 0.50 0.74))
       , (CubicBezier (V2 0.50 0.74) (V2 0.58 0.79) (V2 0.66 0.78) (V2 0.76 0.80))
       , (CubicBezier (V2 0.76 0.80) (V2 0.82 0.88) (V2 0.94 0.95) (V2 1.00 1.00))
       , (CubicBezier (V2 1.00 1.00) (V2 0.90 0.97) (V2 0.81 0.96) (V2 0.76 0.95))
       , (CubicBezier (V2 0.76 0.95) (V2 0.69 0.96) (V2 0.62 0.96) (V2 0.55 0.96))
       , (CubicBezier (V2 0.55 0.96) (V2 0.49 0.90) (V2 0.40 0.83) (V2 0.35 0.80))
       , (CubicBezier (V2 0.35 0.80) (V2 0.29 0.76) (V2 0.19 0.72) (V2 0.14 0.69))
       , (CubicBezier (V2 0.14 0.69) (V2 0.09 0.65) (V2 (-0.03) 0.57) (V2 (-0.05) 0.28))
       , (CubicBezier (V2 (-0.05) 0.28) (V2 (-0.04) 0.18) (V2 (-0.02) 0.05) (V2 0.00 0.00))
    
       , (CubicBezier (V2 0.10 0.15) (V2 0.14 0.18) (V2 0.18 0.22) (V2 0.18 0.25))
       , (CubicBezier (V2 0.18 0.25) (V2 0.16 0.26) (V2 0.14 0.27) (V2 0.12 0.27))
       , (CubicBezier (V2 0.12 0.27) (V2 0.11 0.23) (V2 0.11 0.19) (V2 0.10 0.15))
    
       , (CubicBezier (V2 0.05 0.18) (V2 0.10 0.20) (V2 0.08 0.26) (V2 0.09 0.30))
       , (CubicBezier (V2 0.09 0.30) (V2 0.07 0.32) (V2 0.06 0.34) (V2 0.04 0.33))
       , (CubicBezier (V2 0.04 0.33) (V2 0.04 0.27) (V2 0.04 0.19) (V2 0.05 0.18))

       , (CubicBezier (V2 0.11 0.30) (V2 0.16 0.44) (V2 0.24 0.61) (V2 0.30 0.66))
       , (CubicBezier (V2 0.30 0.66) (V2 0.41 0.78) (V2 0.62 0.84) (V2 0.80 0.92))

       , (CubicBezier (V2 0.23 0.20) (V2 0.35 0.20) (V2 0.44 0.22) (V2 0.50 0.25))
       , (CubicBezier (V2 0.50 0.25) (V2 0.50 0.33) (V2 0.50 0.41) (V2 0.50 0.49))
       , (CubicBezier (V2 0.50 0.49) (V2 0.46 0.53) (V2 0.42 0.57) (V2 0.38 0.61))

       , (CubicBezier (V2 0.29 0.29) (V2 0.36 0.26) (V2 0.43 0.27) (V2 0.48 0.31))

       , (CubicBezier (V2 0.34 0.39) (V2 0.38 0.34) (V2 0.44 0.36) (V2 0.48 0.37))

       , (CubicBezier (V2 0.34 0.49) (V2 0.38 0.44) (V2 0.41 0.42) (V2 0.48 0.43))

       , (CubicBezier (V2 0.45 0.58) (V2 0.46 0.60) (V2 0.47 0.61) (V2 0.48 0.61))

       , (CubicBezier (V2 0.42 0.61) (V2 0.43 0.64) (V2 0.46 0.68) (V2 0.48 0.67))

       , (CubicBezier (V2 0.25 0.74) (V2 0.17 0.83) (V2 0.08 0.91) (V2 0.00 0.99))
       , (CubicBezier (V2 0.00 0.99) (V2 (-0.08) 0.91) (V2 (-0.17) 0.82) (V2 (-0.25) 0.74))
       , (CubicBezier (V2 (-0.25) 0.74) (V2 (-0.20) 0.63) (V2 (-0.11) 0.53) (V2 (-0.03) 0.43))

       , (CubicBezier (V2 (-0.17) 0.74) (V2 (-0.13) 0.66) (V2 (-0.08) 0.60) (V2 (-0.01) 0.56))

       , (CubicBezier (V2 (-0.12) 0.79) (V2 (-0.07) 0.71) (V2 (-0.02) 0.66) (V2 0.05 0.60))

       , (CubicBezier (V2 (-0.06) 0.86) (V2 (-0.03) 0.77) (V2 0.03 0.72) (V2 0.10 0.66))

       , (CubicBezier (V2 (-0.02) 0.92) (V2 0.02 0.84) (V2 0.09 0.77) (V2 0.16 0.70))
       ]

testImage = [(CubicBezier (V2 0 0) (V2 1 0) (V2 0 1) (V2 1 1))]
baseImage = beside testImage (flip testImage)


scale s = applyI (\(V2 x y) -> V2 (x * s) (y * s))

fish2 = flip $ rot45 fish
fish3 = rot $ rot $ rot fish2
t = over fish (over fish2 fish3)
u = over (over fish2 (rot fish2)) (over (rot (rot fish2)) (rot (rot (rot fish2))))

side 0 = blank
side n = quartet (side (n-1)) (side (n-1)) (rot t) t

corner 0 = blank
corner n = quartet (corner (n-1)) (side (n-1)) (rot (side (n-1))) u

nonet p q r s t u v w x = aboveS 0.34 0.66 (besideS 0.34 0.66 p (beside q r)) (above (besideS 0.34 0.66 s (beside t u)) (besideS 0.34 0.66 v (beside w x)))

squarelimit n = nonet (corner n) (side n) (rot $ rot $ rot $ corner n) (rot $ side n) u (rot $ rot $ rot $ side n) (rot $ corner n) (rot $ rot $ side n) (rot $ rot $ corner n)

scaledImage = scale 1000 $ squarelimit 3