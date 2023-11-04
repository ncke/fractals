module ShaderAlgos
( blackAndWhite
, blueLagoon
, escapeTime
, infrared
, ShaderAlgo
) where

import Data.Colour.RGBSpace.HSV (hsv)
import Data.Colour.RGBSpace(RGB(..))

hsv2rgb :: Float -> Float -> Float -> (Int, Int, Int)
hsv2rgb h s v = 
    (red, grn, blu)
    where
        rgb = hsv h s v
        rounded f = round (254.0 * f)
        red = rounded (channelRed rgb)
        grn = rounded (channelGreen rgb)
        blu = rounded (channelBlue rgb)

type ShaderAlgo = Int -> Int -> (Int, Int, Int)

blackAndWhite :: ShaderAlgo
blackAndWhite greatest n = 
    (min 254 n', min 254 n', min 254 n')
    where
        n' = n * 10

infrared :: ShaderAlgo
infrared greatest n = (min 254 (n * 9), 0, 0)

escapeTime :: ShaderAlgo
escapeTime greatest n =
    hsv2rgb hue sat val
    where
        scaled = div (n * 360) greatest
        hue = fromIntegral scaled
        sat = 1.0
        val = 1.0

blueLagoon :: ShaderAlgo
blueLagoon greatest n =
    hsv2rgb hue sat val
    where
        scaled = 240 - (mod n 240)
        hue = fromIntegral scaled
        sat = 1.0
        val = 1.0  