module ShaderAlgos
( escapeTime
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

type ShaderAlgo = Int -> (Int, Int, Int)

infrared :: ShaderAlgo
infrared n = (min 254 (n * 9), 0, 0)

escapeTime :: ShaderAlgo
escapeTime n =
    hsv2rgb hue sat val
    where
        scaled = 360 - min 360 (n * 9)
        hue = fromIntegral scaled
        sat = 1.0
        val = 1.0
