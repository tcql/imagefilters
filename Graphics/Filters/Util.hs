module Graphics.Filters.Util where

type RGBA = (Int,Int,Int,Int)

-- | Utility function for clamping a value between a minimum and maximum value
clamp :: (Ord a, Num a) =>  a       -- ^ Minimum
                            -> a    -- ^ Maximum
                            -> a    -- ^ Value to clamp
                            -> a
clamp minm maxm num 
	| num < minm = minm
	| num > maxm = maxm
	| otherwise = num

-- | Function for converting RGBA from an Int 0-255 range to a Float 0.0-1.0 range 
normalizeRGBA :: RGBA -> (Float,Float,Float,Float)
normalizeRGBA (r,g,b,a) = (fromIntegral r/255,fromIntegral g/255,fromIntegral b/255,fromIntegral a/127)


