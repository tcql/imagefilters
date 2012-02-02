module Graphics.Filters.GD 
( 
    -- * Filters
    brightness
    , colorize
    , contrast
    , gaussianBlur
    , grayscale
    , edgeDetect
    , emboss
    , meanRemoval
    , negative
    , smoothing
    -- * Pixel transformation functions
    , pixelTransform
    , convolute
) where
    
import Graphics.GD
import Graphics.Filters.Util
import Control.Monad (mapM_,foldM)
import Control.Applicative ((<$>),(<*>)) 

{- |
    Performs the supplied transform function on every pixel of the image.
    The transform function should take as it's argument a quadruple of Ints (RGBA) and returns a new RGBA quadruple
    which is will be the new RGBA values of the the pixel.

    For example, the 'colorize' filter's inner workings are implemented with this as the transform function:

    @
      (\\(r,g,b,a) -> let
            nr = clamp 0 255 (r+ar)
            ng = clamp 0 255 (g+ag)
            nb = clamp 0 255 (b+ab)
            na = clamp 0 127 (a+aa)
        in (nr,ng,nb,na))
    @
-}
pixelTransform ::    Image 
                    -> ( RGBA -> RGBA ) -- ^ Transform function to be performed on each pixel
                    -> IO ()
pixelTransform img fx = do
    (width,height) <- imageSize img
    mapM_ (\(x,y) -> do
            curr <- getPixel (x,y) img
            let
                (r,g,b,a) = toRGBA curr
                (nr,ng,nb,na) = fx (r,g,b,a)
            setPixel (x,y) (rgba nr ng nb na) img
        ) $ (,) <$> [0..(width-1)] <*> [0..(height-1)]

{- |
    Performs the convolution matrix on each pixel of the original image.
    After the matrix has been applied, the resulting RGBA value has each of it's elements divided by the Divisor argument
    and then the Offset argument is added to each element

    For example, the 'emboss' filter is implemented with the following convolution:

    >   emboss img = convolute img [[1.5,0.0,0.0],[0.0,0.0,0.0],[0.0,0.0,-1.5]] 1 127
-}
convolute ::    Image 
                -> [[Float]]    -- ^ Convolution matrix
                -> Float        -- ^ Divisor
                -> Float        -- ^ Offset
                -> IO ()
convolute img matrix fdiv offset = do
    (width,height) <- imageSize img
    imgCpy <- copyImage img
    mapM_ (\(x,y) -> 
            convoluteImage img imgCpy matrix fdiv offset x y
        ) $ (,) <$> [0..(width-1)] <*> [0..(height-1)]

convoluteImage :: Image -> Image -> [[Float]] -> Float -> Float -> Int -> Int -> IO ()
convoluteImage img imgCpy matrix fdiv offset x y = do
    (nr,ng,nb,na) <- foldM (\(or,og,ob,oa) (k,j) -> do
            let 
                yy = min (max (y-(1+j)) 0) (max (y-1) 0)
                xx = min (max (x-(1+k)) 0) (max (x-1) 0)
                mVal = matrix!!j!!k
            curr <- getPixel (xx,yy) imgCpy
            let (r,g,b,a) = toRGBA curr
            return ( or + fromIntegral r * mVal
                    ,og + fromIntegral g * mVal
                    ,ob + fromIntegral b * mVal
                    ,fromIntegral a) 
        ) (0.0,0.0,0.0,0.0) $ (,) <$> [0..(length (matrix!!0) - 1)] <*> [0..(length matrix - 1)]
    let 
        new_r = clamp 0 255 . truncate $ (nr/fdiv)+offset
        new_g = clamp 0 255 . truncate $ (ng/fdiv)+offset
        new_b = clamp 0 255 . truncate $ (nb/fdiv)+offset
    setPixel (x,y) (rgba new_r new_g new_b (truncate na)) img

{- |
    Applies the supplied color transformation to the image.
    The range of the passed RGB values are -255 to +255, and the range of the A value is -127 to +127.
-}
colorize :: Image -> RGBA -> IO ()
colorize img (ar,ag,ab,aa) =
    if or [ar > 255, ar < (-255), ag > 255, ag < (-255), ab > 255, ab < (-255), aa > 127, aa < (-127)] then
        error "Argument out of bounds. Colorize expects the color argument's RGB components to be between -255 and +255\
                \ and it's A component between -127 and +127"
    else
        pixelTransform img (\(r,g,b,a) -> let
                nr = clamp 0 255 (r+ar)
                ng = clamp 0 255 (g+ag)
                nb = clamp 0 255 (b+ab)
                na = clamp 0 127 (a+aa)
            in (nr,ng,nb,na))

{- |
    Inverts the image's color.
-}
negative :: Image -> IO ()
negative img =
    pixelTransform img (\(r,g,b,a) -> let
            nr = abs 255-r
            ng = abs 255-g
            nb = abs 255-b
        in (nr,ng,nb,a))

{- |
    Converts an image to grayscale
-}
grayscale :: Image -> IO ()
grayscale img = 
    pixelTransform img (\(r,g,b,a) -> let
            newcol = truncate $ 0.299 * fromIntegral r + 0.587 * fromIntegral g + 0.114 * fromIntegral b
        in (newcol, newcol, newcol, a))        

{- |
    Applies the supplied contrast adjustment to the image. 
    The range of the contrast argument is -100 to +100, with
    -100 being maximum contrast, and +100 being minimum contrast
-}
contrast :: Image -> Int -> IO ()
contrast img contVal = 
    if or [contVal > 100, contVal < (-100)] then
        error "Argument out of bounds. Contrast expects the contrast argument to be between -100 and +100"
    else
        pixelTransform img (\(r,g,b,a) -> let
                contr = (**2) $ (100.0 - fromIntegral contVal )/100.0
                (ur,ug,ub,_) = normalizeRGBA (r,g,b,a)
                nr = clamp 0 255 $ (*) 255 . (+) 0.5 . (*) contr $ ur-0.5
                ng = clamp 0 255 $ (*) 255 . (+) 0.5 . (*) contr $ ug-0.5
                nb = clamp 0 255 $ (*) 255 . (+) 0.5 . (*) contr $ ub-0.5
            in (truncate nr, truncate ng, truncate nb, a))

{- |
    Applies the supplied brightness adjustment to the image.
    The range of the brightness argument is -255 to +255
-}
brightness :: Image -> Int -> IO ()
brightness img brightVal = 
    if or [brightVal > 255, brightVal < (-255)] then
        error "Argument out of bounds. Brightness expects the brightness argument to be between -255 and +255"
    else
        colorize img (brightVal,brightVal,brightVal,0)

{- |
    Applies Gaussian blur to the image
-}
gaussianBlur :: Image -> IO ()
gaussianBlur img = convolute img [[1.0,2.0,1.0],[2.0,4.0,2.0],[1.0,2.0,1.0]] 16 0

{- |
    Applies an Emboss effect to the image
-}
emboss :: Image -> IO ()
emboss img = convolute img [[1.5,0.0,0.0],[0.0,0.0,0.0],[0.0,0.0,-1.5]] 1 127

{- | 
    Applies Edge Detection to the image
-}
edgeDetect :: Image -> IO ()
edgeDetect img = convolute img [[-1.0,0.0,-1.0],[0.0,4.0,0.0],[-1.0,0.0,-1.0]] 1 127

{- |
    Applies a Mean Removal effect to the image
-}
meanRemoval :: Image -> IO ()
meanRemoval img = convolute img [[-1.0,-1.0,-1.0],[-1.0,9.0,-1.0],[-1.0,-1.0,-1.0]] 1 0

{- |
    Applies weighted Smoothing to the image. The smoothing amount is technically unbounded, 
    but larger values produce a less noticeable result
-}
smoothing :: Image -> Float -> IO ()
smoothing img weighting= convolute img [[1.0,1.0,1.0],[1.0,weighting,1.0],[1.0,1.0,1.0]] (weighting+8.0) 0
