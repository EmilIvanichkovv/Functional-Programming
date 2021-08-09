module Task2 where

import Data.Word

data Rgb = Rgb
  { red :: Word8,
    green :: Word8,
    blue :: Word8
  }
  deriving (Show, Read)

data Image = Image
  { width :: Int,
    height :: Int,
    content :: [[Rgb]]
  }
  deriving (Show, Read)

testImage =
  Image
    3
    2
    [ [Rgb 255 0 0, Rgb 155 128 0, Rgb 255 255 0],
      [Rgb 0 255 0, Rgb 255 255 255, Rgb 128 255 128]
    ]

testImage2 :: Image
testImage2 = Image 4 4 [[Rgb 255 0 0, Rgb 155 128 0,   Rgb 255 255 0,   Rgb 155 128 0],
                        [Rgb 0 255 0, Rgb 255 255 128, Rgb 128 255 128, Rgb 155 128 0],
                        [Rgb 0 128 0, Rgb 128 255 255, Rgb 128 255 128, Rgb 155 255 0],
                        [Rgb 0 255 0, Rgb 255 128 255, Rgb 128 128 128,  Rgb 255 128 0]]


getRed :: Rgb -> Word8
getRed (Rgb red green blue) = red
getGreen :: Rgb -> Word8
getGreen (Rgb red green blue) = green
getBlue :: Rgb -> Word8
getBlue (Rgb red green blue) = blue

getWidth :: Image -> Int
getWidth (Image width height content) = width
getHeight :: Image -> Int
getHeight (Image width height content) = height
getContent :: Image -> [[Rgb]]
getContent (Image width height content) = content


-- A)
pixelConvertor :: Rgb -> Rgb
pixelConvertor pixel =
  Rgb grayVal grayVal grayVal
   where grayVal = (truncate ((fromIntegral (getRed pixel)) * 0.30 + (fromIntegral (getGreen pixel)) * 0.59 + (fromIntegral (getBlue pixel)) * 0.11))
    
rowConvertor :: [Rgb] -> [Rgb]
rowConvertor row = map pixelConvertor row

contentConvertor :: [[Rgb]] -> [[Rgb]]
contentConvertor content = map rowConvertor content

grayscale  :: Image -> Image
grayscale img = Image (getWidth img) (getHeight img) (contentConvertor (getContent img))


-- B)

-- Sobel operator and clamp
gX :: [[Float]]
gX = [[1,0,-1],[2,0,-2],[1,0,-1]]
gY :: [[Float]]
gY = [[1,2,1],[0,0,0],[-1,-2,-1]] 

convolution :: [[Float]] -> [[Float]] -> Int
convolution m1 m2 = truncate(m1!!2!!2 * m2!!0!!0 + m1!!2!!1 * m2!!0!!1 + m1!!2!!0 * m2!!0!!2 + m1!!1!!2 * m2!!1!!0 + m1!!1!!1 * m2!!1!!1 + m1!!1!!0 * m2!!1!!2 + m1!!0!!2 * m2!!2!!0 + m1!!0!!1 * m2!!2!!1 + m1!!0!!0 * m2!!2!!2) 

sobel :: [[Float]] -> Int
sobel matrix = truncate (sqrt (fromIntegral (((convolution matrix gX) ^ 2) + ((convolution matrix gY) ^ 2))))

sobelRGBCorrection :: [[Float]] -> Int
sobelRGBCorrection matrix
  | res < 0 = 0
  | res > 255 = 255
  | otherwise = res 
    where res = sobel matrix

-- Extracting 3x3 submatrix 
  -- Extracting Intager matrix form image content
extracIntMatrix content = map (\row -> (map (\ a -> (fromIntegral (getRed a))) row)) content

  -- Taking all possible coordinates using Crop Edge Handling
getAllCordinates :: Image -> [(Int, Int)]
getAllCordinates img= [(x,y) | x<-[1..((getWidth img)-2)] , y<-[1..((getHeight img)-2)]]

 -- Extracting 3x3 submatrix
submatrix3x3 :: [[a]] -> Int -> Int -> [[a]]
submatrix3x3 arrOfRGBs x y = [ [arrOfRGBs !! (y-1) !! (x-1)] ++  [arrOfRGBs !! (y-1) !! (x)] ++  [arrOfRGBs !! (y-1) !! (x+1)] ]   ++ 
                             [ [arrOfRGBs !! (y) !! (x-1)] ++  [arrOfRGBs !! (y) !! (x)] ++  [arrOfRGBs !! (y) !! (x+1)] ]         ++ 
                             [ [arrOfRGBs !! (y+1) !! (x-1)] ++  [arrOfRGBs !! (y+1) !! (x)] ++  [arrOfRGBs !! (y+1) !! (x+1)] ]



-- Appying Sobel operator on every pixel - returns vector with new pixel values which shoud be converted to RGB
sobelCalc :: Image -> [Int]
sobelCalc img = (map (\ coords -> sobelCalcCurrCoord coords (getContent img)) (getAllCordinates img))
  where sobelCalcCurrCoord (x,y) matrix = sobelRGBCorrection (extracIntMatrix (submatrix3x3 matrix x y))

-- Converting the vector with new pixels values to RGB vector
helpConvertorRGBVector :: Image -> [Rgb]
helpConvertorRGBVector img = intToRGBConvertor (sobelCalc img)
  where intToRGBConvertor intVector = map (\x->(Rgb x x x)) (map (\ x -> (fromIntegral x)) intVector)

-- Making RGB matrix with proper dementions from RGB vector
helpConvertorRGBMatrix :: [a] -> Int -> [[a]]
helpConvertorRGBMatrix vector x 
  |null vector = []
  |otherwise   = [take x vector] ++ (helpConvertorRGBMatrix (drop x vector) x)

edgeDetect :: Image -> Image
edgeDetect img = (Image ((getWidth img)-2) ((getHeight img)-2) newContent)
  where newContent = helpConvertorRGBMatrix (helpConvertorRGBVector img) ((getWidth img)-2)