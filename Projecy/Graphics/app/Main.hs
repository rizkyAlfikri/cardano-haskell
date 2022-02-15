import Graphics.Gloss

main :: IO ()
main = display window background drawing
        where
            window = InWindow "Nice Window" (600, 600) (0, 0)
            background = white 
            drawing = picture

picture = pictures [
    translate 100 100 ( color blue (circleSolid 60)),
    translate 0 100 (color red (circleSolid 25)),
    trianglePicture,
    trianglePolygon
    ]

trianglePicture = translate 50 0  (line [(0,0),(10,-70),(-10,-70),(0,0)])

trianglePolygon = polygon [(-75,-100), (0,87), (75,-100)]

