
import Graphics.Gloss

-- Define the background color
background :: Color
background = white

-- Render a "blank" square by matching the outline color with the background
blankSquare :: Picture
blankSquare = color background $ rectangleWire 100 100

-- Function to create a graphical representation of a Triangle
renderTriangleLL :: Float -> Picture
renderTriangleLL h = translate (0) (0) $ lineLoop [(0, 0), (h, 0), (0, h)]

-- Tessellation function to repeat the pattern
tessellate :: Picture -> Picture
tessellate shape = pictures 
    [ translate (x * 100) (y * 100) shape 
    | x <- [-10..10], y <- [-10..10], even (round (x + y)) ]

-- Redefine rotateAroundTopLeft using the bounding box
rotateAroundTopLeft :: Float -> Float -> Picture -> Picture
rotateAroundTopLeft angle scaleFactor pic =
    translate (-topLeftX) (topLeftY) $             -- Step 4: Translate back to the original position
    rotate angle $                            -- Step 3: Rotate the shape
    scale scaleFactor scaleFactor $           -- Step 2: Scale the shape
    translate (topLeftX) (-topLeftY) pic     -- Step 1: Translate so top-left is at origin
  where
    ((topLeftX, topLeftY), _) = boundingBox pic

-- Main function to display the shapes using Gloss
main :: IO ()
main = do
    -- Load the BMP image
    image <- loadBMP "fishstock.bmp"  -- Change to your BMP image file

    -- Display the image along with other shapes
    display (InWindow "Gloss Shapes" (600, 600) (10, 10)) background $ drawing image


-- Combined drawing with transformations
--drawing :: Picture
--drawing = let
drawing :: Picture -> Picture
drawing image = let

    rtTriangle = renderTriangleLL 100
    myFish = rotate 315 $ scale (1) (1) image
    fish = rtTriangle
    --fish = myFish
    
    --------------------
    rot315 = translate (50) (50) $ rotateAroundTopLeft 315 (1/sqrt 2) fish
    rot45 = translate (50) (50) $ rotateAroundTopLeft 45 (1/sqrt 2) fish

    fish2 = scale (-1) (1) rot315
    fish3 = rotate 90 fish2
    fish4 = translate 100 0 fish2
    fish5 = translate 50 (-50) fish2
    t = pictures[fish,fish4,fish3]

    u = pictures[fish5,rotate 270 fish5,rotate 180 fish5,rotate 90 fish5]

    v = scale (0.5) (0.5) $ pictures[t,rotate 270 t,rotate 180 t,rotate 90 t]

    s1 = scale (0.5) (0.5) $ pictures[blankSquare,blankSquare,rotate 270 t, t]
    s2 = scale (0.5) (0.5) $ pictures[translate (-50) 100 s1,translate 50 100 s1,rotate 270 t, t]
    
    rs1 = rotate 270 s1
    
    rs2 = rotate 270 s2
    rrs2 = rotate 180 s2
    rrrs2 = rotate 90 s2


    c1 = scale (0.5) (0.5) $ pictures[blankSquare,blankSquare,blankSquare, u]
    c2 = scale (0.5) (0.5) $ pictures[translate (-75) 75 c1,translate 0 50 s1,translate (-50) 0 rs1,u]

    rc2 = rotate 270 c2
    rrc2 = rotate 180 c2
    rrrc2 = rotate 90 c2 


    np = c2
    nq = s2
    nr = rrrc2

    ns = rs2
    nt = u
    nu = rrrs2

    nv = rc2
    nw = rrs2
    nx = rrc2

    

    squareLimit2 = pictures[translate (-75) 75 np,translate 0 50 nq,translate 75 75 nr,
                            translate (-50) 0 ns,nt,translate 50 0 nu,
                            translate (-75) (-75) nv,translate 0 (-50) nw,translate 75 (-75) nx]


    
    myTess = tessellate (scale (0.5) (0.5) squareLimit2)

    
  --in overlapped
  in squareLimit2


-- Function to calculate bounding box manually for specific shapes
boundingBox :: Picture -> (Point, Point)
boundingBox (Polygon ps) = ((minimum xs, minimum ys), (maximum xs, maximum ys))
  where (xs, ys) = unzip ps

boundingBox (Translate x y p) = ((x + fst l, y + snd l), (x + fst u, y + snd u))
  where ((l, u)) = boundingBox p

boundingBox (Scale sx sy p) = ((sx * fst l, sy * snd l), (sx * fst u, sy * snd u))
  where ((l, u)) = boundingBox p

boundingBox (Rotate angle p) = rotateBoundingBox angle (boundingBox p)

boundingBox (Pictures ps) = combineBoundingBoxes $ map boundingBox ps

boundingBox (Circle r) = ((-r, -r), (r, r))

boundingBox (ThickCircle r t) = ((-r-t, -r-t), (r+t, r+t))

boundingBox _ = ((0, 0), (0, 0))  -- Default case for unsupported shapes

-- Helper functions to manage known dimensions for rectangles
rectangleBoundingBox :: Float -> Float -> (Point, Point)
rectangleBoundingBox w h = ((-w/2, -h/2), (w/2, h/2))

-- Combine bounding boxes for composite pictures
combineBoundingBoxes :: [(Point, Point)] -> (Point, Point)
combineBoundingBoxes bbs = ((minimum xs, minimum ys), (maximum xs, maximum ys))
  where
    (lowerPoints, upperPoints) = unzip bbs
    (xs, ys) = (map fst lowerPoints ++ map fst upperPoints, map snd lowerPoints ++ map snd upperPoints)

-- Rotate bounding box
rotateBoundingBox :: Float -> (Point, Point) -> (Point, Point)
rotateBoundingBox angle ((lx, ly), (ux, uy)) = ((minX, minY), (maxX, maxY))
  where
    rad = angle * pi / 180
    cosA = cos rad
    sinA = sin rad
    corners = [ (lx, ly), (lx, uy), (ux, ly), (ux, uy) ]
    rotatedCorners = [(x * cosA - y * sinA, x * sinA + y * cosA) | (x, y) <- corners]
    xs = map fst rotatedCorners
    ys = map snd rotatedCorners
    minX = minimum xs
    minY = minimum ys
    maxX = maximum xs
    maxY = maximum ys
