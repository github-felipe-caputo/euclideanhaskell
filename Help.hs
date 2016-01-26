module Help where

import Graphics.Gloss

-- Our own primitives
data Primitive = MyLine Point Point        |
                 MyLines [Point]           |
                 MyCircle Point Float      |
                 MyArc Point Point Point   deriving (Eq,Show)


--
-- Drawing
--

-- functions that return pictures
draw :: Primitive -> Picture
draw (MyLine p0 p1)       = Line [p0, p1]                     -- Draws a line given two points
draw (MyLines lst)        = Line lst   
draw (MyCircle (px,py) r) = Translate px py $ Circle r        -- Draws a circle given the center and a radius
draw (MyArc p1 p2 p3)     = makearc p1 p2 p3

-- mirror drawinds
drawMirror :: Primitive -> Picture
drawMirror (MyLine (x1,y1) (x2,y2))      = Line [(-x1,y1), (-x2,y2)]                     
drawMirror (MyLines lst)                 = Line (map (\(x,y) -> (-x,y)) lst)   
drawMirror (MyCircle (px,py) r)          = Translate (-px) py $ Circle r 
drawMirror (MyArc (x,y) (x1,y1) (x2,y2)) = makearc (-x,y) (-x2,y2) (-x1,y1)

-- Draws a counter-clockwise arc, first argument is the center and the other
-- points are the beggining and end of the arc
makearc :: Point -> Point -> Point -> Picture
makearc (x,y) (x1,y1) (x2,y2) =
  let (lp1x,lp1y) = (x1-x,y1-y)
      (lp2x,lp2y) = (x2-x,y2-y)
      arcSize = distance (x,y) (x1,y1)
      angleFirstPoint = (atan2 lp1y lp1x) * 180/pi
      angleSecondPoint = (atan2 lp2y lp2x) * 180/pi
  in (Translate x y $ Arc angleFirstPoint angleSecondPoint arcSize)

-- drawing a point and label
point :: Point -> Picture
point (x,y) = Translate x y $ ThickCircle 0 2

label :: Point -> String -> Picture
label (x,y) label = Translate (x+2) (y+2) $ Scale 0.05 0.05 $ Text label

--
-- Vertical and Horizontal lines
--

vertical :: Point -> Primitive
vertical (x,_) = MyLine (x,-1000) (x,1000)

horizontal :: Point -> Primitive
horizontal (_,y) = MyLine (-1000,y) (1000,y)

--
-- Make line with a slope
--

makeLine :: Float -> Point -> Primitive
makeLine s (x,y) = 
  let b  = y-(s*x)
      x2 = x+1
      y2 = (s*x2)+b
  in MyLine (x,y) (x2,y2)

--
-- Distance between points
--

distance :: Point -> Point -> Float
distance (x1,y1) (x2,y2) = sqrt (dx*dx + dy*dy)
    where dx = x2 - x1
          dy = y2 - y1

xdistance :: Point -> Point -> Float
xdistance (x1,_) (x2,_) = abs (x2-x1)

--
-- midpoint
--

midpoint :: Point -> Point -> Point
midpoint (x1,y1) (x2,y2) = ((x1+x2)/2,(y1+y2)/2)

--
-- Shifts
--        

xshift :: Point -> Float -> Point
xshift (x,y) s = (x+s,y)

yshift :: Point -> Float -> Point
yshift (x,y) s = (x,y+s)

--
-- Mirror
--

mirror :: Point -> Point
mirror (x,y) = (-x,y)

-- 
-- Center
--

center :: Primitive -> Point
center (MyCircle p _) = p

--
-- Reverse Circle
--

reverseCircle :: Primitive -> Float -> Point -> [Primitive]
reverseCircle (MyCircle c r) outer pt =
  let outerRadius = outer - r
      (p1,p2) = twoPoints (intersect (MyCircle pt (abs outerRadius)) (MyCircle c (outerRadius+r)))
  in [MyCircle p1 (abs outerRadius),MyCircle p2 (abs outerRadius)]

--
-- Lower Circle
--

lowerCircle :: [Primitive] -> Primitive
lowerCircle (x:y:xs)
  | y1 > y2   = y
  | otherwise = x
  where (MyCircle (_,y1) r1) = x
        (MyCircle (_,y2) r2) = y

upperCircle :: [Primitive] -> Primitive
upperCircle (x:y:xs)
  | y1 < y2   = y
  | otherwise = x
  where (MyCircle (_,y1) r1) = x
        (MyCircle (_,y2) r2) = y

-- 
-- Circle quadrants
--

north :: Primitive -> Point
north (MyCircle (px,py) r) = (px,py+r)

south :: Primitive -> Point
south (MyCircle (px,py) r) = (px,py-r)

east :: Primitive -> Point
east (MyCircle (px,py) r) = (px+r,py)

west :: Primitive -> Point
west (MyCircle (px,py) r) = (px-r,py)

--
-- Intersections
--

intersect :: Primitive -> Primitive -> Maybe [Point]
intersect (MyLine (x1,y1) (x2,y2)) (MyLine (x3,y3) (x4,y4))         -- lines
    | d == 0    = Nothing -- parallel lines
    | otherwise = Just [(xi, yi)]
    where d  = (x1-x2) * (y3-y4) - (y1-y2) * (x3-x4)
          xi = ((x3-x4)*(x1*y2-y1*x2)-(x1-x2)*(x3*y4-y3*x4))/d
          yi = ((y3-y4)*(x1*y2-y1*x2)-(y1-y2)*(x3*y4-y3*x4))/d
intersect (MyLine (x1,y1) (x2,y2)) (MyCircle (px,py) r)             -- line circle
    | delta < 0  = Nothing -- no intersection
    | delta == 0 =         -- one intersection
        let u = -b / (2 * a)                                        
            fpx = x1+(u * difx)
            fpy = y1+(u * dify)
        in Just [(fpx,fpy)]
    | otherwise  =        -- two intersection
        let sqrtDelta = (sqrt delta)
            u1 = (-b + sqrtDelta) / (2 * a)
            u2 = (-b - sqrtDelta) / (2 * a)
            fpx1 = x1+(u1 * difx)
            fpy1 = y1+(u1 * dify)
            fpx2 = x1+(u2 * difx)
            fpy2 = y1+(u2 * dify)
        in Just [(fpx1,fpy1), (fpx2,fpy2)]
    where (lp1x, lp1y) = (x1 - px, y1 - py)
          (lp2x, lp2y) = (x2 - px, y2 - py)
          (difx, dify) = (lp2x - lp1x, lp2y - lp1y)
          a = difx * difx + dify * dify
          b = 2 * ((difx * lp1x) + (dify * lp1y))
          c = (lp1x * lp1x) + (lp1y * lp1y) - (r * r)
          delta = b * b - (4 * a * c)
intersect (MyCircle (x1,y1) r1) (MyCircle (x2,y2) r2) 
   | d == 0 && r1 == r2 = Nothing -- circles are coincident, for our purposes we return nothing because this case will not happen
   | otherwise          = 
        let a = (r1*r1 - r2*r2 + d*d) / (2*d) 
            h = sqrt (r1*r1 - a*a)
            (px,py) = (x1+a*(x2-x1)/d, y1+a*(y2-y1)/d)
            fpx1 = px + h * (y2-y1) / d
            fpy1 = py - h * (x2-x1) / d
            fpx2 = px - h * (y2-y1) / d
            fpy2 = py + h * (x2-x1) / d
        in if (d == r1+r2) -- might have one or two intersections
            then Just [(fpx1,fpy1)]                         
            else Just [(fpx1,fpy1),(fpx2,fpy2)]
    where (difx, dify) = (x1 - x2, y1 - y2)
          d = sqrt (difx * difx + dify * dify)

--
-- Unwrapping intersections
--

onePoint :: Maybe [Point] -> Point
onePoint (Just (x:xs)) = x

twoPoints :: Maybe [Point] -> (Point, Point)
twoPoints (Just (x:y:xs)) = (x,y)

--
-- Choosing points
--

top :: Maybe [Point] -> Point
top (Just lst) 
    | y1 > y2   = (x1,y1)
    | otherwise = (x2,y2)
    where (x1,y1) = lst !! 0
          (x2,y2) = lst !! 1

bottom :: Maybe [Point] -> Point
bottom (Just lst) 
    | y1 < y2   = (x1,y1)
    | otherwise = (x2,y2)
    where (x1,y1) = lst !! 0
          (x2,y2) = lst !! 1

right :: Maybe [Point] -> Point
right (Just lst) 
    | x1 > x2   = (x1,y1)
    | otherwise = (x2,y2)
    where (x1,y1) = lst !! 0
          (x2,y2) = lst !! 1

left :: Maybe [Point] -> Point
left (Just lst) 
    | x1 < x2   = (x1,y1)
    | otherwise = (x2,y2)
    where (x1,y1) = lst !! 0
          (x2,y2) = lst !! 1

