import Graphics.Gloss
import Help

main = display 
       (InWindow "Hexagon" (640, 480) (10, 10)) -- window
       white                                     -- background color
       (hexagon (0,0) 100)                       -- drawings

-- Pentagon

hexagon :: Point -> Float -> Picture
hexagon a s =
  let b = north (MyCircle a s)
      c = onePoint (intersect (MyCircle a s) (MyCircle b s))
      d = onePoint (intersect (MyCircle a s) (MyCircle c s))
      e = onePoint (intersect (MyCircle a s) (MyCircle d s))
      f = onePoint (intersect (MyCircle a s) (MyCircle e s))
      g = onePoint (intersect (MyCircle a s) (MyCircle f s))
      in Pictures [ Color red  $ draw (MyCircle a s)
                  , Color red  $ draw (MyCircle b s)
                  , Color red  $ draw (MyCircle c s)
                  , Color red  $ draw (MyCircle d s)
                  , Color red  $ draw (MyCircle e s)
                  , Color red  $ draw (MyCircle f s)
                  , Color red  $ draw (MyCircle g s)
                 -- the actual hexagon
                  , Color blue $ draw (MyLines [b,c,d,e,f,g,b])
                  -- points
                  , point a,  label a "a"
                  , point b,  label b "b"
                  , point c,  label c "c"
                  , point d,  label d "d"
                  , point e,  label e "e"
                  , point f,  label f "f"
                  , point g,  label g "g"
                  ]