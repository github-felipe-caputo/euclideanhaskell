import Graphics.Gloss
import Help

main = display 
       (InWindow "Pentagon" (640, 480) (10, 10)) -- window
       white                                  -- background color
       (pentagon' (0,0) 100)                  -- drawings

-- Pentagon

pentagon' :: Point -> Float -> Picture
pentagon' a s =
  let b = xshift a s
      (x,y) = twoPoints (intersect (MyCircle a s) (MyCircle b s))
      c = onePoint (intersect (MyLine a b) (MyLine x y))
      d = top (intersect (vertical c) (MyCircle c s))
      f = top (intersect (MyLine b d) (MyCircle d (distance a c)))
      g = top (intersect (vertical c) (MyCircle b (distance b f)))
      p = left (intersect (MyCircle g s) (MyCircle a s))
      q = right (intersect (MyCircle g s) (MyCircle b s))
      in Pictures [ Color red  $ draw (MyLine b f)
                  , Color red  $ draw (MyCircle a s)
                  , Color red  $ draw (MyCircle c s)
                  , Color red  $ draw (MyCircle d (distance d f))
                  , Color red  $ draw (MyCircle b (distance b f))
                  -- the actual pentagon
                  , Color blue $ draw (MyLines [g,p,a,b,q,g])
                  -- points
                  , point g,  label g "g"
                  , point a,  label a "a"
                  , point p,  label p "p"
                  , point b,  label b "b"
                  , point q,  label q "q"
                  ]