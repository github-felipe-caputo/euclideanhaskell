import Graphics.Gloss
import Help

main = display 
       (InWindow "Spades" (640, 480) (10, 10)) -- window
       white                                     -- background color
       (spades (0,0) 100)                       -- drawings

-- Pentagon

spades :: Point -> Float -> Picture
spades a s =
  let b = north (MyCircle a s)
      c = onePoint (intersect (MyCircle a s) (MyCircle b s))
      d = onePoint (intersect (MyCircle a s) (MyCircle c s))
      e = onePoint (intersect (MyCircle a s) (MyCircle d s))
      f = onePoint (intersect (MyCircle a s) (MyCircle e s))
      g = onePoint (intersect (MyCircle a s) (MyCircle f s))
      h = right (intersect (MyCircle e s) (MyCircle d s))
      i = left (intersect (MyCircle e s) (MyCircle f s))
      j = right (intersect (MyCircle b s) (MyCircle c s))
      k = left (intersect (MyCircle b s) (MyCircle g s))
      l = north (MyCircle b s)
      m = south (MyCircle i s)
      n = south (MyCircle h s)
      o = left (intersect (MyCircle f s) (MyCircle g s))
      p = right (intersect (MyCircle c s) (MyCircle d s))
      in Pictures [ Color red  $ draw (MyCircle a s)
                  , Color red  $ draw (MyCircle b s)
                  , Color red  $ draw (MyCircle c s)
                  , Color red  $ draw (MyCircle d s)
                  , Color red  $ draw (MyCircle e s)
                  , Color red  $ draw (MyCircle f s)
                  , Color red  $ draw (MyCircle g s)
                  , Color red  $ draw (MyCircle h s)
                  , Color red  $ draw (MyCircle i s)
                  , Color red  $ draw (MyCircle j s)
                  , Color red  $ draw (MyCircle k s)
                  -- the actual spades
                  , Color blue $ draw (MyLine m n)
                  , Color blue $ draw (MyArc (right (intersect (MyCircle h s) (MyCircle d s))) e n)
                  , Color blue $ draw (MyArc (left (intersect (MyCircle i s) (MyCircle f s))) m e)
                  , Color blue $ draw (MyArc f o e)
                  , Color blue $ draw (MyArc d e p)
                  , Color blue $ draw (MyLine o l)
                  , Color blue $ draw (MyLine p l)
                  -- points
                  , point a,  label a "a"
                  , point b,  label b "b"
                  , point c,  label c "c"
                  , point d,  label d "d"
                  , point e,  label e "e"
                  , point f,  label f "f"
                  , point g,  label g "g"
                  , point h,  label h "h"
                  , point i,  label i "i"
                  , point j,  label j "j"
                  , point k,  label k "k"
                  , point l,  label l "l"
                  , point m,  label m "m"
                  , point n,  label n "n"
                  , point o,  label o "o"
                  , point p,  label p "p"
                  ]