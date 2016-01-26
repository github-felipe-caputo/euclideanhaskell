import Graphics.Gloss
import Help

main = display 
       (InWindow "Lute" (640, 480) (10, 10)) -- window
       white                                  -- background color
       (lute' (0,0) 100)                      -- drawings

-- Lute

lute' :: Point -> Float -> Picture
lute' origin rad = 
  let c = MyCircle origin rad
      d = 2*rad
      p = west c
      q = north c
      qprime = south c
      r = left (intersect (MyLine (mirror p) q) (MyCircle (mirror p) d))
      s = yshift q (distance q r)
      rose = MyCircle (0,rad/2) (3/10 * rad)
  in Pictures [ -- drawing lines
                Color red $ draw (MyCircle origin rad)
              , Color red $ draw (MyCircle (mirror p) d)
              , Color red $ draw (MyLine (mirror p) r)
              , Color red $ draw (MyLine p (mirror r))
              , Color red $ draw (MyLine (mirror p) p)
              , Color red $ draw (MyCircle q (distance q r))
              , Color red $ draw (MyLine qprime s)
              -- left side
              , Color blue $ draw (MyArc (center rose) (north rose) (west rose))
              , Color blue $ draw (MyArc (center rose) (west rose) (south rose))
              , Color blue $ draw (MyArc origin p qprime)
              , Color blue $ draw (MyArc (mirror p) r p)
              , Color blue $ draw (MyArc q s r)
              -- now the right side
              , Color blue $ drawMirror (MyArc (center rose) (north rose) (west rose))
              , Color blue $ drawMirror (MyArc (center rose) (west rose) (south rose))
              , Color blue $ drawMirror (MyArc origin p qprime)
              , Color blue $ drawMirror (MyArc (mirror p) r p)
              , Color blue $ drawMirror (MyArc q s r)
              -- points!
              , point p, label p "p"
              , point q, label q "q"
              , point r, label r "r"
              , point qprime, label qprime "q'"
              , point (mirror p), label (mirror p) "p"
              , point (mirror r), label (mirror r) "r"
              ]