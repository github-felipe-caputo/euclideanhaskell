import Graphics.Gloss
import Help

main = display 
       (InWindow "Pentagon2" (640, 480) (10, 10)) -- window
       white                                  -- background color
       (pentagon2' (0,0) 100)                  -- drawings

-- Pentagon no Compass

pentagon2' :: Point -> Float -> Picture
pentagon2' a s = 
  let b = xshift a s
      t = yshift (midpoint a b) (s * (14/9))
      d = yshift (xshift b (s * (3/10))) (s * (20/21))
      c = yshift (xshift a (s * (-3/10))) (s * (20/21))
  in Pictures [ Color blue $ draw (MyLines [a,b,d,t,c,a])
              , point d,  label d "d"
              , point a,  label a "a"
              , point t,  label t "t"
              , point b,  label b "b"
              , point c,  label c "c"
              ]