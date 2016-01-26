import Graphics.Gloss
import Help

main = display 
       (InWindow "Amati" (640, 480) (10, 10)) -- window
       white                                  -- background color
       amati'                                 -- drawings

-- Amati
amati' :: Picture
amati' = 
  let xq = 400
      -- Layout of the area
      xc = (0,0)
      ac = xshift xc (-(xq/2))
      qc = yshift xc xq
      nc = midpoint xc (midpoint xc qc)
      q  = xshift (onePoint (intersect (horizontal qc) (vertical ac))) ((distance xc nc)/2)
      vv = xshift ac ((distance xc nc)/8)
      oc = yshift qc (-((distance xc nc)*(5/4)))
      zc = yshift nc ((distance xc nc)*(2/3))
      pc = yshift xc (-((distance xc nc)*(8/3))) -- you can change here
      p  = onePoint (intersect (horizontal pc) (vertical vv))
      mc = midpoint xc pc
      a  = xshift ac ((distance xc zc)/2)
      b  = xshift zc (-((distance ac a)/2))
      ee = xshift (onePoint (intersect (vertical b) (horizontal nc))) (-(xdistance b p)*(3/8))
      c  = xshift (onePoint (intersect (vertical p) (horizontal xc))) ((xdistance ee p)/4)
      d  = xshift (onePoint (intersect (vertical p) (horizontal xc))) ((xdistance ee p)/2)
      h  = xshift (onePoint (intersect (vertical ee) (horizontal zc))) (-(xdistance ee p)/4)
      g  = xshift (onePoint (intersect (vertical ee) (horizontal zc))) (-(xdistance ee p)/2)
      -- lower bouts
      zmcircle     = MyCircle zc (distance zc mc)
      zpcircle     = MyCircle zc (distance zc pc)
      m            = bottom (intersect (makeLine 1 p) zmcircle)
      mcircle      = MyCircle m (distance mc pc)
      n            = xshift m ((distance xc zc)-(distance mc pc))
      ncircle      = MyCircle n (distance xc zc)
      lowercircle  = lowerCircle (reverseCircle (MyCircle n (distance xc zc)) ((distance xc zc)+((distance xc nc)/2)) c)
      lfirstarc    = MyArc zc (onePoint (intersect zpcircle mcircle)) pc
      lsecondarc   = MyArc m (onePoint (intersect mcircle ncircle)) (onePoint (intersect zpcircle mcircle))
      lthirdarc    = MyArc n (onePoint (intersect ncircle lowercircle)) (onePoint (intersect mcircle ncircle))
      lfourtharc   = MyArc (center lowercircle) (onePoint (intersect ncircle lowercircle)) c
      -- upper bouts
      nccircle    = MyCircle nc (distance nc qc)
      o           = top (intersect (makeLine (-1) q) (MyCircle nc (distance nc oc)))
      ocircle     = MyCircle o (distance oc qc)
      uppercircle = upperCircle (reverseCircle ocircle (distance nc oc) g)
      ufirstarc   = MyArc nc qc (onePoint (intersect nccircle ocircle))
      usecondarc  = MyArc o (onePoint (intersect nccircle ocircle)) (top (intersect (MyLine (center uppercircle) o) uppercircle))
      ufourtharc  = MyArc (center uppercircle) g (top (intersect (MyLine (center uppercircle) o) uppercircle))
      -- middle bouts
      f            = xshift ee (-(distance xc zc))
      v            = xshift ee (-((distance xc nc)/2))
      s            = xshift ee (-((distance nc zc)/2))
      ecircle      = MyCircle f (distance f ee)
      vcircle      = MyCircle f (distance f v)
      scircle      = MyCircle f (distance f s)
      lowercircleM = upperCircle (reverseCircle ecircle (distance f v) d)
      uppercircleM = lowerCircle (reverseCircle ecircle (distance f s) h)
      mfirstarc    = MyArc (center uppercircleM) (onePoint (intersect uppercircleM ecircle)) (top (intersect uppercircle uppercircleM))
      msecondarc   = MyArc f (onePoint (intersect lowercircleM ecircle)) (onePoint (intersect uppercircleM ecircle))
      mthirdarc    = MyArc (center lowercircleM) (bottom (intersect lowercircle lowercircleM)) (onePoint (intersect lowercircleM ecircle))
    in Pictures [ -- layout of the whole shape
                  Color red $ draw (horizontal nc)
                , Color red $ draw (horizontal oc)
                , Color red $ draw (horizontal zc)
                , Color red $ draw (horizontal pc)
                , Color red $ draw (horizontal xc)
                , Color red $ draw (horizontal mc)
                , Color red $ draw (vertical p)
                , Color red $ draw (vertical q)
                , Color red $ draw (vertical b)
                , Color red $ draw (vertical ee)
                , Color red $ draw (vertical zc)
                -- lower bout
                , Color red $ draw (MyCircle n (distance n (center lowercircle)))
                , Color red $ draw zpcircle
                , Color red $ draw mcircle
                , Color red $ draw ncircle
                , Color red $ draw lowercircle
                , Color red $ draw (MyLine n (center lowercircle))
                , Color red $ draw (MyLine n (onePoint (intersect mcircle ncircle)))
                , Color red $ draw (MyLine zc (onePoint (intersect zpcircle mcircle)))
                , Color blue $ draw lfirstarc
                , Color blue $ draw lsecondarc
                , Color blue $ draw lthirdarc
                , Color blue $ draw lfourtharc
                -- upper bouts
                , Color red $ draw (MyCircle o (distance o (center uppercircle)))
                , Color red $ draw nccircle
                , Color red $ draw ocircle
                , Color red $ draw uppercircle
                , Color red $ draw (MyLine nc (onePoint (intersect nccircle ocircle)))
                , Color red $ draw (MyLine o (center uppercircle))
                , Color blue $ draw ufirstarc
                , Color blue $ draw usecondarc
                , Color blue $ draw ufourtharc
                -- midle bouts
                , Color red $ draw (MyLine f (onePoint (intersect uppercircleM ecircle)))
                , Color red $ draw (MyLine f (onePoint (intersect lowercircleM ecircle)))
                , Color red $ draw ecircle                
                , Color red $ draw vcircle
                , Color red $ draw scircle
                , Color red $ draw lowercircleM
                , Color red $ draw uppercircleM
                , Color blue $ draw mfirstarc
                , Color blue $ draw msecondarc
                , Color blue $ draw mthirdarc
                -- drawing the right side now
                -- lower bout
                , Color red $ drawMirror (MyLine n (center lowercircle))
                , Color red $ drawMirror (MyLine n (onePoint (intersect mcircle ncircle)))
                , Color red $ drawMirror (MyLine zc (onePoint (intersect zpcircle mcircle)))
                , Color blue $ drawMirror lfirstarc
                , Color blue $ drawMirror lsecondarc
                , Color blue $ drawMirror lthirdarc
                , Color blue $ drawMirror lfourtharc
                -- upper bouts
                , Color red $ drawMirror (MyLine nc (onePoint (intersect nccircle ocircle)))
                , Color red $ drawMirror (MyLine o (center uppercircle))
                , Color blue $ drawMirror ufirstarc
                , Color blue $ drawMirror usecondarc
                , Color blue $ drawMirror ufourtharc
                -- midle bouts
                , Color red $ drawMirror (MyLine c (center lowercircle))
                , Color red $ drawMirror (MyLine c (center lowercircleM))
                , Color red $ drawMirror (MyLine g (center uppercircle))
                , Color red $ drawMirror (MyLine g (center uppercircleM))
                , Color red $ drawMirror (MyLine f (onePoint (intersect uppercircleM ecircle)))
                , Color red $ drawMirror (MyLine f (onePoint (intersect lowercircleM ecircle)))
                , Color blue $ drawMirror mfirstarc
                , Color blue $ drawMirror msecondarc
                , Color blue $ drawMirror mthirdarc
                -- points!
                , point f,  label f "f"
                , point m,  label m "m"
                , point n,  label n "n"
                , point xc, label xc "X"
                , point ac, label ac "A"
                , point qc, label qc "Q"
                , point nc, label nc "N"
                , point q,  label q "q"
                , point oc, label oc "O"
                , point zc, label zc "Z"
                , point pc, label pc "P"
                , point p,  label p "p"
                , point mc, label mc "M"
                , point a,  label a "a"
                , point b,  label b "b"
                , point ee, label ee "e"
                , point c,  label c "c"
                , point d,  label d "d"
                , point h,  label h "h"
                , point g,  label g "g"
                , point o,  label o "o"
                , point v,  label v "v"
                , point s,  label s "s"
                , point (mirror f),  label (mirror f) "f'"
                , point (mirror m),  label (mirror m) "m'"
                , point (mirror n),  label (mirror n) "n"
                , point (mirror ac), label (mirror ac) "A'"
                , point (mirror q),  label (mirror q) "q'"
                , point (mirror p),  label (mirror p) "p'"
                , point (mirror a),  label (mirror a) "a'"
                , point (mirror b),  label (mirror b) "b'"
                , point (mirror ee), label (mirror ee) "e'"
                , point (mirror c),  label (mirror c) "c'"
                , point (mirror d),  label (mirror d) "d'"
                , point (mirror h),  label (mirror h) "h'"
                , point (mirror g),  label (mirror g) "g'"
                , point (mirror o),  label (mirror o) "o'"
                , point (mirror v),  label (mirror v) "v'"
                , point (mirror s),  label (mirror s) "s'"
                ]