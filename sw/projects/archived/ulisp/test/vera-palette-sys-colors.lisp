'start

(vera :display :enable)
(vera :layer 0 :enable)
(vera :tileset 0 :width 320 :height 32 :bpp 8 :num-tiles 1)
(vera :layer 0 :bitmap :tileset 0 :tile 0)

(vera-tileset-pixel 0 0 0 0 +vera-color-black+)
(vera-tileset-pixel 0 0 1 0 +vera-color-white+)
(vera-tileset-pixel 0 0 2 0 +vera-color-red+)
(vera-tileset-pixel 0 0 3 0 +vera-color-cyan+)
(vera-tileset-pixel 0 0 4 0 +vera-color-purple+)
(vera-tileset-pixel 0 0 5 0 +vera-color-green+)
(vera-tileset-pixel 0 0 6 0 +vera-color-blue+)
(vera-tileset-pixel 0 0 7 0 +vera-color-yellow+)
(vera-tileset-pixel 0 0 8 0 +vera-color-orange+)
(vera-tileset-pixel 0 0 9 0 +vera-color-brown+)
(vera-tileset-pixel 0 0 10 0 +vera-color-light-red+)
(vera-tileset-pixel 0 0 11 0 +vera-color-dark-grey+)
(vera-tileset-pixel 0 0 12 0 +vera-color-grey+)
(vera-tileset-pixel 0 0 13 0 +vera-color-light-green+)
(vera-tileset-pixel 0 0 14 0 +vera-color-light-blue+)
(vera-tileset-pixel 0 0 15 0 +vera-color-light-grey+)









(vera-tileset-pixel 0 0 16 0 +vera-color-grayscale-0+)
(vera-tileset-pixel 0 0 17 0 +vera-color-grayscale-1+)
(vera-tileset-pixel 0 0 18 0 +vera-color-grayscale-2+)
(vera-tileset-pixel 0 0 19 0 +vera-color-grayscale-3+)
(vera-tileset-pixel 0 0 20 0 +vera-color-grayscale-4+)
(vera-tileset-pixel 0 0 21 0 +vera-color-grayscale-5+)
(vera-tileset-pixel 0 0 22 0 +vera-color-grayscale-6+)
(vera-tileset-pixel 0 0 23 0 +vera-color-grayscale-7+)
(vera-tileset-pixel 0 0 24 0 +vera-color-grayscale-8+)
(vera-tileset-pixel 0 0 25 0 +vera-color-grayscale-9+)
(vera-tileset-pixel 0 0 26 0 +vera-color-grayscale-10+)
(vera-tileset-pixel 0 0 27 0 +vera-color-grayscale-11+)
(vera-tileset-pixel 0 0 28 0 +vera-color-grayscale-12+)
(vera-tileset-pixel 0 0 29 0 +vera-color-grayscale-13+)
(vera-tileset-pixel 0 0 30 0 +vera-color-grayscale-14+)
(vera-tileset-pixel 0 0 31 0 +vera-color-grayscale-15+)

(vera :irqline 0)
(vera :linecapture :enable)
(loop (if (= (vera :linecapture :enabled) 0) (return)))
(dotimes (ii 32)
  (print (vera :linecapture :pixel :x ii)))



'end
