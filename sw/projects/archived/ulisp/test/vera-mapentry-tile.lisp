'start

(vera :display :enable)
(vera :tileset 0 :width 16 :height 16 :bpp 8 :num-tiles 32)

(dotimes (ii 16)
  (dotimes (jj (1+ ii))
    (vera-tileset-pixel 0 1 jj ii +vera-color-blue+)))









(vera :map 0 :width 32 :height 32 :map-type +vera-map-type-tile+)
(vera :layer 0 :map 0)
(vera :layer 0 :tileset 0)

(defvar row1 10)
(defvar col1 20)
(defvar row2 20)
(defvar col2 10)
(defvar row3 16)
(defvar col3 16)

(vera :map 0 :entry :x col1 :y row1 :val 1)
(vera :map 0 :entry :x col2 :y row2 :val (logior +vera-mapentry-hflip-mask+ +vera-mapentry-vflip-mask+ 1))
(vera :map 0 :entry :x col3 :y row3 :val (logior (ash 1 +vera-mapentry-pal-offset-shift+) 1))
(vera :layer 0 :enable)
(dolist (rowcol (list (cons row1 col1) (cons row2 col2) (cons row3 col3)))
  (vera :irqline (* 16 (car rowcol)))
  (vera :linecapture :enable)
  (loop (if (= (vera :linecapture :enabled) 0) (return)))
  (dotimes (ii 16)
    (print (vera :linecapture :pixel :x (+ (* 16 (cdr rowcol)) ii)))))





'end
