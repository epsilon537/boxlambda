'start
(vera :init)

(defun make_scanline_list ()
  (loop (if (< (vera_scanline) 10) (return)))
  (list (vera_scanline) (vera_scanline) (vera_scanline)))

(let ((scanline_list (make_scanline_list)))
  (if (and
        (> 40 (- (second scanline_list) (first scanline_list)))
        (> 40 (- (third scanline_list) (second scanline_list))))
    (print "OK")
    (format t "Fail: ~a~%" scanline_list)))

'end

