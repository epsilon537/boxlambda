'start
(vera :init)

(defun make-scanline-list ()
  (loop (if (< (vera-scanline) 10) (return)))
  (list (vera-scanline) (vera-scanline) (vera-scanline)))

(let ((scanline-list (make-scanline-list)))
  (if (and
        (> 40 (- (second scanline-list) (first scanline-list)))
        (> 40 (- (third scanline-list) (second scanline-list))))
    (print "OK")
    (format t "Fail: ~a~%" scanline-list)))

'end

