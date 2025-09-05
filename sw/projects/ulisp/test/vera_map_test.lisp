'start
(vera :init)

(dotimes (jj 2)
  (let ((idx 0))
    (dolist (wh (list (cons 32 32) (cons 64 64) (cons 128 128)))
      (dolist (maptype (list 0 1 2))
        (vera :map idx :width (car wh) :height (cdr wh) :map_type maptype)
        (print (vera :map idx :info))
        (setq idx (1+ idx)))))
  (dotimes (ii 9)
    (vera :map ii :deinit)))
'end

