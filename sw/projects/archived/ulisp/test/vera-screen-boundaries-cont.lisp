'start
(vera :display :enable)
(vera :display :enabled)

(vera :bordercolor +vera-color-blue+)
(vera :bordercolor)

(vera :boundaries :hstart 100 :hstop 540 :vstart 100 :vstop 380)
(vera :boundaries)

(vera :irqline 379)
(vera :linecapture :enable)
(loop (if (= (vera :linecapture :enabled) 0) (return)))
(print (vera :linecapture :pixel :x 99))
(print (vera :linecapture :pixel :x 100))
(print (vera :linecapture :pixel :x 539))
(print (vera :linecapture :pixel :x 540))


(vera :irqline 380)
(vera :linecapture :enable)
(loop (if (= (vera :linecapture :enabled) 0) (return)))
(print (vera :linecapture :pixel :x 100))
(print (vera :linecapture :pixel :x 539))

(vera :display :disable)
(vera :display :enabled)

'end


