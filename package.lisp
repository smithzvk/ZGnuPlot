
(defpackage :zgnuplot
    (:use :cl :hu.dwim.defclass-star :iter :modf :ima :ppcre)
  (:nicknames :zgp)
  (:export #:gnuplot-setup
           #:make-gnuplot-setup
           #:data-rep #:func-rep #:save-plot
           #:splot
           #:plot
           #:*gnuplot-setup*
           #:with-gnuplot
           #:start-gnuplot
           #:close-gnuplot
           #:send-gnuplot))

