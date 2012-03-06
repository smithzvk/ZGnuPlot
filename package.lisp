
(defpackage :zgnuplot
    (:use :cl :toolbox :defclass* :iter)
  (:nicknames :zgp)
  (:export #:gnuplot-setup
           #:make-gnuplot-setup
           #:data-rep #:func-rep #:save-plot
           #:splot
           #:plot
           #:*gnuplot-setup*
           #:with-gnuplot))

