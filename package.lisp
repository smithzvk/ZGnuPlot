
(defpackage :zgnuplot
    (:use :cl :toolbox :defclass* :iter)
  (:nicknames :zgp)
  (:export #:gnuplot #:gnuplot-setup
           #:data-rep #:func-rep #:save-plot))

