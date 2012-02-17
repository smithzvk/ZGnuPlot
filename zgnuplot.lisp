
(in-package :zgnuplot)

(defclass* data-rep ()
  ((plot-style "points")
   (plot-type)
   (plot-smoothing)
   (rep-label)
   (x-data)
   (y-data)
   (error-bars) ))

(defclass* func-rep ()
  ((func)
   (plot-style "lines")
   (plot-type)
   (rep-label) ))

(defclass* surface-rep ()
  ((func)
   (plot-style "lines")
   (plot-type)
   (rep-label) ))

(defclass* gnuplot-setup ()
  ((title)
   (key)
   (tics)
   (n-tics 100)
   (x-label) (y-label)
   (x-range) (y-range)
   (point-size 1) ))

(defparameter *gnuplot-state* (make-instance 'gnuplot-setup :x-range '(0 1)))

(defmethod x-data-of ((f-rep func-rep))
  (destructuring-bind (low high) (x-range-of *gnuplot-state*)
    (iter (for x from low to high by (/ (- high low) (n-tics-of *gnuplot-state*)))
          (collect x) )))

(defmethod y-data-of ((f-rep func-rep))
  (destructuring-bind (low high) (x-range-of *gnuplot-state*)
    (iter (for x from low to high by (/ (- high low) (n-tics-of *gnuplot-state*)))
          (collect (funcall (func-of f-rep) x)) )))

(defmethod x-data-of ((f-rep surface-rep))
  (destructuring-bind (low high) (x-range-of *gnuplot-state*)
    (iter (for x from low to high by (/ (- high low) (n-tics-of *gnuplot-state*)))
          (collect x) )))

(defmethod y-data-of ((f-rep surface-rep))
  (destructuring-bind (low high) (x-range-of *gnuplot-state*)
    (iter (for x from low to high by (/ (- high low) (n-tics-of *gnuplot-state*)))
          (collect (funcall (func-of f-rep) x)) )))

(defmacro if-set (slot obj on-bound &optional (on-not-bound ""))
  "This needs to be a macro since I must walk the syntax try to
replace instances of SLOT with the proper slot accessor.  In this case
I am using WITH-SLOTS to do my walking for me, but the outcome is the
same."
  (let ((obj-sym (gensym)))
    `(let ((,obj-sym ,obj))
       (if (and (slot-exists-p ,obj-sym ',slot)
                (slot-boundp ,obj ',slot))
           (with-slots (,slot) ,obj-sym
             ,on-bound)
           ,on-not-bound))))

(defun space-pad (string) (mkstr " " string " "))

(defun stringify-plot (file-name plot)
  (mkstr "'" (namestring file-name) "' "
         (if-set rep-label plot (space-pad (mkstr "title '" rep-label "'"))
                 (space-pad "notitle") )
         (if-set plot-style plot (space-pad (mkstr "with " plot-style)))
         (if-set plot-type plot (space-pad (mkstr plot-type))) )))

(defun %gnuplot (state &rest plots)
  (let* ((*gnuplot-state* (or state *gnuplot-state* (error "No gnuplot state set.")))
         (st *gnuplot-state*) )
    (iter (for plot in plots)
      (for file-name =
        (pathname (osicat-posix:mktemp
                   (namestring osicat:*temporary-directory*) )))
      (for temp-file = (open file-name :direction :output))
      (collecting file-name into file-names)
      (collecting temp-file into temp-files)
      (collecting (stringify-plot file-name plot)
                  into plot-strings )
      (if (equal (plot-style-of plot) "errorbars")
          (iter (for x in (x-data-of plot))
            (for y in (y-data-of plot))
            (for eb in (error-bars-of plot))
            (when (and x y) (format-ext temp-file "~A ~A ~A~%" x y eb)) )
          (iter (for x in (x-data-of plot))
            (for y in (y-data-of plot))
            (when (and x y) (format-ext temp-file "~A ~A~%" x y)) ))
      (finally (mapc #'close temp-files)
               (cgn:format-gnuplot
                (mkstr "set pointsize " (point-size-of st) ";"
                       (if-set title st
                           (space-pad (mkstr "set title '" title "';")) )
                       (if-set x-label st
                           (space-pad (mkstr "set xlabel '" x-label "';")) )
                       (if-set y-label st
                           (space-pad (mkstr "set ylabel '" y-label "';")) )
                       (if-set x-range st
                           (format-ext nil "set xrange[~{~A~^:~}];"
                                       x-range ))
                       (if-set y-range st
                           (format-ext nil "set yrange[~{~A~^:~}];"
                                       y-range ))
                       "plot "
                       "~{~A~^, ~}" )
                plot-strings )
               (return
                 (aif (t-ret
                        (< 0 (length
                              (:ret (mkstr
                                     (coerce
                                      (iter (for c = (read-char-no-hang cgn::*gnuplot*))
                                        (while c)
                                        (collect c) ) 'string ))))))
                      (error it) ))))))

(defun gnuplot (state &rest plots)
  (if cgn::*gnuplot*
      (apply #'%gnuplot state plots)
      (cgn:with-gnuplot (:linux)
        (apply #'%gnuplot state plots) )))

(defun ensure-namestring (fname)
  (etypecase fname
    (pathname (namestring fname))
    (string fname) ))

(defclass* terminal ()
  (linewidth
   canvas-size ))

(defparameter *default-term* "x11")

(defun set-default-term (term)
  (setf *default-term* term))

(defun save-plot (fname &key (term "svg") size)
  (let ((fname (ensure-namestring fname)))
    (cgn:format-gnuplot
     (apply
      #'mkstr
      (remove nil
              (list
               "set out '" fname "';"
               (when size
                 (format nil
                         "set size ~A;" size ))
               "set term " term ";"
               "replot; set term " *default-term* ";"
               ;; replot to ensure that stupid terminals actually
               ;; finish output, don't know a better way...
               "replot" ))))))
