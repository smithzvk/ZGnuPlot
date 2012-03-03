
(in-package :zgnuplot)

;; @\section{Introduction}

;; @ZGnuplot is a library that aims to bring some polish to CL generated plots
;; using gnuplot.  Gnuplot is an interesting program in that it is simple to get
;; up and running and can also produce quite beautiful plots, but most users
;; never wade through the dense docs to find out how those plots can be
;; produced.  ZGnuplot will attempt to, at the very least, produce prettier
;; plots by default than gnuplot does out of the box.  It also attempts to
;; remove many of the inconsistencies that are in the gnuplot interface and to
;; remedy some of the limitations if possible.

;; @\section{Overview}

;;<<>>=
(defun determine-minimum-arity (fn &optional (ranges (tb:roll-list '((.1 1))))
                                             (maximum-arity-tried 10))
  (iter (for arity from 0 to maximum-arity-tried)
    (until (ignore-errors (apply fn (mapcar #'first (head ranges arity)))))
    (finally (if (>= arity maximum-arity-tried)
                 (error "Arity could not be determined")
                 (return arity)))))

;;<<>>=
(defun determine-arity (fn &optional (ranges (tb:roll-list '((.1 1))))
                                     (maximum-arity-tried 10))
  (let ((min nil))
    (iter (for arity from 0 to maximum-arity-tried)
      (let ((res (ignore-errors (apply fn (mapcar #'first (head ranges arity))))))
        (when (and (not min) res) (setf min arity))
        (until (and min (not res)))
        (finally (return (values min (if (>= arity maximum-arity-tried)
                                         nil
                                         (- arity 1)))))))))

;; @The class <<gnuplot-setup>> is the structure that allows you to control how
;; gnuplot performs the plot.

;; @The function <<plot>> performs all plotting.  It takes an object of type
;; <<gnuplot-setup>> as its first argument and any number of plottable objects
;; after it.  If the object passed as the first argument is <<nil>> then the
;; function tries to come up with something reasonable to do.

;;<<>>=
(defclass* gnuplot-setup ()
  ((title)
   (key)
   (key-position)
   (key-inset)
   (tics)
   (x-tics)
   (y-tics)
   (n-tics 100)
   (plot-type)
   (x-label) (y-label)
   (x-range) (y-range)
   (t-range)
   (u-range) (v-range)
   (autoscale)
   ;; styles for lines and points
   (line-styles)
   (point-size 1)))

;; @\section{2D Plotting}

;; @\subsection{Plot Representations}

;;<<>>=
(defclass* data-rep ()
  ((plot-style "points")
   (plot-type)
   (plot-smoothing)
   (rep-label)
   (x-data)
   (y-data)
   (error-bars)))

;;<<>>=
(defclass* func-rep ()
  ((func)
   (plot-style "lines")
   (plot-type)
   (rep-label)))

;;<<>>=
(defparameter *gnuplot-state* (make-instance 'gnuplot-setup :x-range '(0 1)))

;;<<>>=
(defmethod x-data-of ((f-rep func-rep))
  (destructuring-bind (low high) (x-range-of *gnuplot-state*)
    (iter (for x from low to high by (/ (- high low) (n-tics-of *gnuplot-state*)))
          (collect x))))

;;<<>>=
(defmethod y-data-of ((f-rep func-rep))
  (destructuring-bind (low high) (x-range-of *gnuplot-state*)
    (iter (for x from low to high by (/ (- high low) (n-tics-of *gnuplot-state*)))
          (collect (funcall (func-of f-rep) x)))))

;;<<>>=
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

;;<<>>=
(defun space-pad (string) (mkstr " " string " "))

;;<<>>=
(defun stringify-plot (file-name plot)
  (mkstr "'" (namestring file-name) "' "
         (if-set rep-label plot (space-pad (mkstr "title '" rep-label "'"))
             (space-pad "notitle"))
         (if-set error-bars plot
             (if-set plot-style plot
                 (cond ((equal "lines" plot-style)
                        (space-pad "with errorlines"))
                       ((equal "points" plot-style)
                        (space-pad "with errorbars")))
                 (space-pad "with errorbars"))
             (if-set plot-style plot (space-pad (mkstr "with " plot-style))))
         (if-set plot-type plot (space-pad (mkstr plot-type)))))

(defmethod stringify-plot ((plot data-rep) file-name)
  (with-open-file (out file-name :direction :output)
    (format-ext "~{~A ~A~%~}" (mapcar #'list
                                      (x-data-of data-rep)
                                      (y-data-of data-rep))))
  (mkstr "'" (namestring file-name) "' "
         (if-set rep-label plot (space-pad (mkstr "title '" rep-label "'"))
             (space-pad "notitle"))
         (if-set error-bars plot
             (if-set plot-style plot
                 (cond ((equal "lines" plot-style)
                        (space-pad "with errorlines"))
                       ((equal "points" plot-style)
                        (space-pad "with errorbars")))
                 (space-pad "with errorbars"))
             (if-set plot-style plot (space-pad (mkstr "with " plot-style))))
         (if-set plot-type plot (space-pad (mkstr plot-type)))))

;; @\subsection{Plotting Curves}

;;<<>>=
(defun %plot (state &rest plots)
  (let* ((*gnuplot-state* (or state *gnuplot-state* (error "No gnuplot state set.")))
         (st *gnuplot-state*))
    (unless plots
      (error "No plots given"))
    (iter (for plot in plots)
      (for x-data = (cond ((or (typep plot 'func-rep)
                               (slot-boundp plot 'x-data))
                           (x-data-of plot))
                          (t (iter (for y in (y-data-of plot))
                               (for i from 0)
                               (collect i)))))
      (for file-name =
        (pathname (osicat-posix:mktemp
                   (namestring osicat:*temporary-directory*))))
      (for temp-file = (open file-name :direction :output))
      (collecting file-name into file-names)
      (collecting temp-file into temp-files)
      (collecting (stringify-plot file-name plot)
                  into plot-strings)
      (if (and (typep plot 'data-rep)
               (slot-boundp plot 'error-bars))
          (iter (for x in x-data)
            (for y in (y-data-of plot))
            (for eb in (error-bars-of plot))
            (when (and x y eb) (format-ext temp-file "~A ~A ~A~%" x y eb)))
          (iter (for x in x-data)
            (for y in (y-data-of plot))
            (when (and x y) (format-ext temp-file "~A ~A~%" x y))))
      (finally (mapc #'close temp-files)
               (cgn:format-gnuplot
                (mkstr "set pointsize " (point-size-of st) ";"
                       (if-set autoscale st
                           (space-pad (mkstr "set autoscale;"))
                           (space-pad (mkstr "unset autoscale;")))
                       (if-set title st
                           (space-pad (mkstr "set title '" title "';"))
                           (space-pad (mkstr "set title;")))
                       (if-set x-label st
                           (space-pad (mkstr "set xlabel '" x-label "';"))
                           (space-pad (mkstr "unset xlabel;")))
                       (if-set y-label st
                           (space-pad (mkstr "set ylabel '" y-label "';"))
                           (space-pad (mkstr "unset ylabel;")))
                       (if-set x-range st
                           (format-ext nil "set xrange[~{~A~^:~}];"
                                       x-range)
                           (format-ext nil "unset xrange;"))
                       (if-set y-range st
                           (format-ext nil "set yrange[~{~A~^:~}];"
                                       y-range)
                           (format-ext nil "unset yrange;"))
                       "plot "
                       "~{~A~^, ~}")
                plot-strings)
               (return
                 (aif (t-ret
                        (< 0 (length
                              (:ret (mkstr
                                     (coerce
                                      (iter (for c = (read-char-no-hang cgn::*gnuplot*))
                                        (while c)
                                        (collect c)) 'string))))))
                      (error it)))))))

;;<<>>=
(defun infer-rep (obj)
  (cond ((or (functionp obj) (symbolp obj))
         (make-instance 'func-rep :func obj))
        ((and (consp obj) (consp (car obj)))
         (apply
          #'make-instance 'data-rep
          :x-data (mapcar #'first obj)
          :y-data (mapcar #'second obj)
          (if (> (length (first obj)) 2)
              (list :error-bars
                    (mapcar #'caddr
                            obj)
                    :plot-style "errorbars"))))
        ((consp obj)
         (make-instance 'data-rep :y-data obj))
        (t obj)))

;;<<>>=
(defun plot (state &rest plots)
  (if cgn::*gnuplot*
      (apply #'%plot state (mapcar 'infer-rep plots))
      (cgn:with-gnuplot (:linux)
        (apply #'%plot state (mapcar 'infer-rep plots)))))

;; @\subsection{Polar Plots}

;; Polar plots are a special type of 2D plot and of parameteric plot.  As with
;; gnuplot,this requires a separate interface.  However unlike gnuplot, the
;; interface is keeping with the <<plot>> and <<splot>> split.  This makes for a
;; more uniform interface.


;;<<>>=
(defun %polar-plot (state &rest plots)
  (let* ((*gnuplot-state* (or state *gnuplot-state* (error "No gnuplot state set.")))
         (st *gnuplot-state*))
    (unless plots
      (error "No plots given"))
    (iter (for plot in plots)
      (for x-data = (cond ((or (typep plot 'func-rep)
                               (slot-boundp plot 'x-data))
                           (x-data-of plot))
                          (t (iter (for y in (y-data-of plot))
                               (for i from 0)
                               (collect i)))))
      (for file-name =
        (pathname (osicat-posix:mktemp
                   (namestring osicat:*temporary-directory*))))
      (for temp-file = (open file-name :direction :output))
      (collecting file-name into file-names)
      (collecting temp-file into temp-files)
      (collecting (stringify-plot file-name plot)
                  into plot-strings)
      (if (and (typep plot 'data-rep)
               (slot-boundp plot 'error-bars))
          (iter (for x in x-data)
            (for y in (y-data-of plot))
            (for eb in (error-bars-of plot))
            (when (and x y eb) (format-ext temp-file "~A ~A ~A~%" x y eb)))
          (iter (for x in x-data)
            (for y in (y-data-of plot))
            (when (and x y) (format-ext temp-file "~A ~A~%" x y))))
      (finally (mapc #'close temp-files)
               (cgn:format-gnuplot
                (mkstr "set pointsize " (point-size-of st) ";"
                       (if-set autoscale st
                           (space-pad (mkstr "set autoscale;"))
                           (space-pad (mkstr "unset autoscale;")))
                       (if-set title st
                           (space-pad (mkstr "set title '" title "';"))
                           (space-pad (mkstr "set title;")))
                       (if-set x-label st
                           (space-pad (mkstr "set xlabel '" x-label "';"))
                           (space-pad (mkstr "unset xlabel;")))
                       (if-set y-label st
                           (space-pad (mkstr "set ylabel '" y-label "';"))
                           (space-pad (mkstr "unset ylabel;")))
                       (if-set x-range st
                           (format-ext nil "set xrange[~{~A~^:~}];"
                                       x-range)
                           (format-ext nil "unset xrange;"))
                       (if-set y-range st
                           (format-ext nil "set yrange[~{~A~^:~}];"
                                       y-range)
                           (format-ext nil "unset yrange;"))
                       "plot "
                       "~{~A~^, ~}")
                plot-strings)
               (return
                 (aif (t-ret
                        (< 0 (length
                              (:ret (mkstr
                                     (coerce
                                      (iter (for c = (read-char-no-hang cgn::*gnuplot*))
                                        (while c)
                                        (collect c)) 'string))))))
                      (error it)))))))

;;<<>>=
(defun infer-rep (obj)
  (cond ((or (functionp obj) (symbolp obj))
         (make-instance 'func-rep :func obj))
        ((and (consp obj) (consp (car obj)))
         (apply
          #'make-instance 'data-rep
          :x-data (mapcar #'first obj)
          :y-data (mapcar #'second obj)
          (if (> (length (first obj)) 2)
              (list :error-bars
                    (mapcar #'caddr
                            obj)
                    :plot-style "errorbars"))))
        ((consp obj)
         (make-instance 'data-rep :y-data obj))
        (t obj)))

;;<<>>=
(defun polar-plot (state &rest plots)
  (if cgn::*gnuplot*
      (apply #'%plot state (mapcar 'infer-rep plots))
      (cgn:with-gnuplot (:linux)
        (apply #'%plot state (mapcar 'infer-rep plots)))))

;; @\section{3D Plotting}

;; @\subsection{Plot Representations}

;;<<>>=
(defclass* surface-rep ()
  ((func)
   (plot-style "lines")
   (plot-type)
   (rep-label)))

;;<<>>=
(defmethod x-data-of ((f-rep surface-rep))
  (destructuring-bind (low high) (x-range-of *gnuplot-state*)
    (iter (for x from low to high by (/ (- high low) (n-tics-of *gnuplot-state*)))
          (collect x))))

;;<<>>=
(defmethod y-data-of ((f-rep surface-rep))
  (destructuring-bind (low high) (x-range-of *gnuplot-state*)
    (iter (for x from low to high by (/ (- high low) (n-tics-of *gnuplot-state*)))
          (collect (funcall (func-of f-rep) x)))))

;; @\section{Plotting Surfaces}

;;<<>>=
(defun %splot (state &rest plots)
  (let* ((*gnuplot-state* (or state *gnuplot-state* (error "No gnuplot state set.")))
         (st *gnuplot-state*))
    (unless plots
      (error "No plots given"))
    (iter (for plot in plots)
      (for x-data = (cond ((or (typep plot 'func-rep)
                               (slot-boundp plot 'x-data))
                           (x-data-of plot))
                          (t (iter (for y in (y-data-of plot))
                               (for i from 0)
                               (collect i)))))
      (for file-name =
        (pathname (osicat-posix:mktemp
                   (namestring osicat:*temporary-directory*))))
      (for temp-file = (open file-name :direction :output))
      (collecting file-name into file-names)
      (collecting temp-file into temp-files)
      (collecting (stringify-plot file-name plot)
                  into plot-strings)
      (if (and (typep plot 'data-rep)
               (slot-boundp plot 'error-bars))
          (iter (for x in x-data)
            (for y in (y-data-of plot))
            (for eb in (error-bars-of plot))
            (when (and x y eb) (format-ext temp-file "~A ~A ~A~%" x y eb)))
          (iter (for x in x-data)
            (for y in (y-data-of plot))
            (when (and x y) (format-ext temp-file "~A ~A~%" x y))))
      (finally (mapc #'close temp-files)
               (cgn:format-gnuplot
                (mkstr "set pointsize " (point-size-of st) ";"
                       (if-set autoscale st
                           (space-pad (mkstr "set autoscale;"))
                           (space-pad (mkstr "unset autoscale;")))
                       (if-set title st
                           (space-pad (mkstr "set title '" title "';"))
                           (space-pad (mkstr "set title;")))
                       (if-set x-label st
                           (space-pad (mkstr "set xlabel '" x-label "';"))
                           (space-pad (mkstr "unset xlabel;")))
                       (if-set y-label st
                           (space-pad (mkstr "set ylabel '" y-label "';"))
                           (space-pad (mkstr "unset ylabel;")))
                       (if-set x-range st
                           (format-ext nil "set xrange[~{~A~^:~}];"
                                       x-range)
                           (format-ext nil "unset xrange;"))
                       (if-set y-range st
                           (format-ext nil "set yrange[~{~A~^:~}];"
                                       y-range)
                           (format-ext nil "unset yrange;"))
                       "splot "
                       "~{~A~^, ~}")
                plot-strings)
               (return
                 (aif (t-ret
                        (< 0 (length
                              (:ret (mkstr
                                     (coerce
                                      (iter (for c = (read-char-no-hang cgn::*gnuplot*))
                                        (while c)
                                        (collect c)) 'string))))))
                      (error it)))))))

;;<<>>=
(defun splot (state &rest plots)
  (if cgn::*gnuplot*
      (apply #'%splot state plots)
      (cgn:with-gnuplot (:linux)
        (apply #'%splot state plots))))

;;<<>>=
(defun ensure-namestring (fname)
  (etypecase fname
    (pathname (namestring fname))
    (string fname)))

;; @\section{Utilities and gnuplot settings}

;;<<>>=
(defclass* terminal ()
  (linewidth
   canvas-size))

;;<<>>=
(defparameter *default-term* "x11")

;;<<>>=
(defun set-default-term (term)
  (setf *default-term* term))

;;<<>>=
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
                         "set size ~A;" size))
               "set term " term ";"
               "replot; set term " *default-term* ";"
               ;; replot to ensure that stupid terminals actually
               ;; finish output, don't know a better way...
               "replot"))))))

;; @\section{Similar Libraries}

;; @\begin{itemize}

;; These need to be reviewed as I don't actually remember all of the names of
;; these libraries.

;; \item Cgn
;; \item Clnuplot
;; \item cl-graph

;; \end{itemize}

