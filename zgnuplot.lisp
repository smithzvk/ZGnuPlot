
(in-package :zgnuplot)

;; @\section{Introduction}

;; @ZGnuplot is a library that aims to bring some polish to CL generated plots
;; using gnuplot.  Gnuplot is an interesting program in that it is simple to get
;; up and running and can also produce quite beautiful plots, but most users
;; never wade through the dense docs to find out how those plots can be
;; produced.  ZGnuplot will attempt to, at the very least, produce prettier
;; plots by default than gnuplot does out of the box.  It also attempts to
;; remove many of the inconsistencies that are in the gnuplot interface and to
;; remedy some of the limitations if possible, making for a simpler plotting
;; utility.

;; @\section{Setting up the plot}

;; @The class <<gnuplot-setup>> defines how a plot will be rendered.

(defmacro defclass** (name supers slots)
  `(progn
     (defclass* ,name ,supers ,(mapcar #'first slots))
     (defun ,(intern (concatenate 'string "MAKE-" (symbol-name name)))
         (&key ,@slots)
       (make-instance ',name ,@(apply
                                #'append
                                (mapcar
                                 (lambda (x)
                                   (let ((x (alexandria:ensure-list x)))
                                     (list
                                      (intern (symbol-name (first x)) :keyword)
                                      (first x))))
                                 slots))))))

;;<<>>=
(defclass** gnuplot-setup ()
  (;; Big picture options
   (plot-type :2d)
   ;; List all of the logscale coordinates
   (logscale nil)
   ;; Plot shape
   (size 1)
   ;; Controlling the key
   (key nil)
   (key-position :top-right)
   (key-verticle t)
   (key-inset t)
   ;; Controlling the tic-marks
   (border t)
   (tics t)
   (x-tics t)
   (y-tics t)
   (z-tics t)
   (r-tics t)
   (theta-tics t)
   (grid (cond ((eql plot-type :polar) :polar)
               (t t)))
   ;; When plotting functions, how many samples do we take?
   (n-samples 100)
   (adaptive-sampling nil)
   ;; How is the plot labelled?
   (title nil)
   (x-label nil) (y-label nil) (z-label nil)
   ;; Variable ranges
   (x-range '(0 1))
   (y-range '(0 1))
   (z-range '(0 1))
   (theta-range (list (- pi) pi))
   (u-range '(0 1)) (v-range '(0 1))
   ;; We don't use autoscale as it screws up when we plot functions
   (autoscale (cond ;; ((eql :2d plot-type) '(:y))
                    ((eql :polar plot-type) '(:r))
                    ;; ((eql :3d plot-type) '(:z))
                    ))
   ;; styles for lines and points
   (styles *muted-colors*)
   (line-width 1.5)
   (point-size 1.5)))

(defparameter *black-and-white*
  '(("'#000000'" 1)
    ("'#000000'" 2)
    ("'#000000'" 3)
    ("'#000000'" 4)
    ("'#000000'" 5)
    ("'#000000'" 6)))

(defparameter *muted-colors*
  '(("'#3355CC'" 1)
    ("'#20BB20'" 1)
    ("'#DD4040'" 1)
    ("'#20CC99'" 1)
    ("'#AA44CC'" 1)
    ("'#E5E510'" 1)))

(defparameter *muted-roygbiv*
  '(("'#DD4040'" 1)
    ("'#E5E510'" 1)
    ("'#20BB20'" 1)
    ("'#3355CC'" 1)
    ("'#AA44CC'" 1)))

(defvar *style* 0)

(defparameter *symbols*
  '(4 6 8 10 12 3 5 7 9 11 13))

(defun keyword-to-string (keyword)
  (let ((*print-case* :downcase))
    (format nil "~A" keyword)))

(defun setup-gnuplot (setup)
  "The job of this function is to create a string that holds all of the state
for the gnuplot plot.  This means makes some intelligent defaults.  Some things
are left to options in the individual plot objects."
  (with-output-to-string (out)
    (iter
      (for (color line-type) in (tb:roll-list (styles-of setup)))
      (for symbol in (tb:roll-list *symbols*))
      (for index from 1 to 14)
      (format-ext
       out
       "set style line ~A linecolor rgb ~A linetype ~A linewidth ~A ~
        pointtype ~A pointsize ~A;~%"
       index color line-type (line-width-of setup)
       symbol (point-size-of setup)))
    (if (size-of setup)
        (format-ext out "set size ~A;" (size-of setup))
        (format-ext out "set size nosquare;"))
    ;; If logscale is set, use that value.  If it is just T, let gnuplot do what
    ;; it thinks it should do.
    (if (logscale-of setup)
        (let ((logscale (if (eql t (logscale-of setup))
                            (alexandria:ensure-list (logscale-of setup))
                            (logscale-of setup))))
          (if (consp logscale)
              (iter (for coordinate in logscale)
                (format-ext out "set logscale ~A;" (keyword-to-string coordinate)))
              (format-ext out "set logscale;")))
        (format-ext out "unset logscale;"))
    ;; Set ranges.  These should always have default values unless you
    ;; explicitly set them to NIL, in which case the old values will be used (or
    ;; something else less specified.
    (if (x-range-of setup)
        (format-ext out "set xrange[~{~A~^:~}];" (x-range-of setup)))
    (if (y-range-of setup)
        (format-ext out "set yrange[~{~A~^:~}];" (y-range-of setup)))
    (if (z-range-of setup)
        (format-ext out "set zrange[~{~A~^:~}];" (z-range-of setup)))
    ;; If autoscale is explicitly set, use that value.  If it is just T, set it
    ;; to a `smart' value: (:x :y) for polar, (:y) for :2D, and (:z) for :3D.
    (if (autoscale-of setup)
        (let ((autoscale (if (eql t (autoscale-of setup))
                             (cond ((eql :polar (plot-type-of setup))
                                    (list :x :y))
                                   ((eql :2D (plot-type-of setup))
                                    (list :y))
                                   ((eql :3D (plot-type-of setup))
                                    (list :z)))
                             (alexandria:ensure-list (autoscale-of setup)))))
          (iter (for axis in autoscale)
            (format-ext out "set autoscale ~A;" (keyword-to-string axis))))
        (format-ext out "unset autoscale;"))
    (when (key-of setup)
      (format-ext out "set key ~A ~A ~A;"
                  (if (key-inset-of setup) "inset" "out")
                  (if (key-verticle-of setup) "vert" "horiz")
                  (case (key-position-of setup)
                    (:top-left "top left")
                    (:top-center "top center")
                    (:top-right "top right")
                    (:center-left "center left")
                    (:center-right "center right")
                    (:bottom-left "bottom left")
                    (:bottom-center "bottom center")
                    (:bottom-right "bottom right"))))
    ;; Labels
    (if (title-of setup)
        (format-ext out "set title '~A';" (title-of setup))
        (format-ext out "unset title;"))
    (if (x-label-of setup)
        (format-ext out "set xlabel '~A';" (x-label-of setup))
        (format-ext out "unset xlabel;"))
    (if (y-label-of setup)
        (format-ext out "set ylabel '~A';" (y-label-of setup))
        (format-ext out "unset ylabel;"))
    (if (z-label-of setup)
        (format-ext out "set zlabel '~A';" (z-label-of setup))
        (format-ext out "unset zlabel;"))
    ;; Tics
    (if (x-tics-of setup)
        (format-ext out "set xtics;")
        (format-ext out "unset xtics;"))
    (if (y-tics-of setup)
        (format-ext out "set ytics;")
        (format-ext out "unset ytics;"))
    (if (z-tics-of setup)
        (format-ext out "set ztics;")
        (format-ext out "unset ztics;"))
    ;; (if (r-tics-of setup)
    ;;     (format-ext out "set rtics;")
    ;;     (format-ext out "unset rtics;"))
    ;; (if (theta-tics-of setup)
    ;;     (format-ext out "set ttics;")
    ;;     (format-ext out "unset ttics;"))
    (if (grid-of setup)
        (if (eql (grid-of setup) :polar)
            (format-ext out "set grid polar;")
            (format-ext out "set grid nopolar;"))
        (format-ext out "unset grid;"))
    (case (plot-type-of setup)
      (:polar (format-ext out "set polar;"))
      (:parametric (format-ext out "set parametric;"))
      (otherwise (format-ext out "unset parametric; unset polar;")))))

;; @\section{2D Plotting}

;; @\subsection{Plot Representations}

;;<<>>=
(defclass* data-rep ()
  ((plot-style "points")
   (plot-type nil)
   (plot-smoothing nil)
   (rep-label nil)
   (x-data nil)
   (y-data)
   (error-bars nil)))

;;<<>>=
(defclass* func-rep ()
  ((func)
   (plot-style "lines")
   (plot-type nil)
   (rep-label nil)
   (error-bars nil)))

;;<<>>=
(defparameter *gnuplot-setup* (make-instance 'gnuplot-setup :x-range '(0 1)))

;;<<>>=
(defun space-pad (string) (mkstr " " string " "))

;; @The <<stringify-plot>> function is a method that takes an object and a
;; filename and writes the plotting data to that filename.  If the user wishes to 

;; Data can be passed in list or array form to plot.  The data structure should
;; be of the proper dimensionality for the plot and the first $N$ values of each
;; tuple should be the independent variable values.  For instance...

;; Common organizations of data can be converted to this form using the
;; ... functions.

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
(defun interprete-error-bars (y-data error-bars)
  (declare (ignore y-data))
  (mapcar #'alexandria:ensure-list
          (alexandria:ensure-list
           error-bars)))
  ;; (cond ((atom error-bars) (

;;<<>>=
(defmethod stringify-plot ((plot cons) setup file-name)
  (infer-rep plot))

;;<<>>=
(defmethod stringify-plot ((plot function) setup file-name)
  (infer-rep plot))

;;<<>>=
(defmethod stringify-plot ((plot data-rep) setup file-name)
  (with-open-file (out file-name :direction :output :if-exists :supersede)
    (format-ext out "~{~{~A ~}~%~}"
                (apply
                 #'mapcar #'list
                 (remove nil
                         (list* (x-data-of plot)
                                (y-data-of plot)
                                (interprete-error-bars (y-data-of plot)
                                                       (error-bars-of plot)))))))
  (apply
   #'mkstr
   (remove
    nil
    (list "'" (namestring file-name) "' "
          (if (rep-label-of plot)
              (space-pad (mkstr "title '" (rep-label-of plot) "'"))
              (space-pad "notitle"))
          (if (error-bars-of plot)
              (if (plot-style-of plot)
                  (cond ((equal "lines" (plot-style-of plot))
                         (space-pad "with errorlines"))
                        ((equal "points" (plot-style-of plot))
                         (space-pad "with errorbars")))
                  (space-pad "with errorbars"))
              (if (plot-style-of plot)
                  (space-pad (mkstr "with " (plot-style-of plot)
                                    " linestyle " (incf *style*)))))
          (if (plot-type-of plot)
              (space-pad (mkstr (plot-type-of plot))))))))

;;<<>>=
(defvar *ignore-errors* t)

;;<<>>=
(defmethod stringify-plot ((plot func-rep) setup file-name)
  (with-open-file (out file-name :direction :output :if-exists :supersede)
    (let ((range-vals (if (eql :polar (plot-type-of setup))
                          (theta-range-of setup)
                          (x-range-of setup))))
      (cond ((error-bars-of plot)
             (warn "No error lines yet.")
             (let ((range (- (second range-vals)
                             (first range-vals))))
               (iter (for x
                       from (first range-vals)
                       to (+ (* 1/2 range (/ (n-samples-of setup)))
                             (second range-vals))
                       by (/ range (n-samples-of setup)))
                 (let ((val (if *ignore-errors*
                                (ignore-errors
                                 (multiple-value-list (funcall (func-of plot) x)))
                                (multiple-value-list (funcall (func-of plot) x)))))
                   (if val
                       (format-ext out "~{~A ~}~%" (cons x val))
                       (format-ext out "~%"))))))
            (t (let ((range (- (second range-vals)
                               (first range-vals))))
                 (iter (for x
                         from (first range-vals)
                         to (+ (* 1/2 range (/ (n-samples-of setup)))
                               (second range-vals))
                         by (/ range (n-samples-of setup)))
                   (let ((val (if *ignore-errors*
                                  (ignore-errors (funcall (func-of plot) x))
                                  (funcall (func-of plot) x))))
                     (if val
                         (format-ext out "~A ~A~%" x val)
                         (format-ext out "~%")))))))))
  (apply
   #'mkstr
   (remove
    nil
    (list "'" (namestring file-name) "' "
          (if (rep-label-of plot)
              (space-pad (mkstr "title '" (rep-label-of plot) "'"))
              (space-pad "notitle"))
          (if (error-bars-of plot)
              (if (plot-style-of plot)
                  (cond ((equal "lines" (plot-style-of plot))
                         (space-pad "with errorlines"))
                        ((equal "points" (plot-style-of plot))
                         (space-pad "with errorbars")))
                  (space-pad "with errorbars"))
              (if (plot-style-of plot)
                  (space-pad (mkstr "with " (plot-style-of plot)
                                    " linestyle " (incf *style*)))))
          (if (plot-type-of plot)
              (space-pad (mkstr (plot-type-of plot))))))))

;; @\subsection{Plotting Curves}

;;<<>>=
(defun %plot (setup &rest plots)
  (let* ((*gnuplot-setup* (or setup *gnuplot-setup* (error "No gnuplot state set.")))
         (st *gnuplot-setup*)
         (*style* 0))
    (unless plots
      (error "No plots given"))
    (iter (for plot in plots)
      (for file-name =
        (pathname (osicat-posix:mktemp
                   (namestring osicat:*temporary-directory*))))
      (collecting (stringify-plot plot st file-name)
                  into plot-strings)
      (finally (send-gnuplot (setup-gnuplot st))
               (ecase (plot-type-of setup)
                 (:3D (send-gnuplot "splot ~{~A~^, ~};" plot-strings))
                 ((:polar :2D) (send-gnuplot "plot ~{~A~^, ~};" plot-strings)))))
    (send-gnuplot "replot;")))

;;<<>>=
(defun plot (state &rest plots)
  (if *gnuplot-stream*
      (apply #'%plot state (mapcar 'infer-rep plots))
      (cgn:with-gnuplot (:linux)
        (apply #'%plot state (mapcar 'infer-rep plots)))))

;; @\subsection{Polar Plots}

;; Polar plots are a special type of 2D plot and of parameteric plot.  As with
;; gnuplot,this requires a separate interface.  However unlike gnuplot, the
;; interface is keeping with the <<plot>> and <<splot>> split.  This makes for a
;; more uniform interface.

;; @\section{3D Plotting}

;; @\subsection{Plot Representations}

;; @\section{Plotting Surfaces}

;;<<>>=
(defun ensure-namestring (fname)
  (etypecase fname
    (pathname (namestring fname))
    (string fname)))

;; @\section{Saving Plots}

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

