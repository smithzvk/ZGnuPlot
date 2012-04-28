
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
;; utility which is also extremely well suited for control from a Common Lisp
;; system.

;; If we are honest with ourselves, we should note that Gnuplot is not a very
;; straight forward program to use.  It has all of the cruft and archaic feel
;; of, say LaTeX, and seemingly no consistency at any level.  Gnuplot has the
;; feel of a program that has accreted over many years to become a capable yet
;; extremely inconsistent program.  So, in many ways, it is a fools errand to
;; attempt to smooth out the Gnuplot interface via a wrapper.  Indeed, I feel
;; that there is much to gain by using a solution such as Tamas Papp's plot
;; library.  That said, Gnuplot has a very large install base and many terminals
;; (output modules) which make it useful none-the-less.  The goal here is to
;; lessen pain until something unequivocally better comes along.

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
   ;; This locks the axis to have equal metrics
   (view-metric-equivalence (eql :polar plot-type))
   ;; Plot size and shape
   (size 1)
   (aspect-ratio (if (or view-metric-equivalence
                         (eql :polar plot-type))
                     1
                     (/ 3 4d0)))
   ;; Controlling the key
   (key nil)
   (key-position :top-right)
   (key-vertical t)
   (key-inset t)
   ;; Controlling the tic-marks
   (border t)
   (tics t)
   (x-tics t) (x2-tics nil)
   (y-tics t) (y2-tics nil)
   (z-tics t) (r-tics t) (theta-tics t)
   (grid nil)
   ;; When plotting functions, how many samples do we take?
   (n-samples 100)
   (adaptive-sampling nil)
   ;; How is the plot labelled?
   (title nil)
   (x-label nil) (y-label nil) (z-label nil)
   (x2-label nil) (y2-label nil)
   ;; Variable ranges
   (x-range '(-1 1))
   (x2-range '(-1 1))
   (y2-range '(-1 1))
   (y-range '(-1 1))
   (cb-range '(-1 1))
   (z-range '(-1 1))
   (theta-range (list (- pi) pi))
   (u-range '(0 1)) (v-range '(0 1))
   ;; We don't use autoscale as it screws up when we plot functions
   (autoscale nil)
   (palette '(rgbformula 33 13 10))
   (coloring-method :mean)
   ;; styles for lines and points
   (styles *muted-colors*)
   (line-width 1.5)
   (point-size 1.5)))

(defvar *style* 0)

(defparameter *symbols*
  '(4 6 8 10 12 3 5 7 9 11 13))

(defun keyword-to-string (keyword)
  (let ((*print-case* :downcase))
    (format nil "~A" keyword)))

;; @The <<setup-gnuplot>> function holds all almost all of the plotting smarts.
;; It is a mess, mostly due to the fact that the gnuplot interface is a mess.
;; Good luck to those who edit it.

;;<<>>=
(defun setup-gnuplot (setup)
  "The job of this function is to create a string that holds all of the state
for the gnuplot plot.  This means makes some intelligent defaults.  Some things
are left to options in the individual plot objects."
  (with-output-to-string (out)
    (format-ext out "set term ~A;~%" *default-term*)
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

    ;; Better colors for pm3d
    (format-ext out "set palette rgbformula ~{~A~^,~};"
                (rest (palette-of setup)))

    ;; Determine how colors are determined from the z coordinate in a 3D graph
    ;; or map.
    (ecase (coloring-method-of setup)
      (:interpolate
       ;; This will interpolate your 3d plot in some optimal sense.  This makes it
       ;; quite slow, though.
       (format-ext out "set pm3d interpolate 0,0;"))
      ((:mean :geomean :median :min :max :c1 :c2 :c3 :c4)
       (format-ext out "set pm3d corners2color ~A;"
                   (keyword-to-string (coloring-method-of setup)))))

    ;; Maps
    (if (eql :map (plot-type-of setup))
        (format-ext out "set pm3d map;"))

    ;; If logscale is set, use that value.  If it is just T, let gnuplot do what
    ;; it thinks it should do.
    (format-ext out "unset logscale;")
    (if (logscale-of setup)
        (let ((logscale (if (and (logscale-of setup)
                                 (not (eql t (logscale-of setup))))
                            (alexandria:ensure-list (logscale-of setup))
                            (logscale-of setup))))
          (if (consp logscale)
              (iter (for coordinate in logscale)
                (format-ext out "set logscale ~A;" (keyword-to-string coordinate)))
              (format-ext out "set logscale;"))))

    ;; Set ranges.  These should always have default values unless you
    ;; explicitly set them to NIL, in which case the old values will be used (or
    ;; something else less specified.
    (if (x-range-of setup)
        (format-ext out "set xrange[~{~A~^:~}];" (x-range-of setup)))
    (if (y-range-of setup)
        (format-ext out "set yrange[~{~A~^:~}];" (y-range-of setup)))
    (if (x2-range-of setup)
        (format-ext out "set x2range[~{~A~^:~}];" (x2-range-of setup)))
    (if (y2-range-of setup)
        (format-ext out "set y2range[~{~A~^:~}];" (y2-range-of setup)))
    (if (z-range-of setup)
        (format-ext out "set zrange[~{~A~^:~}];" (z-range-of setup)))
    (if (cb-range-of setup)
        (format-ext out "set cbrange[~{~A~^:~}];" (cb-range-of setup)))

    ;; If autoscale is explicitly set, use that value.  If it is just T, set it
    ;; to a `smart' value: (:x :y) for polar, (:y) for :2D, and (:z) for :3D.
    (format-ext out "unset autoscale;")
    (if (autoscale-of setup)
        (let ((autoscale (if (eql t (autoscale-of setup))
                             (cond ((eql :polar (plot-type-of setup))
                                    (list :x :y))
                                   ((eql :2D (plot-type-of setup))
                                    (list :y))
                                   ((eql :3D (plot-type-of setup))
                                    (list :z))
                                   (t (format-ext out "set autoscale;")
                                      nil))
                             (alexandria:ensure-list (autoscale-of setup)))))
          (iter (for axis in autoscale)
            (format-ext out "set autoscale ~A;" (keyword-to-string axis)))))

    ;; Size and shape
    (format-ext out "unset size;")
    (format-ext out "set size ~A;" (size-of setup))
    ;; Aspect ratio
    (format-ext out "set size noratio;")
    (format-ext out "set size ratio ~A~,3F;"
                (if (view-metric-equivalence-of setup)
                    "-" "")
                (aspect-ratio-of setup))

    (format-ext out "unset key;")
    (if (key-of setup)
        (format-ext out "set key ~A ~A ~A;"
                    (if (key-inset-of setup) "ins" "out")
                    (if (key-vertical-of setup) "vert" "horiz")
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
    (format-ext out "unset title;")
    (if (title-of setup)
        (format-ext out "set title '~A';" (title-of setup)))
    (format-ext out "unset xlabel;")
    (if (x-label-of setup)
        (format-ext out "set xlabel '~A';" (x-label-of setup)))
    (format-ext out "unset ylabel;")
    (if (y-label-of setup)
        (format-ext out "set ylabel '~A';" (y-label-of setup)))
    (format-ext out "unset x2label;")
    (if (x2-label-of setup)
        (format-ext out "set x2label '~A';" (x2-label-of setup)))
    (format-ext out "unset y2label;")
    (if (y2-label-of setup)
        (format-ext out "set y2label '~A';" (y2-label-of setup)))
    (format-ext out "unset zlabel;")
    (if (z-label-of setup)
        (format-ext out "set zlabel '~A';" (z-label-of setup)))

    ;; Tics
    (format-ext out "unset tics;")
    (format-ext out "unset xtics;")
    (when (x-tics-of setup)
      (format-ext out "set xtics ~A" (if (x2-tics-of setup)
                                         "nomirror"
                                         ""))
      (when (consp (x-tics-of setup))
        (format-ext out "(~:{~S ~A~:^, ~})" (x-tics-of setup)))
      (format-ext out ";"))
    (format-ext out "unset ytics;")
    (when (y-tics-of setup)
      (format-ext out "set ytics ~A" (if (y2-tics-of setup)
                                         "nomirror"
                                         ""))
      (when (consp (y-tics-of setup))
        (format-ext out "(~:{~S ~A~:^, ~})" (y-tics-of setup)))
      (format-ext out ";"))
    (format-ext out "unset x2tics;")
    (when (x2-tics-of setup)
      (format-ext out "set x2tics")
      (when (consp (x2-tics-of setup))
        (format-ext out "(~:{~S ~A~:^, ~})" (x2-tics-of setup)))
      (format-ext out ";"))
    (format-ext out "unset y2tics;")
    (when (y2-tics-of setup)
      (format-ext out "set y2tics")
      (when (consp (y2-tics-of setup))
        (format-ext out "(~:{~S ~A~:^, ~})" (y2-tics-of setup)))
      (format-ext out ";"))
    (format-ext out "unset ztics;")
    (when (z-tics-of setup)
      (format-ext out "set ztics")
      (when (consp (z-tics-of setup))
        (format-ext out "(~:{~S ~A~:^, ~})" (z-tics-of setup)))
      (format-ext out ";"))

    ;; If grid is set to T, use whatever gnuplot thinks is best unless plot-type
    ;; is polar, in which case we use a polar grid.  If the grid is set to a
    ;; something non-nil then make a grid on those axes.
    (format-ext out "set grid nopolar;") ; We need both to reset the grid
    (format-ext out "unset grid;")
    (cond ((and (eql t (grid-of setup)) (eql :polar (plot-type-of setup)))
           (format-ext out "set grid polar;"))
          ((eql t (grid-of setup))
           (format-ext out "set grid;"))
          ((grid-of setup)
           (iter (for grid-option in (alexandria:ensure-list (grid-of setup)))
             (format-ext out "set grid ~Atics;" (keyword-to-string grid-option))))
          (t (format-ext out "unset grid;")))

    ;; Set the final gnuplot plotting style...
    (case (plot-type-of setup)
      (:polar (format-ext out "set polar;"))
      (:parametric (format-ext out "set parametric;"))
      (otherwise (format-ext out "unset parametric; unset polar;")))))

;; @\section{2D Plotting}

;; @\subsection{Plot Representations}

;;<<>>=
(defclass* rep ()
  ((plot-style)
   (rep-label nil)
   (error-bars nil)
   (smoothing-method :cspline)))

;;<<>>=
(defclass* data-rep (rep)
  ((plot-style :points)
   (rep-label nil)
   (x-data nil)
   (y-data)
   (smoothing-method :cspline)))

;;<<>>=
(defclass* func-rep (rep)
  ((func)
   (plot-style :lines)
   (rep-label nil)
   (smoothing-method nil)
   (n-samples nil)
   (adaptive-sampling nil)))

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

(defun plot-style-to-string (style)
  (if (stringp style)
      style
      (case style
        (:lines-and-points "lp")
        (:points "points")
        (:lines "lines")
        (:error-bars "errorbars")
        (:smooth-lines-and-points
         (error "This does not directly correspond to a gnuplot plot style")))))

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
  (if (eql :smooth-lines-and-points (plot-style-of plot))
      ;; We want to draw a smooth line through these points
      (list
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
                       (cond ((eql :lines (plot-style-of plot))
                              (space-pad "with errorlines"))
                             ((eql :points (plot-style-of plot))
                              (space-pad "with errorbars")))
                       (space-pad "with errorbars"))
                   (if (plot-style-of plot)
                       (space-pad (mkstr "with points linestyle " (incf *style*))))))))
       (apply
        #'mkstr
        (remove
         nil
         (list "'" (namestring file-name) "' "
               (space-pad "notitle")
               " smooth " (keyword-to-string (smoothing-method-of plot))
               (if (error-bars-of plot)
                   (if (plot-style-of plot)
                       (cond ((eql :lines (plot-style-of plot))
                              (space-pad "with errorlines"))
                             ((eql :points (plot-style-of plot))
                              (space-pad "with errorbars")))
                       (space-pad "with errorbars"))
                   (if (plot-style-of plot)
                       (space-pad (mkstr "with lines linestyle " *style*))))))))
      (apply
       #'mkstr
       (remove
        nil
        (list "'" (namestring file-name) "' "
              (if (rep-label-of plot)
                  (space-pad (mkstr "title '" (rep-label-of plot) "'"))
                  (space-pad "notitle"))
;;<<>>=
(defvar *ignore-errors* t)

;;<<>>=
(defmethod stringify-plot ((plot func-rep) setup file-name)
  (with-open-file (out file-name :direction :output :if-exists :supersede)
    (if (member (plot-type-of setup) '(:map :3d))
        (let ((x-range-vals (x-range-of setup))
              (y-range-vals (y-range-of setup)))
          (let ((x-range (- (second x-range-vals)
                            (first x-range-vals)))
                (y-range (- (second y-range-vals)
                            (first y-range-vals))))
            (iter (for x
                    from (first x-range-vals)
                    to (+ (* 1/2 x-range (/ (n-samples-of setup)))
                          (second x-range-vals))
                    by (/ x-range (n-samples-of setup)))
              (iter (for y
                      from (first y-range-vals)
                      to (+ (* 1/2 y-range (/ (n-samples-of setup)))
                            (second y-range-vals))
                      by (/ y-range (n-samples-of setup)))
                (let ((val (if *ignore-errors*
                               (ignore-errors (funcall (func-of plot) x y))
                               (funcall (func-of plot) x y))))
                  (if val
                      (format-ext out "~A ~A ~A~%" x y val)
                      (format-ext out "~%"))))
              (format-ext out "~%"))))
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
                             (format-ext out "~%"))))))))))
  (if (member (plot-type-of setup) '(:3d :map))
      (apply
       #'mkstr
       (remove
        nil
        (list "'" (namestring file-name) "' "
              (if (rep-label-of plot)
                  (space-pad (mkstr "title '" (rep-label-of plot) "'"))
                  (space-pad "notitle"))
              (if (smoothing-method-of plot)
                  (space-pad (mkstr "smooth " (keyword-to-string
                                               (smoothing-method-of plot)))))
              (space-pad (mkstr "with pm3d")))))
              ;; (if (error-bars-of plot)
              ;;     (if (plot-style-of plot)
              ;;         (cond ((eql :lines (plot-style-of plot))
              ;;                (space-pad "with errorlines"))
              ;;               ((eql :points (plot-style-of plot))
              ;;                (space-pad "with errorbars")))
              ;;         (space-pad "with errorbars"))
              ;;     (if (plot-style-of plot)
              ;;         (space-pad (mkstr "with " (plot-style-to-string
              ;;                                    (plot-style-of plot))
              ;;                           " linestyle " (incf *style*))))
              ;;     )
      (apply
       #'mkstr
       (remove
        nil
        (list "'" (namestring file-name) "' "
              (if (rep-label-of plot)
                  (space-pad (mkstr "title '" (rep-label-of plot) "'"))
                  (space-pad "notitle"))
              (if (smoothing-method-of plot)
                  (space-pad (mkstr "smooth " (keyword-to-string
                                               (smoothing-method-of plot)))))
              (if (error-bars-of plot)
                  (if (plot-style-of plot)
                      (cond ((eql :lines (plot-style-of plot))
                             (space-pad "with errorlines"))
                            ((eql :points (plot-style-of plot))
                             (space-pad "with errorbars")))
                      (space-pad "with errorbars"))
                  (if (plot-style-of plot)
                      (space-pad (mkstr "with " (plot-style-to-string
                                                 (plot-style-of plot))
                                        " linestyle " (incf *style*))))))))))

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
               (let ((plot-strings
                       (apply #'append (mapcar #'alexandria:ensure-list
                                               plot-strings))))
                 (ecase (plot-type-of setup)
                   ((:3D :map) (send-gnuplot "splot ~{~A~^, ~};" plot-strings))
                   ((:polar :2D) (send-gnuplot "plot ~{~A~^, ~};" plot-strings))))))
    (send-gnuplot "replot;")))

;;<<>>=
(defun plot (state &rest plots)
  (if *gnuplot-stream*
      (apply #'%plot state (mapcar 'infer-rep plots))
      (with-gnuplot
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
(defvar *default-term* "x11")

;;<<>>=
(defun set-default-term (term)
  (setf *default-term* term))

;;<<>>=
(defun save-plot (fname &key (term "svg") size)
  (let ((fname (ensure-namestring fname)))
    (send-gnuplot
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

