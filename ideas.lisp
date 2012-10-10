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

;; @\section{Structure of the Interface}

;; ZGnuplot plotting involves interaction with plotting functions.  Each
;; function takes a setup object and a list of {\em plottable objects} which it
;; uses to produce a plot.  A plot is a self contained object that can be sent
;; to Gnuplot in order to produce a plot.

;; The config object determines what kind of plot to make.

;; The function <<plot-array>> creates a set of plots that are arranged in a
;; grid.  This makes it simple to, for instance, give multiple viewpoints of the
;; same 3D plot.  Each of those subplots are plotted as if plot was used.  You
;; specify the plot by giving the argument list that will pass to plot.

;; The function <<plot-with-inset>> plots a graph with one inset (which once
;; again should be a well formed arg list for plot).

;; The user also specifies the function that will process the arglist.  This way
;; they plots are fully composable.

;; The fence plots are still troublesome.  They are a way to plot a series of
;; two dimensional plots it three dimensions.  Perhaps all of these dimension
;; changing setups are not well defined in this framework.  Should this be a
;; plot type?  I suppose it should be an option on the config object, but if it
;; is that, how do we have other plots that are not part of the fence show up in
;; the plot.  I am just going to leave this unanswered and unimplemented right
;; now.

;; (defun merge-configs (config1 config2)
;;   (when (and (plot-type-of config1)
;;              (plot-type-of config2)
;;              (not (eql (plot-type-of config1)
;;                        (plot-type-of config2))))
;;     (make-condition 'dimension-mismatch))
;;   (make-gnuplot-setup :plot-type (or (plot-type-of config1)
;;                                      (plot-type-of config2))))

;; (defmethod determine-config ((fn func))
;;   "A wrapped function"
;;   (let ((arity (multiple-value-list
;;                 (determine-arity (func-of fn)))))
;;     (cond ((= (first arity) 1)
;;            (make-gnuplot-setup
;;             :plot-type :2D))
;;           ((= (first arity) 2)
;;            (make-gnuplot-setup
;;             :plot-type :3D)))))

;; (defmethod determine-config ((data data-series))
;;   (cond ((= (ima:ima-dimensions (independendent-data-of data)) 1)
;;          (make-gnuplot-setup
;;           :plot-type :2D))
;;         ((= (ima:ima-dimensions (independendent-data-of data)) 2)
;;          (make-gnuplot-setup
;;           :plot-type :3D))))

;; (defmethod determine-config ((fn function))
;;   (sniff-function fn))

;; (defmethod determine-config ((fn symbol))
;;   (sniff-function (symbol-function fn)))

;; (defun sniff-function (fn)
;;   "A bare function"
;;   (let ((arity (multiple-value-list
;;                 (determine-arity fn))))
;;     (cond ((= (first arity) 1)
;;            (make-gnuplot-setup
;;             :plot-type :2D))
;;           ((= (first arity) 2)
;;            (make-gnuplot-setup
;;             :plot-type :3D)))))

;; (define-condition dimensions-mismatch ()
;;   ((plots)))

;; (defun infer-plot-config (config &rest args)
;;   (cond (args
;;          (let ((plottable (first args)))
;;            (apply
;;             'infer-plot-config
;;             (combine-sniffs (determine-config plottable) config)
;;             (rest args))))
;;         (t config)))

;; (defclass* map (rep)
;;   ((plot-style :points)
;;    (rep-label nil)
;;    (x-data nil)
;;    (y-data)
;;    (smoothing-method :cspline)))

;; (defun plot* (&rest plots)
;;   "This is a poorly named function that is going to be the new interface."
;;   (let ((config (apply #'infer-plot-config *gnuplot-config*
;;                        plots)))
;;     ()))

;; (defun plot-array (format &rest plots)
;;   ())

;; (defun plot-inset ()
;;   ())

;; (defun 

;; ;; Plot styles include points, lines, lines-and-points, smooth-lines-and-points,
;; ;; smooth-lines, and map, vector-field, and slope-field for 3D.

;; (defclass* func ()
;;   (func))

;; (defclass* data-series ()
;;   (data))

;; (defclass* data-tuples ()
;;   (data))

;; ;; Aggregaters

;; (defclass* multiplot ()
;;   (plots
;;    arrangement))

;; (defclass* slices ()
;;   (plots
;;    slice-normal))

;; (defclass* auto-slice ()
;;   (plot
;;    slice-normal
;;    start
;;    end
;;    step
;;    n-slices))


;; ;; The plot attempts to determine properties of the plot.  These could be set
;; ;; explicitly, or sniffed out, or set in the plot configure structure.  The
;; ;; precedence goes like this: default values in the config yield to default
;; ;; values in the plot, yield to set values in the config, which finally yields
;; ;; to set values in the plot.

;; ;; In the case of mixed dimensionalities, we do something but that something
;; ;; might be nonsensical.  If we have 3D and 2D data, we can often times get away
;; ;; with a 2D plot and make a series of plots for the 3D, but a 3D plot makes no
;; ;; sense for the 2D data.  4D data and above is almost certainly too much to
;; ;; include on a 2D plot.  4D and above has a hard enough time appearing in 3D.

;; ;; The <<smart-plot>> interface attempts to perform even more intelligent
;; ;; handling of a data set.  When it detects higher dimensions, it uses the multiplot interface to plot an array of plots 

;; ;; Use the 3D box object to plot 2D on the 2D axes planes in a 3D plot?  Use the
;; ;; up-dimension to add basically a fence at an arbitrary spot?

(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-bind ((style-warning #'muffle-warning))
    (eval '(defmacro with-plot-config ((&whole options &key plot-type)
                                       &body body)
            ;; (declare (ignorable plot-type))
            `(let ((*gnuplot-setup*
                     (make-gnuplot-setup ,@options)))
               (list
                ,@body))))))

;; ;; examples

;; ;; plot

;; ;; Simple
;; (plot
;;  (make-gnuplot-setup) 'sin)

;; ;; Multiple lines
;; (plot (make-gnuplot-setup)
;;       'sin
;;       'cos)

;; ;; 3D
;; (plot (make-gnuplot-setup :plot-type :3D)
;;       (lambda (x y) (+ x y)))

;; ;; Polar
;; (plot (make-gnuplot-setup :plot-type :polar)
;;       (constantly 1)
;;       'sin)

;; ;; Map
;; (plot (make-gnuplot-setup :plot-type :map)
;;       (lambda (x y) (+ (sin x) (cos y))))

;; ;; plot-array

;; (plot-array
;;  (list
;;   (list
;;    (list #'plot (make-gnuplot-setup) 'sin)
;;    (list #'plot (make-gnuplot-setup) 'cos)
;;    (list #'plot (make-gnuplot-setup) '((1 1) (1 1) (1 1) (1 1))))))

;; We need smarter dispatching.  Using CLOS would be fine, but we need to have a
;; distinct class type for each way of plotting that requires a special gnuplot
;; command.


 ;; arguments are configure objects or plottables.
 ;; #'sin
 ;; (make-instance 'fill-between
 ;;                :plot1 'sin
 ;;                :plot2 (make-instance 'func-rep :func 'cos))
 ;; (make-instance 'slices
 ;;                :plot (make-instance 'func-rep
 ;;                                     :func (lambda (x y) (* (sin x) (cos y))))
 ;;                :slice-normal :y
 ;;                :step-size .1 ; or :n-slices
 ;;                )
 ;; (lambda (x y) (* (sin x) (cos y)))
 ;; (make-instance 'map (lambda (x y) (* (sin x) (cos y))))
 ;; (lambda (x y) (list (sin x) (cos y)))
 ;; (make-instance 'map-3d (lambda (x y z) (* z (sin x) (cos y))))

;; \section{Dispatch on plot type}

;; @Part of what ZGnuplot gives to users is the ability to modularize and
;; organize code into code segments that make sense and correspond to the
;; service they seek to provide.

;; maybe something like this?

;; (defclass* plot-options ()
;;   ((x-range '(-10 10))
;;    (y-range '(-10 10))))

;; (deflass* frame-options ()
;;   ((x-axis)
;;    (y-axis)))

;; (defclass* plot-3d (plot-options)
;;   ((z-range '(-1 1))
;;    (cb-range '(-1 1))))

;; (defclass* plot-fences (plot-3d)
;;   ())

;; @\section{Nesting and Delaying Plots}

;; The <<with-suppressed-plotting>> macro informs the contained plot commands to
;; refrain from actually plotting the plottables and instead return a cons cell
;; containing a frame configuration object and a list of plottable objects.  If
;; plot is applied to this cons cell outside of a <<with-suppressed-plotting>>
;; macro environment, the desired plot will be produced.  This allows you to
;; defer the plotting of a graph, which allows you to nest plotting commands
;; without unnecessary intermediate plotting steps.

(defvar *suppressed-plotting* nil)

(defmacro with-suppressed-plotting (&body commands)
  "Process the specified plotting commands \(each of which should produce a
plot) but do not send the commands to Gnuplot, just return a list that PLOT can
be applied to create the plot."
  `(let ((*suppress-plotting* t))
     (list ,@commands)))

;; @The dispatching system finds the most specific match in all of the declared
;; dispatch functions and uses the one that matches your setup on the most
;; individual options (ties broken based on the order of the parameters in the
;; class, this might change because I'm not sure this is the best idea).

;; If you try to declare a dipatch that specializes on something that another
;; dispatch doesn't, all of the dispatch functions that don't specialize on that
;; parameter effectively match on anything parameter value that the new function
;; matches on.

;; Thus, if you say that something happens for all 3d plots, then you say
;; something that happens for all 3d plots that are also logarithmic plots, then
;; a linear, 3d plot will run the just 3d code, a 3d log plot will run the 3d
;; log code.

;; We develop our own dispatch system instead of using CLOS is because the
;; dispatch is based on combinations of parameters, not heirarchical as is the
;; case that CLOS excels at.  The sheer number of classes that would be needed
;; to fully specify every possibility is very large.  We also don't need the
;; fancy call chain stuff from CLOS, so we don't miss out on much.

(defvar *dispatch* nil)

(defmacro define-gnuplot-dispatch ((plot-sym file-sym process-sym)
                                   (&rest values) &body body)
  (with-gensyms (plot file process)
    (destructuring-bind (plot-sym plot-type)
        (if (listp plot-sym) plot-sym (list plot-sym t))
      `(push
        (lambda (,plot ,file ,process)
          (and
           (typep ,plot ',plot-type)
           ,@(iter (for value in values)
               (if (listp value)
                   (ecase (length value)
                     (3 (collecting
                         `(funcall ,(third value)
                                   (slot-value ,plot ',(first value))
                                   ,(second value))))
                     (2 (collecting
                         `(equal (slot-value ,plot ',(first value))
                                 ,(second value)))))
                   (collect `(slot-value ,plot ',value))))
           (let ((,plot-sym ,plot)
                 (,file-sym ,file)
                 (,process-sym ,process))
             ,@body)))
        *dispatch*))))

;; Stringify-Plot2 dispatches the plot commands to the appropriate handler.  The
;; handler, for now, must do two things.  First, it must return a string (or
;; list of strings) that, when passed to the proper gnuplot plot or splot
;; command, will plot the data in <file-name> correctly.  Secondly, if and only
;; if <process> is true, the function should write data (into file <file-name>)
;; that will be read from gnuplot.

(defun stringify-plot2 (plottable file-name &optional (process t))
  (iter (for fn in *dispatch*)
    (thereis (funcall fn plottable file-name process))))

(defun dump-data (plot file-name)
  (with-open-file (out file-name :direction :output :if-exists :supersede)
    (format-ext out "~{~{~A ~}~%~}"
                (apply
                 #'mapcar #'list
                 (remove nil
                         (list (x-data-of plot)
                               (y-data-of plot)
                               (error-bars-of plot)))))))

(define-gnuplot-dispatch ((plot data-rep) file-name process)
    ((plot-type :2d)
     (plot-style :smooth-lines-and-points))
  (when process (dump-data plot file-name))
  (list
   (stringify-plot2 (modf (plot-style-of plot) :smooth-lines)
                    file-name nil)
   ;; We need to decrement the style counter so that the color of the line and
   ;; the points are the same.
   (progn
     (decf *style*)
     (stringify-plot2 (modf (plot-style-of plot) :points)
                      file-name nil))))

(define-gnuplot-dispatch ((plot data-rep) file-name process)
    ((plot-type :2d)
     (plot-style '(:points :lines :lines-and-points) 'member))
  (when process (dump-data plot file-name))
  (apply
   #'mkstr
   (remove
    nil
    (list "'" (namestring file-name) "' "
          (if (rep-label-of plot)
              (space-pad (mkstr "title '" (rep-label-of plot) "'"))
              (space-pad "notitle"))
          (mkstr
           (if (error-bars-of plot)
               (if (plot-style-of plot)
                   (cond ((eql :lines (plot-style-of plot))
                          (space-pad "with errorlines"))
                         ((eql :points (plot-style-of plot))
                          (space-pad "with errorbars")))
                   (space-pad "with errorbars"))
               (if (plot-style-of plot)
                   (space-pad (mkstr "with " (plot-style-to-string
                                              (plot-style-of plot))))))
           " linestyle " (incf *style*))))))

(define-gnuplot-dispatch ((plot data-rep) file-name process)
    ((plot-type :2d)
     (plot-style :smooth-lines))
  (when process (dump-data plot file-name))
  (apply
   #'mkstr
   (remove
    nil
    (list "'" (namestring file-name) "' "
          (if (rep-label-of plot)
              (space-pad (mkstr "title '" (rep-label-of plot) "'"))
              (space-pad "notitle"))
          (space-pad (mkstr "smooth " (keyword-to-string
                                       (smoothing-method-of plot))))
          (mkstr
           (if (error-bars-of plot)
               (if (plot-style-of plot)
                   (cond ((eql :lines (plot-style-of plot))
                          (space-pad "with errorlines"))
                         ((eql :points (plot-style-of plot))
                          (space-pad "with errorbars")))
                   (space-pad "with errorbars"))
               (if (plot-style-of plot)
                   (space-pad (mkstr "with lines"))))
           " linestyle " (incf *style*))))))

(defun grid-sample (out fn coords ranges)
  (if ranges
      (destructuring-bind (lo hi n-samples) (first ranges)
        (let ((range (- hi lo)))
          (iter (for x
                  from lo
                  to (+ (* 1/2 range (/ n-samples)) hi)
                  by (/ range n-samples))
            (grid-sample out
                         (lambda (&rest args) (apply fn x args))
                         (cons x coords)
                         (rest ranges)))
          (format-ext out "~%")))
      (let ((val (if *ignore-errors*
                     (ignore-errors (multiple-value-list (funcall fn)))
                     (multiple-value-list (funcall fn)))))
        (if val
            (format-ext out "~{~A ~} ~{~A ~}~%" (reverse coords) val)
            (format-ext out "~%")))))

(define-gnuplot-dispatch ((plot func-rep) file process)
    ((plot-type '(:map :3d) 'member))
  (when process 
    (with-open-file (out file :direction :output :if-exists :supersede)
      (grid-sample out (func-of plot) nil
                   (if (listp (n-samples-of plot))
                       (mapcar (lambda (x y) (append x y))
                               (list (x-range-of plot)
                                     (y-range-of plot))
                               (n-samples-of plot))
                       (mapcar (lambda (x) (append x (list (n-samples-of plot))))
                               (list (x-range-of plot)
                                     (y-range-of plot)))))))
  (apply
   #'mkstr
   (remove
    nil
    (list "'" (namestring file) "' "
          (if (rep-label-of plot)
              (space-pad (mkstr "title '" (rep-label-of plot) "'"))
              (space-pad "notitle"))
          (if (smoothing-method-of plot)
              (space-pad (mkstr "smooth " (keyword-to-string
                                           (smoothing-method-of plot)))))
          (space-pad (mkstr "with pm3d"))))))

(define-gnuplot-dispatch ((plot func-rep) file-name process)
    ((plot-type '(:2d :polar) 'member)
     (error-bars nil))
  (when process
    (with-open-file (out file-name :direction :output :if-exists :supersede)
      (let ((range-vals (if (eql :polar (plot-type-of plot))
                            (theta-range-of plot)
                            (x-range-of plot))))
        (grid-sample
         out (func-of plot) nil
         (list (append range-vals (list (n-samples-of plot))))))))
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
          (if (plot-style-of plot)
              (space-pad (mkstr "with " (plot-style-to-string
                                         (plot-style-of plot))
                                " linestyle " (incf *style*))))))))

(define-gnuplot-dispatch ((plot func-rep) file-name process)
    ((plot-type '(:2d :polar) 'member)
     error-bars)
  (when process
    (with-open-file (out file-name :direction :output :if-exists :supersede)
      (let ((range-vals (if (eql :polar (plot-type-of plot))
                            (theta-range-of plot)
                            (x-range-of plot))))
        (grid-sample
         out (func-of plot) nil
         (list (append range-vals (list (n-samples-of plot))))))))
  (warn "No error lines yet.")
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
          ;; this is basically broken
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
                                    " linestyle " (incf *style*)))))))))

(defun render-equation (plot)
  (cond ((symbolp plot)
         (string-downcase (format nil "~A" plot)))
        (t nil)))

(define-gnuplot-dispatch ((plot (or function symbol)) file-name process)
    ()
  (let ((arity (determine-minimum-arity plot)))
    (stringify-plot2
     (make-instance 'func-rep
                    :plot-type (ecase arity
                                 (1 :2d)
                                 (2 :2d))
                    :func plot
                    :rep-label (render-equation plot)
                    :n-samples 100
                    :x-range '(-1 1)
                    :y-range '(-1 1))
     file-name
     process)))

(define-gnuplot-dispatch ((plot list) file-name process)
    ()
  (cond ((ima:ima-p (first plot))
         ;; If first object in the list is an ima, then process them,
         ;; effectively splicing these into the arg lists
         (iter (for el ima:in-ima plot)
           ())))
  (stringify-plot2
     (infer-rep plot)
     file-name
     process))

(defun plot2 (setup &rest plots)
  (let* ((*gnuplot-setup* (or setup *gnuplot-setup* (error "No gnuplot state set.")))
         (st *gnuplot-setup*)
         (*style* 0))
    (unless plots
      (error "No plots given"))
    (iter (for plot in plots)
      (for file-name =
        (pathname (osicat-posix:mktemp
                   (namestring osicat:*temporary-directory*))))
      (collecting (stringify-plot2 plot file-name)
                  into plot-strings)
      (finally (send-gnuplot (setup-gnuplot st))
               (let ((plot-strings
                       (apply #'append (mapcar #'alexandria:ensure-list
                                               plot-strings))))
                 (ecase (plot-type-of setup)
                   ((:3D :map) (send-gnuplot "splot ~{~A~^, ~};" plot-strings))
                   ((:polar :2D) (send-gnuplot "plot ~{~A~^, ~};" plot-strings))))))
    (send-gnuplot "replot;")))


(defun plot-array (config plots)
  "Plot several graphs \(each with their own config) arranged on a grid.  The
structure of the list structure, plots specifies how the plots will be arranged.
It is specified either as a row major 'table' of plots \(where a NIL can
designate a blank spot), or as a single list where each element of that list is
prefaced by two numbers which specify absolute offsets for that plot."
  ())

(defun plot-with-inset (config inset &rest plots)
  "Plot a single graph with an inset."
  ())

;; @\subsection{Polar Plots}

;; Polar plots are a special type of 2D plot and of parameteric plot.  As with
;; gnuplot,this requires a separate interface.  However unlike gnuplot, the
;; interface is keeping with the <<plot>> and <<splot>> split.  This makes for a
;; more uniform interface.

;; @\section{3D Plotting}

;; @\subsection{Plot Representations}

;; @\section{Plotting Surfaces}

;; @\section{Similar Libraries}

;; @\begin{itemize}

;; These need to be reviewed as I don't actually remember all of the names of
;; these libraries.

;; \item Cgn
;; \item Clnuplot
;; \item cl-graph

;; \end{itemize}
