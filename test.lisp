
(in-package :zgnuplot)

;; @In this section we specify test plots.  These tests often need visual
;; confirmation.

;; @\section{2D Plots}

;; @\subsection{Curves}

(plot (zgp:make-gnuplot-setup :title "Some curves" :x-range (list 0 (* 2 pi))
                              :autoscale t)
      'sin 'cos #'log (lambda (x) (abs (sin (- x (/ pi 4))))))

;; @\subsection{Data}

(plot (zgp:make-gnuplot-setup :autoscale '(:y :x)
                              :x-label "Some x label"
                              :y-label "Some y label")
      (mapcar (lambda (x) (+ (sin x) x)) '(0 1 2 3 4 5 6 7 8 9))
      (mapcar (lambda (x) (list x (+ (cos x) x))) '(0 1 2 3 4 5 6 7 8 9))
      '((10 0.0) (9 1.4546487) (8 1.6215987) (7 2.8602922) (6 4.494679) (5 4.727989)
        (4 5.731714) (3 7.4953036) (2 7.8560486) (1 8.624506)))

(plot (zgp:make-gnuplot-setup :autoscale '(:y :x)
                              :x-label "Some x label"
                              :y-label "Some y label")
      (let ((data (mapcar (lambda (x) (+ (sin x) x)) '(0 1 2 3 4 5 6 7 8 9))))
        (make-instance 'data-rep :plot-style "points" :y-data data))
      (let ((data (mapcar (lambda (x) (list x (+ (cos x) x))) '(0 1 2 3 4 5 6 7 8 9))))
        (make-instance 'data-rep :plot-style "lp"
                       :x-data (mapcar #'first data)
                       :y-data (mapcar #'second data)))
      (let ((data '((10 0.0) (9 1.4546487) (8 1.6215987) (7 2.8602922)
                    (6 4.494679) (5 4.727989) (4 5.731714) (3 7.4953036)
                    (2 7.8560486) (1 8.624506))))
        (make-instance 'data-rep :plot-style "lines"
                                 :x-data (mapcar #'first data)
                                 :y-data (mapcar #'second data))))

(plot (zgp:make-gnuplot-setup :autoscale '(:y)
                              :n-samples 5
                              :x-range '(0 10)
                              :x-label "Some x label"
                              :y-label "Some y label")
      (let ((data (mapcar (lambda (x) (list x (+ (cos x) x))) '(0 1 2 3 4 5 6 7 8 9))))
        (make-instance 'data-rep :plot-style :smooth-lines-and-points
                                 :smoothing-method :csplines
                                 :x-data (mapcar #'first data)
                                 :y-data (mapcar #'second data)))
      (make-instance 'func-rep
                     :smoothing-method :csplines
                     :func (lambda (x) (+ (sin x) x)))
      (make-instance 'func-rep
                     ;; :smoothing-method :csplines
                     :func (lambda (x) (+ (sin x) x))))

;; @\subsection{Error Bars}
(plot (zgp:make-gnuplot-setup :autoscale '(:y :x)
                              :x-label "Some x label"
                              :y-label "Some y label")
      (mapcar (lambda (x) (list x (+ (cos x) x) .1)) '(0 1 2 3 4 5 6 7 8 9)))

;; @\subsection{Polar Plots}

(plot
 (zgp:make-gnuplot-setup :title "A polar plot"
                         :plot-type :polar :key t :grid t :key-inset nil
                         :y-range '(0 1.1) :theta-range (list 0 pi))
 (lambda (theta) (expt (sin theta) 1/16))
 (lambda (theta) (expt (sin theta) 1/4))
 (lambda (theta) (expt (sin theta) 1/2))
 'sin
 (lambda (theta) (expt (sin theta) 2))
 (lambda (theta) (expt (sin theta) 4)))

(plot
 (zgp:make-gnuplot-setup :title "A polar plot"
                         :plot-type :polar :key t :grid t :key-inset nil
                         :y-range '(0 1.1) :theta-range (list 0 pi))
 (make-instance 'func-rep
                :func (lambda (theta) (expt (sin theta) 1/16))
                :rep-label (tb:format-ext nil "(sin(x))^(1/16)"))
 (make-instance 'func-rep
                :func (lambda (theta) (expt (sin theta) 1/4))
                :rep-label (tb:format-ext nil "(sin(x))^(1/4)"))
 (make-instance 'func-rep
                :func (lambda (theta) (expt (sin theta) 1/2))
                :rep-label (tb:format-ext nil "sqrt(sin(x))"))
 (make-instance 'func-rep
                :func (lambda (theta) (sin theta))
                :rep-label (tb:format-ext nil "sin(x)"))
 (make-instance 'func-rep
                :func (lambda (theta) (expt (sin theta) 2))
                :rep-label (tb:format-ext nil "sin²(x)"))
 (make-instance 'func-rep
                :func (lambda (theta) (expt (sin theta) 4))
                :rep-label (tb:format-ext nil "sin⁴(x)")))

;; @\subsection{Maps}

;; @\section{3D Plots}

;; @\subsection{Surface Plots}
