
(asdf:defsystem :zgnuplot
  :author "Zach Smith"
  :license "GPLv3 or later. See file COPYING"
  :depends-on (:cffi :toolbox :defclass-star
                     :iterate :alexandria
                     :osicat)
  :components ((:file "package")
               (:file "utils")
               (:file "plumbing")
               (:file "color-schemes")
               (:file "zgnuplot"))
  :serial t)

;; @\section{Overview}

;; @The class <<gnuplot-setup>> is the structure that allows you to control how
;; gnuplot performs the plot.

;; @The function <<plot>> performs all plotting.  It takes an object of type
;; <<gnuplot-setup>> as its first argument and any number of plottable objects
;; after it.  If the object passed as the first argument is <<nil>> then the
;; function tries to come up with something reasonable to do.

;; @Plotting is done by specifying a plot setup and a series of plots.  The plot
;; setup controls the appearance of the graph in most aspects (certain visual
;; aspects are controlled by the plots such as if plots are drawn as lines or
;; points).  The visual aspects that are controlled by the setup include 2D
;; versus 3D plots, standard versus polar or parametric plots, and the position
;; of the key.

;; @\subsection{The domain of the plot setup}

;; As gnuplot is a very capable plotting program, it also has a hodge-podge of
;; commands and settings in order to specify how gnuplot should perform the
;; plot.  These settings are all contained within the <<gnuplot-setup>> object.

;; @\subsection{The domain of the plots themselves}

;; Plots are often inherently two dimensional even when performing a three
;; dimensional plot.  This is remedied by hueristic plotting algorithms.

;; +@utils.lisp
;; +@plumbing.lisp
;; +@zgnuplot.lisp

;; @\section{Acknowledgements}

;; This library was coded primarily by myself, however I am not a Gnuplot
;; expert, so several individuals helped (although throuugh blog posts, not in
;; person).

;; \begin{itemize}

;; \item The maintainer of the blog Gnuplotting: http://www.gnuplotting.org/

;; \item A person under the psuedonym Gnuplotter:
;; http://gnuplot-tricks.blogspot.com/

;; \item And of course, the Gnuplot team's demo page:
;; http://gnuplot.sourceforge.net/demo_4.6/

;; \end{itemize}

;; \end{document}
