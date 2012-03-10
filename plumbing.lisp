
(in-package :zgnuplot)

;; @\section{The plumbing}

;; @In this section we describe the tools necessary to get gnuplot running and
;; connected to our library.  The basic tasks we need are to start and stop a
;; shell process (<<start-gnuplot>> which uses <<do-execute>> and
;; <<close-gnuplot>> which uses <<close-process-stream>>, where the real code is
;; taken from the LTK v0.96 library) and sending commands to gnuplot
;; (<<send-gnuplot>>).  Any output from the gnuplot process is interpreted as an
;; error and is reported as such.

;;<<>>=
(defvar *gnuplot-stream* nil)

;;<<>>=
(defun start-gnuplot (&optional (gnuplot-executable "gnuplot"))
  (setf *gnuplot-stream* (do-execute gnuplot-executable '("-persist")))
  (let* (char
         (err
           (iter (while (setf char (read-char-no-hang *gnuplot-stream*)))
             (collecting char))))
    (when err
      (print (coerce err 'string)))
    (setf *default-term* (subseq (zgp::send-gnuplot-raw "show term") 17))
    (values)))

;;<<>>=
(defun do-execute (program args &optional (wt nil))
  "Execute program with args a list containing the arguments passed to the program
if wt is non-nil, the function will wait for the execution of the program to return.
returns a two way stream connected to stdin/stdout of the program.

Taken from LTK."
  #+(or :clisp :lispworks) (declare (ignore wt))
  (let ((fullstring program))
    (dolist (a args)
      (setf fullstring (concatenate 'string fullstring " " a)))
    #+(or :cmu :scl)
    (let ((proc (run-program program args :input :stream :output :stream :wait wt
                             #+scl :external-format #+scl :utf-8)))
      (unless proc
        (error "Cannot create process."))
      (make-two-way-stream
       (ext:process-output proc)
       (ext:process-input proc))
      )
    #+:clisp
    (let ((proc (ext:run-program program :arguments args :input :stream :output :stream :wait t)))
      (unless proc
        (error "Cannot create process."))
      proc)
    #+:sbcl
    (let ((proc (sb-ext:run-program program args :input :stream :output :stream :wait wt :search t)))
      (unless proc
        (error "Cannot create process."))
      #+:ext-8859-1
      (make-two-way-stream 
       (sb-sys:make-fd-stream 
        (sb-sys:fd-stream-fd (process-output proc))
        :input t :external-format :iso-8859-1)
       (sb-sys:make-fd-stream 
        (sb-sys:fd-stream-fd (process-input proc))
        :output t  :external-format :iso-8859-1))
      (make-two-way-stream 
       (sb-sys:make-fd-stream 
        (sb-sys:fd-stream-fd (sb-sys::process-output proc))
        :input t :external-format :utf-8)
       (sb-sys:make-fd-stream 
        (sb-sys:fd-stream-fd (sb-sys::process-input proc))
        :output t  :external-format :utf-8))
      #+:xxext-8859-1
      (make-two-way-stream 
       (process-output proc)              
       (process-input proc)))
    #+:lispworks
    (system:open-pipe fullstring :direction :io)
    #+:allegro
    (let ((proc (excl:run-shell-command
                 #+:mswindows fullstring
                 #-:mswindows (apply #'vector program program args)
                 :input :stream :output :stream :wait wt)))
      (unless proc
        (error "Cannot create process."))
      proc)
    #+:ecl
    (ext:run-program program args :input :stream :output :stream
                                  :error :output)
    #+:openmcl
    (let ((proc (ccl:run-program program args :input
                                 :stream :output :stream :wait wt)))
      (unless proc
        (error "Cannot create process."))
      (make-two-way-stream
       (ccl:external-process-output-stream proc)
       (ccl:external-process-input-stream proc)))))

;;; CMUCL, SCL, and SBCL, use a two-way-stream and the constituent
;;; streams need to be closed.
;;<<>>=
(defun close-process-stream (stream)
  "Close a 'stream open by 'do-execute."
  (ignore-errors (close stream))
  #+(or :cmu :scl :sbcl)
  (when (typep stream 'two-way-stream)
    (close (two-way-stream-input-stream stream) :abort t)
    (close (two-way-stream-output-stream stream) :abort t))
  nil)

;;<<>>=
(defun close-gnuplot ()
  (close-process-stream *gnuplot-stream*))

(defparameter *debug* nil)

;;<<>>=
(defun send-gnuplot (control-string &rest args)
  "Send a string to the running gnuplot process."
  ;; Flush any pending output
  (iter (while (read-char-no-hang *gnuplot-stream*)))
  (let ((command (apply #'format-ext nil control-string args)))
    ;; Send the command
    (if *debug* (print command))
    (format *gnuplot-stream* "~A~%" command)
    ;; An extra command to ensure that the plotting is complete before moving on
    (format-ext *gnuplot-stream* "!echo !~%")
    (finish-output *gnuplot-stream*)
    (let* ((ret (coerce
                 (iter (for char = (read-char *gnuplot-stream*))
                   (until (eql char #\!))
                   (collecting char))
                 'string)))
      (unless (equal (string-trim '(#\Space #\Newline #\Tab) ret) "")
        (error ret)))))

;;<<>>=
(defun send-gnuplot-raw (control-string &rest args)
  "Send a string to the running gnuplot process."
  ;; Flush any pending output
  (iter (while (read-char-no-hang *gnuplot-stream*)))
  (let ((command (apply #'format-ext nil control-string args)))
    ;; Send the command
    (if *debug* (print command))
    (format *gnuplot-stream* "~A~%" command)
    ;; An extra command to ensure that the plotting is complete before moving on
    (format-ext *gnuplot-stream* "!echo !~%")
    (finish-output *gnuplot-stream*)
    (let* ((ret (coerce
                 (iter (for char = (read-char *gnuplot-stream*))
                   (until (eql char #\!))
                   (collecting char))
                 'string)))
      (string-trim '(#\Space #\Newline #\Tab) ret))))

;;<<>>=
(defmacro with-gnuplot (&body body)
  `(let ()
     (start-gnuplot)
     (unwind-protect
          (progn ,@body)
       (close-gnuplot))))
